const express = require('express');
const cookieParser = require('cookie-parser');
const axios = require('axios');
const fs = require('fs');
const path = require('path');

// load configuration
const configPath = '/etc/scrapeable/config/cognito.json';
const config = JSON.parse(fs.readFileSync(configPath, 'utf-8'));

// set environment variables from config
Object.entries(config).forEach(([key, value]) => {
  process.env[key] = value;
});

const app = express();
app.use(cookieParser());

// auth code grant redirect
app.get('/callback', async (req, res) => {
    const { code } = req.query; // Get the authorization code from the redirect
    if (!code) {
        console.log('no auth code');
        return res.status(400).send('Authorization code is missing');
    }

    try {
        // exchange the authorization code for tokens
        const response = await axios.post(
            process.env.COGNITO_TOKEN_URL,
            new URLSearchParams({
                grant_type: 'authorization_code',
                client_id: process.env.COGNITO_CLIENT_ID,
                client_secret: process.env.COGNITO_CLIENT_SECRET,
                code,
                redirect_uri: 'https://reprex.org/auth/callback',
                scope: 'openid aws.cognito.signin.user.admin',
            }),
            { headers: { 'Content-Type': 'application/x-www-form-urlencoded' } }
        );

        const { access_token, refresh_token } = response.data;

        if (!access_token) {
            console.warn('access token is missing');
        }
        if (!refresh_token) {
            console.warn('refresh token is missing');
        }

        if(access_token) {
            res.cookie('access_token', access_token, {
                httpOnly: true,
                secure: true,
                sameSite: 'Strict',
                maxAge: 3600 * 1000, // 1 hour
            });
            res.cookie('is_logged_in', 'true', {
                httpOnly: false,
                secure: true,
                sameSite: 'Strict',
                maxAge: 3600 * 1000, // 1 hour
            });
        }
        if(refresh_token) {
            res.cookie('refresh_token', refresh_token, {
                httpOnly: true,
                secure: true, // Use true in production
                sameSite: 'Strict',
                maxAge: 5 * 24 * 3600 * 1000, // 7 days
            });
        }

        // redirect to reprex ide
        res.redirect('https://reprex.org');
    } catch (error) {
        console.error('error exchanging code for tokens:', error.response?.data || error.message);
        res.status(500).send('Authentication failed');
    }
});

// logout - remove all cookies
app.get('/logout', async (req, res) => {
    // clear cookies
    res.clearCookie('access_token');
    res.clearCookie('refresh_token');
    res.clearCookie('is_logged_in'); 
    res.redirect('https://reprex-org.auth.us-east-2.amazoncognito.com/login?client_id=4agp589aqu7nu1fdog4pav4gpg&redirect_uri=https://reprex.org/auth/callback&response_type=code&scope=openid');
});

// refresh access token if refresh token available
app.get('/refresh', async (req, res) => {
    const refreshToken = req.cookies.refresh_token;

    if(!refreshToken){
        return res.status(400).send('Nothing to refresh');
    }

    try {
        // attempt to get a new access token using the refresh token
        const response = await axios.post(
            process.env.COGNITO_TOKEN_URL,
            new URLSearchParams({
                grant_type: 'refresh_token',
                client_id: process.env.COGNITO_CLIENT_ID,
                client_secret: process.env.COGNITO_CLIENT_SECRET,
                refresh_token: refreshToken
            }),
            { headers: { 'Content-Type': 'application/x-www-form-urlencoded' } }
        );

        const { access_token } = response.data;

        // set new access token cookie
        res.cookie('access_token', access_token, {
            httpOnly: true,
            secure: true,
            sameSite: 'Strict',
            maxAge: 3600 * 1000, // 1 hour
        });
        res.cookie('is_logged_in', 'true', {
            httpOnly: false,
            secure: true,
            sameSite: 'Strict',
            maxAge: 3600 * 1000, // 1 hour
        });

        return res.status(200).send('Refreshed');
    } catch (error) {
        // refresh token is invalid or expired
        res.clearCookie('access_token');
        res.clearCookie('refresh_token');
        res.clearCookie('is_logged_in'); 
        return res.status(400).send('Nothing to refresh');
    }
});

// validate access token internally
app.get('/validate', async (req, res) => {
    try {
        console.log('=== Debug Info ===');
        console.log('COGNITO_DOMAIN:', process.env.COGNITO_DOMAIN);
        console.log('Headers:', req.headers);
        
        const cookieHeader = req.headers.cookie;
        console.log('Cookie header:', cookieHeader);
        
        if (!cookieHeader) {
            throw new Error('No cookie header provided');
        }

        // Parse cookies and get token
        const cookies = cookieHeader.split(';').reduce((acc, cookie) => {
            const [key, value] = cookie.trim().split('=');
            acc[key] = value;
            return acc;
        }, {});
        
        console.log('Parsed cookies:', cookies);
        const token = cookies.access_token;
        
        if (!token) {
            throw new Error('No access token found in cookies');
        }

        console.log('Token found, attempting Cognito request');
        console.log('Request URL:', `${process.env.COGNITO_DOMAIN}/oauth2/userInfo`);
        
        const response = await axios({
            method: 'get',
            url: `${process.env.COGNITO_DOMAIN}/oauth2/userInfo`,
            headers: {
                'Authorization': `Bearer ${token}`
            },
            timeout: 5000
        });
        
        const userId = response.data.sub;
        console.log('Successfully validated user:', userId);
        
        res.setHeader('X-User-Id', userId);
        return res.status(200).json({
            valid: true,
            userId: userId,
            user: response.data
        });
        
    } catch (error) {
        console.error('Full error:', {
            message: error.message,
            stack: error.stack,
            response: error.response?.data,
            config: error.config
        });
        
        if (error.response?.status === 401) {
            return res.status(401).json({
                valid: false,
                error: 'Invalid or expired token'
            });
        }
        
        return res.status(500).json({
            valid: false,
            error: error.message || 'Error validating token'
        });
    }
});

// Start the server
app.listen(3001, () => {
    console.log('auth service is running on 3001');
});