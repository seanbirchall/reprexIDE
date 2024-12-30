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

    // redirect to cognito logout
    const logoutUrl = `${process.env.COGNITO_DOMAIN}/logout?` +
        `client_id=${process.env.COGNITO_CLIENT_ID}&` +
        `logout_uri=${encodeURIComponent('https://reprex.org')}`;
    
    res.redirect(logoutUrl);
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
app.post('/validate', async (req, res) => {
    const token = req.cookies.access_token;
    
    if (!token) {
        return res.status(400).json({ valid: false, error: 'No token provided' });
    }
    
    try {
        const response = await axios.get(
            `${process.env.COGNITO_DOMAIN}/oauth2/userInfo`,
            {
                headers: {
                    'Authorization': `Bearer ${token}`
                }
            }
        );
        
        // The 'sub' claim is the unique identifier we want
        const userId = response.data.sub;
        
        return res.status(200).json({
            valid: true,
            userId: userId,  // Include this in the response
            user: response.data
        });
        
    } catch (error) {
        if (error.response?.status === 401) {
            return res.status(401).json({
                valid: false,
                error: 'Invalid or expired token'
            });
        }
        
        return res.status(500).json({
            valid: false,
            error: 'Error validating token'
        });
    }
});

// Start the server
app.listen(3001, () => {
    console.log('auth service is running on 3001');
});