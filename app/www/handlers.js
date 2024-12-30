// put code to digitial ocean
Shiny.addCustomMessageHandler("put_code", function(message) {
  const { inputId, payload } = message;
  fetch('https://reprex.org/put/code/', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(payload)  // payload already contains code and id
  })
  .then(response => {
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return response.json();
  })
  .then(data => {
    Shiny.setInputValue(inputId, {
      status: 'success',
      timestamp: Date.now(),
      data: data
    });
  })
  .catch(error => {
    Shiny.setInputValue(inputId, {
      status: 'error',
      timestamp: Date.now(),
      error: error.message
    });
  });
});

// check for refresh token
Shiny.addCustomMessageHandler('check_refresh_token', function(message) {
  const { inputId } = message;
  fetch('https://reprex.org/auth/refresh', {
    method: 'GET',
    credentials: 'include'
  })
  .then(response => {
    if (response.status === 200) {
      Shiny.setInputValue(inputId, {
        status: 'success',
        timestamp: Date.now(),
        data: response
      })
    } else {
      Shiny.setInputValue(inputId, {
        status: 'error',
        timestamp: Date.now(),
        data: response
      })
    }
  })
  .catch(error => {
    Shiny.setInputValue(inputId, {
        status: 'error',
        timestamp: Date.now(),
        data: error
      })
  });
});

// query duckdb directly with sql - subhandler
Shiny.addCustomMessageHandler("duckdb_sql", function(message) {
  executeSql(message.sql, message.id);
});

// query duckdb using r - subhandler
Shiny.addCustomMessageHandler("duckdb_r", function(message) {
  runQuery(message.query, message.url, message.id);
});

// view object using df_viewer
Shiny.addCustomMessageHandler("view", function(message) {
  Shiny.setInputValue('view', {object: message.obj, id: Date.now()})
});
