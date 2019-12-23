import React from 'react';
import ReactDOM from 'react-dom';
import App from './src/app';

setTimeout(function() {
    ReactDOM.render(App(), document.getElementById('root'));
  }, 500);