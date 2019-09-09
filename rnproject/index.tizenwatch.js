import React from 'react';
import ReactDOM from 'react-dom';
import App from './src/app';
import { Api } from 'renative';
import { TIZEN_WATCH, FORM_FACTOR_WATCH, registerServiceWorker } from 'renative';

Api.platform = TIZEN_WATCH;
Api.formFactor = FORM_FACTOR_WATCH;

setTimeout(function() {
    ReactDOM.render(App(), document.getElementById('root'));
  }, 500);