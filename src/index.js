import 'bootstrap/dist/css/bootstrap.min.css'
window.bootstrap = require('bootstrap/dist/js/bootstrap.bundle.min.js')
// import 'bootswatch/dist/darkly/bootstrap.min.css'
import './main.css'
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

app.ports.copyToClipboard.subscribe(function (x) {
  var inputc = document.body.appendChild(document.createElement("input"));
  inputc.value = x[1];
  inputc.focus();
  inputc.select();
  document.execCommand('copy');
  inputc.parentNode.removeChild(inputc);
  app.ports.notifyCopyToClipboard.send(x[0]);
});

app.ports.ready.subscribe(_ => {
  requestAnimationFrame(_ => {
    var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
    tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl, {
        trigger: 'hover'
      })
    });
  })
});
