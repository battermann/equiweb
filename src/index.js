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
  inputc.value = x.text;
  inputc.focus();
  inputc.select();
  try {
    document.execCommand('copy');
    inputc.parentNode.removeChild(inputc);
    app.ports.notifyCopyToClipboard.send(x.index);
  }
  catch (err) {
    console.log('Oops, unable to copy');
  }
});
