import 'bootstrap/dist/css/bootstrap.min.css'
import './main.css'
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const SIMULATION_API_BASE_URL = process.env.ELM_APP_SIMULATION_API_BASE_URL // E.g. "https://safe-shore-53897.herokuapp.com"

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: SIMULATION_API_BASE_URL
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

app.ports.copyToClipboard.subscribe(function (x) {
  var textArea = document.body.appendChild(document.createElement("TEXTAREA"));
  textArea.innerHTML = x.text;
  textArea.select();
  try {
    document.execCommand('copy');
    textArea.parentNode.removeChild(textArea);
    app.ports.notifyCopyToClipboard.send({ "index": x.index, "sharingType": x.sharingType });
  }
  catch (err) {
    console.log('Oops, unable to copy');
  }
});
