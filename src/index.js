const Elm = require('./elm/Main.elm');

const mountNode = document.getElementById('main');
const app = Elm.Main.embed(mountNode);
