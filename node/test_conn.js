const WebSocket = require('ws');

const _ws = new WebSocket('wss://mc.laserturtle.net:62126');
const __ws = new WebSocket('wss://71.212.3.110:62126');
const ws = new WebSocket('wss://192.168.1.204:62126');

ws.on('open', function open() {
      ws.send('something');
});

ws.on('message', function incoming(data) {
      console.log(data);
});
