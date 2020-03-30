const WebSocket = require('ws');

const wss = new WebSocket.Server({
    port : 8080
});

var ids = {}

function random_index(size) {
	return Math.floor(Math.random() * size);
}

function generateID() {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	function get_id() {
		return letters[random_index(letters.length)]
			   + letters[random_index(letters.length)]
			   + letters[random_index(letters.length)]
			   + letters[random_index(letters.length)];
	}

	let id = get_id();
	while (id in ids ) {
		id = get_id();
	}

	return id;
}

console.log(generateID());
console.log(generateID());
console.log(generateID());
console.log(generateID());

wss.on('connection', function connection(ws) {
  console.log("Got connection");
  const id = generateID();	
  ids[id] = {
	'socket': ws,
	'id' : id,
  };

  ws.on('close', (event) => {
	  delete ids[id];
  });

  ws.on('message', (message) => {
	try {
		console.log('received: %s', message);
		var msg = JSON.parse(message);
		if (msg.host) {
			ids[id].host_signal = msg.signal;
		} else if (msg.connect) {
			ws.send(JSON.stringify({
				'signal_data': ids[msg.connect].host_signal
			}));
		} else if (msg.answer) {
			ids[msg.id].socket.send(JSON.stringify({
				'answer': msg.answer
			}));
		}
	} catch (err) { 
		console.log(err)
	}
  });

  ws.send(JSON.stringify(
	  { 'id': id }
  ));
});

console.log("waiting...");

