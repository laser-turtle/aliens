const WebSocket = require('ws');

const wss = new WebSocket.Server({
    port : 8080
});

function random_index(size) {
	return Math.floor(Math.random() * size);
}

var lobbies = {}

function generateID() {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	function get_id() {
		return letters[random_index(letters.length)]
			   + letters[random_index(letters.length)]
			   + letters[random_index(letters.length)]
			   + letters[random_index(letters.length)];
	}

	let id = get_id();
	while (id in lobbies) {
		id = get_id();
	}

	return id;
}

function is_lobby_full(game_id) {
	let lobby = lobbies[game_id].lobby_info;
	let full = lobby.max_players === lobby.players.length;
	console.log("Lobby is full?", full);
	return full;
}

function get_lobby_info(game_id) {
	return JSON.stringify({
		type: 'lobby-update',
		data: lobbies[game_id].lobby_info,
	})
}

function send_to_lobby_exclude_host(game_id, msg) {
	var sockets = lobbies[game_id].sockets;
	for (var i = 1; i < sockets.length; ++i)
	{
		if (sockets[i].alive) {
			sockets[i].ws.send(msg);
		}
	}
}

function update_lobby(game_id) {
	let json = get_lobby_info(game_id);
	var sockets = lobbies[game_id].sockets;
	for (var i = 0; i < sockets.length; ++i)
	{
		if (sockets[i].alive) {
			sockets[i].ws.send(json);
		}
	}
}

function kill_socket(game_id, socket_id) {
	lobbies[game_id].sockets[socket_id].alive = false;
}

function close_all_sockets(game_id) {
	let socks = lobbies[game_id].sockets;
	for (var i = 0; i < socks.length; ++i) {
		socks[i].alive = false;
		socks[i].ws.close();
	}
}

wss.on('connection', function connection(ws) {
  console.log("Got connection");

  var game_id = null;
  var socket_id = null;

  ws.on('close', (event) => {
	  if (socket_id == 0) {
	  	delete lobbies[game_id];
	  } else {
		  kill_socket(game_id, socket_id);
		  lobbies[game_id].sockets[0].ws.send(JSON.stringify({
			  type: 'player-dropped',
			  id: socket_id,
		  }));
	  }
  });

  ws.on('message', (message) => {
	  let msg = JSON.parse(message);
	  switch (msg.type) {
		  case 'new-lobby':
			  game_id = generateID();	
			  lobbies[game_id] = {
			    sockets: [{alive:true, ws}],
				lobby_info: {
					id : game_id,
					players : [msg.name],
					max_players : msg.max_players,
				},
			  };
			  socket_id = 0;
			  console.log(lobbies);
			  update_lobby(game_id);
			break;

	      case 'game-update':
			  send_to_lobby_exclude_host(msg.game_id, msg.data);
			  break;

		  case 'player-update':
			  msg.player_id = socket_id;
			  lobbies[msg.game_id].sockets[0].send(msg.data);
			  break;

		  case 'join-lobby':
			  console.log("join-lobby");
			  console.log(msg);
			  console.log(lobbies);
			  if (msg.game_id in lobbies) {
			  	console.log("game exists");
				  if (is_lobby_full(msg.game_id))
				  {
					  console.log("Lobby is full, closing");
					  ws.send(JSON.stringify({
						  type: 'lobby-full'
					  }));
					  ws.close();
				  } else {
					  info = lobbies[msg.game_id];
					  game_id = msg.game_id;
					  socket_id = info.sockets.length;
					  info.lobby_info.players.push(msg.name);
					  info.sockets.push({alive: true, ws:ws});
					  update_lobby(msg.game_id);
				  }
			  } else {
				  ws.send(JSON.stringify({
					  type: 'no-such-lobby',
				  }));
				  ws.close();
			  }
			  break;
	  }
  });
});

console.log("waiting...");

