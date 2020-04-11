const WebSocket = require('ws');
const fs = require('fs');
const https = require('https');

const server = https.createServer({
    cert: fs.readFileSync('/home/minecraft/certs/fullchain.pem'),
    key: fs.readFileSync('/home/minecraft/certs/privkey.pem'),
});

const wss = new WebSocket.Server({ server });

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

function get_lobby_info(game_id, player_id) {
    return JSON.stringify({
        type: 'lobby-update',
        player_id: player_id,
        data: lobbies[game_id].lobby_info,
    })
}

function send_to_lobby_exclude_list(game_id, data, excludes) {
    var sockets = lobbies[game_id].sockets;
    for (var i = 1; i < sockets.length; ++i)
    {
        if (!excludes.includes(i) && sockets[i].alive) {
            console.log('Sending to', i);
            sockets[i].ws.send(data);
        }
    }
}

function update_lobby(game_id) {
    var sockets = lobbies[game_id].sockets;
    for (var i = 0; i < sockets.length; ++i)
    {
        let json = get_lobby_info(game_id, i);
        if (sockets[i].alive) {
            console.log('Sending to', i);
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
      } else if (game_id in lobbies) {
          kill_socket(game_id, socket_id);
          lobbies[game_id].sockets[0].ws.send(JSON.stringify({
              type: 'player-dropped',
              id: socket_id,
          }));
      }
  });

  ws.on('message', (message) => {
      let msg = JSON.parse(message);
      console.log("MSG from", socket_id, "for", game_id, msg);
      switch (msg.type) {
          case 'new-lobby':
              game_id = generateID();   
              lobbies[game_id] = {
                sockets: [{alive:true, ws}],
                lobby_info: {
                    id : game_id,
                    players : [msg.name.substring(0, 24)],
                    max_players : msg.max_players,
                    latest_seq_id : 0,
                },
              };
              socket_id = 0;
              console.log(lobbies);
              update_lobby(game_id);
            break;

          case 'game-update':
          console.log("GAME UPDATE START " + msg.data.seq_id);
              let data = {
                  type: 'game-update',
                  data: msg.data,
              };
              let lobby = lobbies[game_id];
              if (lobby.lobby_info.latest_seq_id <= msg.data.seq_id)
              {
                  lobby.lobby_info.latest_seq_id = msg.data.seq_id;
                  send_to_lobby_exclude_list(game_id, JSON.stringify(data), msg.exclude);
              }
          console.log("GAME UPDATE END " + msg.data.seq_id);
              break;

          case 'player-update':
              msg.player_id = socket_id;
              let json_result = JSON.stringify(msg);
              lobbies[game_id].sockets[0].ws.send(json_result);
              break;

          case 'join-lobby':
              console.log("join-lobby");
              console.log(msg);
              console.log(lobbies);
              msg.game_id = msg.game_id.toUpperCase();
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

server.listen(62126);

