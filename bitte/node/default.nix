# State directory: /var/lib/cardano-node
# Port: 3001
# Socket: /run/cardano-node/node.socket
{ writeShellScriptBin, cardano-node, coreutils, lib }:
let
  config-dir = ./config;

  db = "/var/lib/cardano-node/db";

  socket-path = "/run/cardano-node/node.socket";
in
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-node ]}"

  PORT=3001

  mkdir -p ${dirOf socket-path}

  cardano-node run --topology ${config-dir}/topology.yaml --database-path ${db} --socket-path ${socket-path} --config ${config-dir}/config.json  --port "$PORT"
''
