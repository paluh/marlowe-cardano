# Port: 9083
# Socket: /run/cardano-node/node.socket
# State directory: /var/lib/plutus-chain-index
# Network ID: 1564
{ writeShellScriptBin, plutus-chain-index, coreutils, lib }:
let
  socket-path = "/run/cardano-node/node.socket";

  db = "/var/lib/plutus-chain-index/db.sqlite";
in
writeShellScriptBin "chain-index-entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils plutus-chain-index ]}"

  PORT=9083
  NETWORK_ID=1564

  mkdir -p ${dirOf db}

  plutus-chain-index start-index --socket-path ${socket-path} --db-path ${db} --port "$PORT" --network-id "$NETWORK_ID"
''
