# Port: 8090
# Socket: /run/cardano-node/node.socket
{ writeShellScriptBin, cardano-wallet, coreutils, lib }:
let
  node-config-dir = ./node/config;

  socket-path = "/run/cardano-node/node.socket";
in
writeShellScriptBin "wbe-entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-wallet ]}"

  PORT=8090

  cardano-wallet serve --port "$PORT" --node-socket ${socket-path} --testnet ${node-config-dir}/byron-genesis.json
''
