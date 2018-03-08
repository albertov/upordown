{ pkgsPath ? <nixpkgs> }:
let pkgs = import pkgsPath {};
in pkgs.haskellPackages.callCabal2nix "upordown" ./.  {}
