{ pkgs ? import <nixpkgs> {} }:
let
in
pkgs.mkShell {
  buildInputs = [
    rebar3
    lfe
  ];
  shellHook = ''
  '';
}
