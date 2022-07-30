{ pkgs ? import <nixpkgs> {} }:
let
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    rebar3
    lfe
  ];
  shellHook = ''
  '';
}
