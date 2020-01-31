{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = with haskellPackages; [
    brittany
    hlint
    stack
    weeder
  ];
}
