{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = with haskellPackages; [
    python3
    brittany
    hlint
    stack
    weeder
  ];
}
