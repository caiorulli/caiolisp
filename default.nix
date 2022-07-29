{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./caiolisp.nix { }
