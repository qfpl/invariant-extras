{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      invariant-extras = import ../../invariant-extras/. { inherit nixpkgs compiler; };
      common = import ../common/. { inherit nixpkgs compiler; };
    };
  };

  drv = modifiedHaskellPackages.callPackage ./binary-examples.nix {};
in
  drv
