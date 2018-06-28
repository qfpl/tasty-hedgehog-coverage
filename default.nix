{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./tasty-hedgehog-coverage.nix {};

in
  if pkgs.lib.inNixShell then drv.env else drv
