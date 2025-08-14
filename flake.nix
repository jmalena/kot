{
  description = "Kot Language Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    nixpkgs-old.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-old, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        pkgsOld = import nixpkgs-old {
          inherit system;
          config.allowBroken = true;
        };
        
        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = self: super: {
            llvm-hs = super.llvm-hs.override {
              llvm-config = pkgsOld.llvm_9;
            };
          };
        };
        
        kotPkg = haskellPackages.callCabal2nix "kot" ./. {};
      in {
        packages.default = kotPkg;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell.compiler.ghc8107
            haskellPackages.cabal-install
            haskellPackages.ghcid
            pkgsOld.llvm_9
            kotPkg
          ];
        };
      }
    );
}
