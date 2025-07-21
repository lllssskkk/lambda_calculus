{
  description = "template";

  inputs = {
    nixpkgs.url = "github:Nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        haskell-pkgs = pkgs.haskellPackages;

        devEnv = (
          (with pkgs; [
            bashInteractive
            fd
            git
            gnumake
            clang
            cabal-install
            alex
            happy
            wavebox
          ])
          ++ [
            pkgs.haskell.compiler.ghc9102
            pkgs.haskell.packages.ghc9102.haskell-language-server
          ]
          ++ (with haskell-pkgs; [
            BNFC
            cabal-plan
          ])

        );

        flakeDep = [
        ];

        devShell = pkgs.mkShell {
          packages = devEnv ++ flakeDep;
          shellHook = ''
            export PATH=${self}/bin:$PATH
          '';
        };

      in
      {
        devShells.default = devShell;
      }
    );
}
