{
  description = "template";

  inputs = {
    nixpkgs.url = "github:Nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/nixos-unstable/nixexprs.tar.xz";
    # nixpkgs.url = "https://mirrors.ustc.edu.cn/nix-channels/nixos-unstable/nixexprs.tar.xz";
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
