{
  description = "template";

  inputs = {
    nixpkgs.url = "github:Nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Examples of depending on flake project hosted in my github
    # chromium4ghunter.url = "git+ssh://git@github.com/lllssskkk/chromium4ghunter.git";
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
          ])

        );

        flakeDep = [
        ];

        # project = pkgs.rustPlatform.buildRustPackage {
        #   pname = "ghunter4chrome";
        #   version = "0.1";

        #   cargoLock = {
        #     lockFile = ./Cargo.lock;
        #     outputHashes = {
        #       "chromiumoxide-0.7.0" = "sha256-ZRTk5r5WLq9c+rvjqyAnB7pzdFbXl2IQBTg2w77IllY=";
        #     };

        #   };

        #   nativeBuildInputs = devEnv;

        #   src = pkgs.lib.cleanSource ./.;
        # };
        devShell = pkgs.mkShell {
          packages = devEnv ++ flakeDep;
          shellHook = ''
            export PATH=${self}/bin:$PATH
          '';
        };

        # runScript = pkgs.writeShellApplication {
        #   name = "ghunter4chromium";
        #   runtimeInputs = [
        #     project
        #     chromium4ghunter.packages.${system}.default
        #   ];

        #   text = ''
        #     exec ${project}/bin/ghunter4chromium-gadget-finder \
        #       --chromium-executable ${chromium4ghunter.packages.${system}.default}/bin/chromium-ghunter \
        #       --url "https://smallforbig.com" "$@"
        #   '';
        # };

      in
      {
        devShells.default = devShell;
        # packages.default = runScript;
        # apps.default = flake-utils.lib.mkApp { drv = runScript; };
      }
    );
}
