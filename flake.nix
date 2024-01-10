{
  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.11";
    haskell.url = "github:nixos/nixpkgs/6dc93f0daec55ee2f441da385aaf143863e3d671";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgsArgs = { inherit system; config = { }; };
      nixpkgs = {
        stable = import inputs.stable nixpkgsArgs;
        haskell = import inputs.haskell nixpkgsArgs;
      };
      freckle = inputs.freckle.packages.${system};
      freckleLib = inputs.freckle.lib.${system};
    in
    rec {
      packages = {
        awscli = freckle.aws-cli-2-11-x;

        apply-refact =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.stable.haskellPackages.apply-refact;

        cabal = nixpkgs.stable.cabal-install;

        dhall = nixpkgs.stable.dhall;

        fast-tags =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.stable.haskellPackages.fast-tags;

        fourmolu = freckle.fourmolu-0-13-x;

        ghc = freckleLib.haskellBundle {
          ghcVersion = "ghc-9-4-8";
          packageSelection = p: [
            p.base64
            p.bytestring
            p.containers
            p.cookie
            p.crypton
            p.data-default
            p.hspec
            p.mtl
            p.nonce
            p.persistent
            p.QuickCheck
            p.text
            p.time
            p.wai
            p.yesod
            p.yesod-core
            p.yesod-test
          ];
        };

        haskell-language-server =
          nixpkgs.stable.haskell-language-server.override
            { supportedGhcVersions = [ "948" ]; };

        hlint =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.stable.hlint;

        hiedb =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.haskell.haskellPackages.hiedb;

        stack = nixpkgs.stable.writeShellApplication {
          name = "stack";
          text = ''
            ${nixpkgs.stable.stack}/bin/stack --system-ghc --no-nix "$@"
          '';
        }
        ;
      };

      devShells.default = nixpkgs.stable.mkShell {
        buildInputs = with (nixpkgs.stable); [
          pcre
          pcre.dev
          zlib
          zlib.dev
        ];

        nativeBuildInputs = with (packages); [
          cabal
          fourmolu
          ghc
          haskell-language-server
          hlint
          stack
        ];

        LOCALE_ARCHIVE = "${nixpkgs.stable.glibcLocales}/lib/locale/locale-archive";

        shellHook = ''
          export STACK_YAML=stack.yaml
        '';
      };
    });
}
