{
  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.11";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgsArgs = { inherit system; config = { }; };
      nixpkgs = {
        stable = import inputs.stable nixpkgsArgs;
      };
      freckle = inputs.freckle.packages.${system};
      freckleLib = inputs.freckle.lib.${system};
    in
    rec {
      packages = {
        cabal = nixpkgs.stable.cabal-install;

        fourmolu = freckle.fourmolu-0-13-x;

        ghc = freckleLib.haskellBundle {
          enableHLS = true;
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

        hlint =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.stable.hlint;

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
