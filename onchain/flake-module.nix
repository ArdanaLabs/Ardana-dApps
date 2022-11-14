{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      inherit (self.inputs) plutarch;
      # We use plutarch's nixpkgs / haskell.nix etc. to make sure that we don't
      # bother with mixing and matching nixpkgs / haskell.nix versions.
      pkgs =
        import plutarch.inputs.nixpkgs {
          inherit system;
          overlays = [
            plutarch.inputs.haskell-nix.overlay
            (import "${plutarch.inputs.iohk-nix}/overlays/crypto")
          ];
        };
      compiler-nix-name = "ghc923";

      # cat-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix and some common attributes to give to
      # cabalProject'
      inherit (config) cat-lib;
      inherit (cat-lib.haskell) fixHaskellDotNix mkCommonPlutusShell;

      commonPlutusShell = mkCommonPlutusShell compiler-nix-name pkgs;

      plutarch-hackage =
        plutarch.inputs.haskell-nix-extra-hackage.mkHackagesFor
          system
          compiler-nix-name
          [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
          ];

      project = pkgs.haskell-nix.cabalProject' (
        plutarch.applyPlutarchDep pkgs {
          src = ./.;

          inherit compiler-nix-name;

          inherit (plutarch-hackage)
            extra-hackages
            extra-hackage-tarballs
            modules
            ;

          shell = commonPlutusShell // {
            additional = ps: [
              ps.plutarch
              ps.plutarch-extra
            ];
          };
        }
      );

      haskellNixFlake =
        fixHaskellDotNix (project.flake { }) [ ./onchain.cabal ];
    in
    {
      apps = {
        "onchain:docs:serve" =
          let s = self'.devShells.onchain; in
          cat-lib.mkApp
            (
              pkgs.writeShellApplication {
                name = "onchain-docs-serve";
                runtimeInputs = s.buildInputs ++ s.nativeBuildInputs;
                text = ''
                  ${s.shellHook}
                  hoogle server --database=${self'.packages."onchain:docs:hoogle-db"} --port=8081 --local
                '';
              }
            );

        "onchain:test" =
          cat-lib.mkApp
            (
              pkgs.writeShellApplication
                {
                  name = "run-onchain-tests";
                  text = ''
                    nix build -L ${self}#checks.\"${system}\".\"onchain:test:tests\"
                    cat result/test-stdout
                  '';
                }
            );
      };
      packages =
        haskellNixFlake.packages
        // {
          "onchain:docs:hoogle-db" =
            let s = self'.devShells.onchain; in
            pkgs.runCommand "onchain-docs-db"
              { inherit (s) buildInputs nativeBuildInputs; }
              ''
                ${s.shellHook}
                hoogle generate --database=$out --local
              '';
          "onchain:dusd-cbor-purs" =
            pkgs.runCommand "dusd-cbor-purs" { } ''
              mkdir -p $out/src
              ${haskellNixFlake.packages."onchain:exe:dusd"}/bin/dusd $out/src
            '';
          "onchain:danaswap-cbor-purs" =
            pkgs.runCommand "danaswap-cbor-purs" { } ''
              mkdir -p $out/src
              ${haskellNixFlake.packages."onchain:exe:danaswap"}/bin/danaswap $out/src
            '';
          "onchain:hello-world-cbor-purs" =
            pkgs.runCommand "hello-world-cbor-purs" { } ''
              mkdir -p $out/src
              ${haskellNixFlake.packages."onchain:exe:hello-world"}/bin/hello-world $out/src
            '';
        };
      checks = haskellNixFlake.checks // {
        "onchain:test:tests" =
          haskellNixFlake.checks."onchain:test:tests".overrideAttrs (old: {
            GOLDEN_FILES = "${./goldens}/";
          });
      };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = { };
}
