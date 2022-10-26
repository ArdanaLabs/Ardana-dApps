{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) cat-lib offchain-lib;

      ctl-utils-test = {
        dependencies =
          with ps-pkgs;
          [
            ctl-pkgs.cardano-transaction-lib
            prelude
            bigints
            node-fs-aff
            self'.packages."offchain:ctl-utils"
          ];
        ps =
          purs-nix.purs
            {
              inherit (ctl-utils-test) dependencies;
              dir = ./.;
            };
        package =
          purs-nix.build
            {
              name = "ctl-utils-test";
              src.path = ./.;
              info = {
                inherit (ctl-utils-test) dependencies;
                version = "0.0.1";
              };
            };
      };
    in
    {
      packages."offchain:ctl-utils-test" = ctl-utils-test.package;

      checks = {
        "offchain:ctl-utils-test:compile" =
          pkgs.runCommand "compile-ctl-utils-test" { buildInputs = [ (ctl-utils-test.ps.command { srcs = [ self'.packages."offchain:ctl-utils-test" ]; }) ]; } ''
            set -euo pipefail
            purs-nix compile
            touch $out
          '';
      };

      devShells."offchain:ctl-utils-test" =
        offchain-lib.makeProjectShell { project = ctl-utils-test; };
    };
  flake = { };
}
