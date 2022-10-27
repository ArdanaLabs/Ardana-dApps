{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      ctl-utils = {
        dependencies =
          with ps-pkgs;
          [
            cardano-transaction-lib
            prelude
          ];
        ps =
          purs-nix.purs
            {
              inherit (ctl-utils) dependencies;
              dir = ./.;
            };
        package =
          purs-nix.build
            {
              name = "ctl-utils";
              src.path = ./.;
              info = {
                inherit (ctl-utils) dependencies;
                version = "0.0.1";
              };
            };
      };
    in
    {
      packages."offchain:ctl-utils" = ctl-utils.package;
      checks = {
        "offchain:ctl-utils:compile" =
          pkgs.runCommand "compile-ctl-utils" { buildInputs = [ (ctl-utils.ps.command { srcs = [ self'.packages."offchain:ctl-utils" ]; }) ]; } ''
            set -euo pipefail
            purs-nix compile
            touch $out
          '';
      };

      devShells."offchain:ctl-utils" =
        offchain-lib.makeProjectShell { project = ctl-utils; };
    };
  flake = { };
}
