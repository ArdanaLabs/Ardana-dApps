{ lib, self, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
in
{
  perSystem = { config, self', inputs', system, ... }: {
    options = {
      ps = {
        purs-nix = mkOption {
          type = types.unspecified;
          default = self.inputs.purs-nix {
            inherit system;
            overlays = [
              (import ./ctl-overlay.nix {
                ctl-rev = self.inputs.cardano-transaction-lib.rev;
              })
            ];
          };
        };
      };
    };
  };
}

