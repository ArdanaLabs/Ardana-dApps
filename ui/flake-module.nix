{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (config) dream2nix;
      ui =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.danaswap-ui;
    in
    {
      packages = {
        danaswap-ui =
          pkgs.runCommand "build-danaswap-ui"
            { buildInputs = [ self'.packages."offchain:danaswap-browser" ]; }
            ''
              mkdir -p $out/build/assets/scripts
              cp -r ${ui}/lib/node_modules/danaswap-ui/build $out
              cp -r ${self'.packages."offchain:danaswap-browser"}/dist/* $out/build/assets/scripts
            '';
      };
    };
  flake = { };
}
