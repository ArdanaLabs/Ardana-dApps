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
              mkdir -p $out
              cp -r ${ui}/lib/node_modules/danaswap-ui/build $out && chmod -R +w $out/build
              cp ${self'.packages."offchain:danaswap-browser"}/dist/*.js $out/build/assets/scripts
            '';
      };
    };
  flake = { };
}
