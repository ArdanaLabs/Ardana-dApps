{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };
    in
    {
      packages = {
        danaswap-ui =
          let
	    nodeModules = pkgs.symlinkJoin {
              name = "danaswap-ui-node-modules";
              paths = [
                (npmlock2nix.node_modules { src = ./.; })
              ];
            };
          in
          pkgs.runCommand "build-danaswap-ui"
            { buildInputs = [ pkgs.nodejs-16_x self'.packages."offchain:danaswap-browser" ]; }
            ''
              cp ${./build.js} build.js
              cp ${./routes.json} routes.json
              cp ${./package.json} package.json
              cp ${./package-lock.json} package-lock.json
              cp -r ${./views} views
              cp -r ${./assets} assets
              cp -r ${./plugins} plugins
              cp -r ${nodeModules}/* .
              node build.js
              cp ${self'.packages."offchain:danaswap-browser"}/dist/*.js build/assets/scripts/
              mkdir -p $out
              cp -r build $out/
            '';
      };
    };
  flake = { };
}
