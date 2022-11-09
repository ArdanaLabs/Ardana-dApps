{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      dusd-browser = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  halogen
                  halogen-store
                  halogen-svg-elems
                  cardano-transaction-lib
                  # self'.packages."offchain:dusd-api"
                ];
              dir = ./.;
            };
        package =
          let
            nodeModules = pkgs.symlinkJoin {
              name = "dusd-browser-node-modules";
              paths = [
                config.ctl.nodeModules
              ];
            };
          in
          pkgs.runCommand "build-dusd-browser" { }
            # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              export BROWSER_RUNTIME=1
              cp -r ${dusd-browser.ps.modules."Dusd.Browser.Home".output { }} homeOutput
              cp ${./home.js} home.js
              cp -r ${nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir -p $out/dist
              webpack --mode=production -c ${../webpack.config.js} -o $out/dist/home --entry ./home.js
            '';
      };
    in
    {
      devShells = {
        "offchain:dusd-browser" =
          offchain-lib.makeProjectShell { project = dusd-browser; };
      };
      packages = {
        "offchain:dusd-browser" = dusd-browser.package;
      };
    };
  flake = { };
}
