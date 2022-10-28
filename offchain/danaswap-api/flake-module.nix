{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      danaswap-api = {
        dependencies =
          with ps-pkgs;
          [
            aeson
            aff
            bigints
            cardano-transaction-lib
            ordered-collections
            aff-retry
            self'.packages."offchain:danaswap-cbor"
          ];
        test-dependencies =
          with ps-pkgs;
          [
            node-process
            spec
          ];
        ps =
          purs-nix.purs
            {
              inherit (danaswap-api) dependencies test-dependencies;
              dir = ./.;
            };
        package =
          purs-nix.build
            {
              name = "danaswap-api";
              src.path = ./.;
              info = {
                inherit (danaswap-api) dependencies;
                version = "0.0.1";
              };
            };
      };

      danaswap-api-tests = { mode, runVolumeTests ? false }:
        pkgs.writeShellApplication
          {
            name = "danaswap-api-tests";
            runtimeInputs = [
              pkgs.nodejs
              inputs'.yubihsm.packages.default
            ] ++ pkgs.lib.optionals (mode == "local") [
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text =
              pkgs.lib.optionalString runVolumeTests "export RUN_VOLUME_TESTS=1" + ''
                export MODE=${mode}
                export TEST_RESOURCES=${./fixtures}
                export NODE_PATH=${config.ctl.nodeModules}/node_modules
                ${danaswap-api.ps.test.run { }}
              '';
          };
    in
    {
      apps = {
        "offchain:danaswap-api:test:testnet" = cat-lib.mkApp (danaswap-api-tests { mode = "testnet"; });
        "offchain:danaswap-api:test:local" = cat-lib.mkApp (danaswap-api-tests { mode = "local"; });
      };
      checks = {
        "offchain:danaswap-api:test:local" =
          let test = danaswap-api-tests { mode = "local"; }; in
          pkgs.runCommand test.name { }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        "offchain:danaswap-api:test:volume" =
          let test = danaswap-api-tests { mode = "local"; runVolumeTests = true; }; in
          pkgs.runCommand test.name { }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
      };
      devShells."offchain:danaswap-api" =
        offchain-lib.makeProjectShell { project = danaswap-api; extraBuildInputs = [ inputs'.yubihsm.packages.default ]; };
      packages = {
        "offchain:danaswap-api" = danaswap-api.package;
        "offchain:danaswap-api:docs" =
          pkgs.runCommand "danaswap-api-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${danaswap-api.ps.command { srcs = [ ./src ];}}/bin/purs-nix docs
            '';
      };
    };
  flake = { };
}
