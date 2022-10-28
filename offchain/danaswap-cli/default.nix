{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      danaswap-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  prelude
                  node-fs-aff
                  node-fs
                  self'.packages."offchain:danaswap-api"
                ];
              dir = ./.;
              test-dependencies =
                with ps-pkgs;
                [
                  stringutils
                ];
            };
        package =
          let js = "${danaswap-cli.ps.modules.Main.output {}}/Main/index.js"; in
          pkgs.writeShellApplication {
            name = "danaswap-cli";
            runtimeInputs = [ pkgs.nodejs ];
            text = ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${js}"; main()' \
                -- "danaswap-cli" "''$@"
            '';
          };
      };

      danaswap-cli-tests = mode:
        let testExe = danaswap-cli.ps.test.run { }; in
        pkgs.writeShellApplication
          {
            name = "danaswap-cli-tests";
            runtimeInputs = [
              pkgs.coreutils
              self'.packages."offchain:danaswap-cli"
              inputs'.yubihsm.packages.default
            ] ++ pkgs.lib.optionals (mode == "local") [
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text = ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              ${testExe} run --${mode} --test-resources ${./fixtures}
            '';
          };
    in
    {
      apps = {
        "offchain:danaswap-cli" = cat-lib.mkApp (danaswap-cli.package);
        "offchain:danaswap-cli:test:local" = cat-lib.mkApp (danaswap-cli-tests "local");
        "offchain:danaswap-cli:test:testnet" = cat-lib.mkApp (danaswap-cli-tests "testnet");
      };

      packages."offchain:danaswap-cli" = danaswap-cli.package;

      checks."offchain:danaswap-cli:test:local" =
        let test = danaswap-cli-tests "local"; in
        pkgs.runCommand test.name { }
          "${test}/bin/${test.meta.mainProgram} | tee $out";

      devShells."offchain:danaswap-cli" =
        offchain-lib.makeProjectShell { project = danaswap-cli; extraBuildInputs = [ inputs'.yubihsm.packages.default ]; };
    };
  flake = { };
}
