{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      dusd-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  prelude
                  node-fs-aff
                  node-fs
                  self'.packages."offchain:dusd-api"
                ];
              dir = ./.;
              test-dependencies =
                with ps-pkgs;
                [
                  stringutils
                ];
            };
        package =
          let js = "${dusd-cli.ps.modules.Main.output {}}/Main/index.js"; in
          pkgs.writeShellApplication {
            name = "dusd-cli";
            runtimeInputs = [ pkgs.nodejs ];
            text = ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${js}"; main()' \
                -- "dusd-cli" "''$@"
            '';
          };
      };

      dusd-cli-tests = mode:
        let testExe = dusd-cli.ps.test.run { }; in
        pkgs.writeShellApplication
          {
            name = "dusd-cli-tests";
            runtimeInputs = [
              pkgs.coreutils
              self'.packages."offchain:dusd-cli"
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
        "offchain:dusd-cli" = cat-lib.mkApp (dusd-cli.package);
        "offchain:dusd-cli:test:local" = cat-lib.mkApp (dusd-cli-tests "local");
        "offchain:dusd-cli:test:testnet" = cat-lib.mkApp (dusd-cli-tests "testnet");
      };

      packages."offchain:dusd-cli" = dusd-cli.package;

      checks."offchain:dusd-cli:test:local" =
        let test = dusd-cli-tests "local"; in
        pkgs.runCommand test.name { }
          "${test}/bin/${test.meta.mainProgram} | tee $out";

      devShells."offchain:dusd-cli" =
        offchain-lib.makeProjectShell { project = dusd-cli; extraBuildInputs = [ inputs'.yubihsm.packages.default ]; };
    };
  flake = { };
}
