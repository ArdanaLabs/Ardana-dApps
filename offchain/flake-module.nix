{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world";
      purs-nix = self.inputs.purs-nix { inherit system; };
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };

      ctl-rev = self.inputs.cardano-transaction-lib.rev;

      dusd-lib = config.dusd-lib;

      ps-pkgs-ctl =
        let
          f = self:
            import ./ps-pkgs-ctl.nix {
              ps-pkgs = purs-nix.ps-pkgs // self;
              inherit ctl-rev;
            };
        in
        pkgs.lib.fix
          (self:
            builtins.mapAttrs (n: v: purs-nix.build (v // { name = n; })) (f self)
          );
      all-ps-pkgs = purs-nix.ps-pkgs // ps-pkgs-ctl;

      hello-world-cbor =
        purs-nix.build
          {
            name = "hello-world-cbor";
            src.path = self'.packages."onchain:hello-world-cbor-purs";
            info.dependencies = [ ];
            info.version = "0.0.1";
          };

      hello-world-api = {
        dependencies =
          with all-ps-pkgs;
          [
            aeson
            aff
            bigints
            cardano-transaction-lib
            hello-world-cbor
            node-child-process
            node-fs-aff
            node-process
            ordered-collections
            spec
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies;
              srcs = [ ./hello-world-api ];
            };
        package =
          purs-nix.build
            {
              name = "hello-world-api";
              src.path = ./hello-world-api;
              info = {
                inherit (hello-world-api) dependencies;
                version = "0.0.1";
              };
            };
      };

      hello-world-browser = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  aff
                  bigints
                  halogen
                  halogen-store
                  safe-coerce
                  transformers
                  cardano-transaction-lib
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-browser/src ];
            };
      };

      hello-world-browser-e2e = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  aff
                  cardano-transaction-lib
                  express
                  mote
                  node-process
                  test-unit
                  toppokki
                ];
              srcs = [ ./hello-world-browser/test/e2e ];
            };
      };

      hello-world-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  prelude
                  hello-world-api.package
                  optparse
                  node-fs-aff
                  node-fs
                  dotenv
                  stringutils
                ];
              srcs = [ ./hello-world-cli ];
            };
      };

      ctlNodeModules = "${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}";

      # Ideally we would just append the CTL overlay to the haskell-nix pkgs
      # we already have at `config.haskell-nix.pkgs`, but our haskell-nix
      # instances seem to be incompatible. So we just use CTLs haskell-nix here.
      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = with self.inputs.cardano-transaction-lib; [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          overlays.runtime
        ];
      };

      # use more recent slot to avoid long sync time
      ctlRuntimeConfig = {
        datumCache.blockFetcher.firstBlock = {
          slot = 62153233;
          id = "631c621b7372445acf82110282ba72f4b52dafa09c53864ddc2e58be24955b2a";
        };
      };

      hello-world-api-tests =
        let
          testModule = hello-world-api.ps.modules."Test.Main".output { };
          scriptName = "hello-world-api-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [ pkgs.nodejs ];
            text = ''
              export TEST_RESOURCES=${./hello-world-api/fixtures}
              export NODE_PATH=${ctlNodeModules}/node_modules
              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/Test.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
      hello-world-cli-tests =
        let
          testExe =
            hello-world-cli.ps.modules."Test.Main".app
              { name = scriptName; };
          scriptName = "hello-world-cli-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [
              testExe
              self'.packages."offchain:hello-world-cli"
              pkgs.coreutils
            ];
            text = ''
              export TEST_RESOURCES=${./hello-world-cli/fixtures}
              ${scriptName}
            '';
          };
      hello-world-browser-tests =
        let
          testModule = hello-world-browser-e2e.ps.modules."HelloWorld.Test.E2E.Main".output { };
          scriptName = "hello-world-browser-tests";

          namiSettings = "${self.inputs.cardano-transaction-lib}/test-data/nami_settings.tar.gz";
          namiExtension = "${self.inputs.cardano-transaction-lib}/test-data/chrome-extensions/nami_3.2.5_1.crx";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [ self'.packages."offchain:hello-world-browser" pkgs.nodejs pkgs.chromium pkgs.unzip ];
            text = ''
              export NODE_PATH=${ctlNodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium.outPath}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser"}

              TEST_DATA="$(mktemp --directory)"
              unzip ${namiExtension} -d "$TEST_DATA/nami" > /dev/zero || echo "ignore warnings" 
              tar zxf ${namiSettings} --directory "$TEST_DATA"
              export TEST_DATA

              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/HelloWorld.Test.E2E.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
      prefixOutputs = dusd-lib.prefixAttrNames "offchain";
    in
    {
      packages =
        prefixOutputs {
          inherit hello-world-cbor;
          hello-world-api = hello-world-api.package;
          docs =
            pkgs.runCommand "offchain-docs" { }
              ''
                mkdir $out && cd $out
                # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
                ${hello-world-api.ps.command { srcs = [ ./hello-world-api/src ];} }/bin/purs-nix docs
              '';
          hello-world-browser =
            pkgs.runCommand "build-hello-world-browser" { }
              # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
              # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
              ''
                mkdir $out && cd $out
                export BROWSER_RUNTIME=1
                cp -r ${hello-world-browser.ps.modules.Main.output { }} output
                cp ${./hello-world-browser/index.js} index.js
                cp ${./hello-world-browser/index.html} index.html
                cp ${./webpack.config.js} webpack.config.js
                cp -r ${ctlNodeModules}/* .
                export NODE_PATH="node_modules"
                export PATH="bin:$PATH"
                mkdir dist
                cp ${./hello-world-browser/main.css} dist/main.css
                webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
              '';
          hello-world-cli =
            let js = "${hello-world-cli.ps.modules.Main.output {}}/Main/index.js"; in
            pkgs.writeScriptBin "hello-world-cli"
              ''
                export NODE_PATH=${ctlNodeModules}/node_modules
                ${pkgs.nodejs}/bin/node \
                  --preserve-symlinks \
                  --input-type=module \
                  -e 'import { main } from "${js}"; main()' \
                  -- "hello-world-cli" "''$@"
              '';
        };

      checks = {
        run-hello-world-api-tests =
          let test = hello-world-api-tests; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        run-hello-world-cli-tests =
          let test = hello-world-cli-tests; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        run-hello-world-browser-tests =
          let test = hello-world-browser-tests; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        ctl-runtime-modules-test = inputs'.nixpkgs.legacyPackages.callPackage ./nixos/tests/ctl-runtime-modules.nix {
          inherit (self.inputs) cardano-node cardano-ogmios mlabs-ogmios;
          inherit (self.nixosModules) ctl-server ogmios-datum-cache;
        };
      };

      apps =
        { ctl-runtime = ctl-pkgs.launchCtlRuntime config; }
        // (
          let
            makeServeApp = pathToServe:
              dusd-lib.mkApp (
                pkgs.writeShellApplication
                  {
                    name = projectName;
                    runtimeInputs = [ pkgs.nodePackages.http-server ];
                    text = "http-server -c-1 ${pathToServe}";
                  }
              );
          in
          prefixOutputs {
            "docs:serve" =
              makeServeApp "${self'.packages."offchain:docs"}/generated-docs/html/";
            "hello-world-browser:serve" =
              makeServeApp self'.packages."offchain:hello-world-browser";

            "hello-world-api:test" =
              dusd-lib.mkApp hello-world-api-tests;
            "hello-world-cli:test" =
              dusd-lib.mkApp hello-world-cli-tests;
            "hello-world-browser:test" =
              dusd-lib.mkApp hello-world-browser-tests;
          }
        );

      devShells =
        let
          # Helper function to create a devshell without declaring common dependencies.
          # If you want to add more dependencies, use `.overrideAttrs (old: { ... })`.
          makeProjectShell = project: cmdArgs:
            pkgs.mkShell {
              name = projectName;
              buildInputs = (with pkgs; [
                nodejs-16_x
                (project.ps.command cmdArgs)
                purs-nix.ps-pkgs.psci-support
                purs-nix.purescript
                purs-nix.purescript-language-server
                nodePackages.purs-tidy
              ]);
              shellHook = "export NODE_PATH=${ctlNodeModules}/node_modules/";
            };
        in
        prefixOutputs {
          hello-world-cli = makeProjectShell hello-world-cli { };
          hello-world-browser = makeProjectShell hello-world-browser { };
          hello-world-api = makeProjectShell hello-world-api { };
          "hello-world-browser:e2e" = makeProjectShell hello-world-browser-e2e { };
        };
    };
  flake = {
    nixosModules.ogmios-datum-cache = { pkgs, lib, ... }: {
      imports = [ ./nixos/modules/ogmios-datum-cache.nix ];
      services.ogmios-datum-cache.package = lib.mkDefault self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system};
    };
    nixosModules.ctl-server = { pkgs, lib, ... }: {
      imports = [ ./nixos/modules/ctl-server.nix ];
      services.ctl-server.package = lib.mkDefault self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server";
    };
  };
}
