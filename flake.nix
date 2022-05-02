{
  description = "dUSD";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    plutus-apps.url = "github:input-output-hk/plutus-apps?rev=e4062bca213f233cdf9822833b07aa69dff6d22a";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      cardano-node,
      plutus,
      plutus-apps,
      lint-utils
    }
    @ inputs:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" ]; #"aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      # Name of our project; used in script prefixes.
      projectName = "dusd";

      # Derivation for a Haskell Plutus project that lives in the sub-directory of this mono repo.
      plutusProjectIn =
        { system
        , subdir          # The sub-directory name
        , extraShell      # Extra 'shell' attributes used by haskell.nix
        , pkg-def-extras  # For overriding the package set
        , sha256map       # Extra sha256 hashes used by haskell.nix
        }:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;
          fakeSrc = pkgs.runCommand "real-source-${subdir}" { } ''
            cp -rT ${self}/${subdir} $out
            chmod u+w $out/cabal.project
            cat $out/cabal-haskell.nix.project >> $out/cabal.project
          '';
        in
          pkgs.haskell-nix.cabalProject' {
            inherit pkg-def-extras;
            src = fakeSrc.outPath;
            compiler-nix-name = "ghc8107";
            cabalProjectFileName = "cabal.project";
            modules = [
              {
                packages = {
                  marlowe.flags.defer-plugin-errors = deferPluginErrors;
                  plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                  plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                  plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                  cardano-crypto-praos.components.library.pkgconfig =
                    nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                  cardano-crypto-class.components.library.pkgconfig =
                    nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                };
              }
            ];
            shell = {
              withHoogle = true;
              tools = {
                haskell-language-server = { };
              };
              exactDeps = true;
              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];
            } // extraShell;
            inherit sha256map;
          } ;

      lintSpec = {
        cabal-fmt = {};
        fourmolu = {
          ghcOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
        };
        # Enable after https://github.com/ArdanaLabs/dUSD/issues/8
        # nixpkgs-fmt = {};
      };

      # Checks the shell script using ShellCheck
      checkedShellScript = system: name: text:
        ((nixpkgsFor system).writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";

      # Take a flake app (identified as the key in the 'apps' set), and return a
      # derivation that runs it in the compile phase.
      #
      # In effect, this allows us to run an 'app' as part of the build process (eg: in CI).
      flakeApp2Derivation = system: appName:
        (nixpkgsFor system).runCommand appName { } "${self.apps.${system}.${appName}.program} | tee $out";
    in
      {
        onchain-scripts = forAllSystems (system: (nixpkgsFor system).stdenv.mkDerivation {
          name = "onchain-scripts";
          src = self;
          buildInputs = [ self.onchain.${system}.flake.packages."dUSD-onchain:exe:scripts" ];
          doCheck = false;
          installPhase = ''
            scripts "$out"
            '';
          configurePhase = ''
            mkdir $out
            '';
        });

        onchain = forAllSystems (system: rec {
          project = plutusProjectIn {
            inherit system;
            subdir = "onchain";
            extraShell = {
              additional = ps: [
                ps.apropos
                ps.apropos-tx
                ps.plutarch
                ps.plutarch-extra
                ps.sydtest
                ps.sydtest-hedgehog
              ];
            };
            pkg-def-extras = [];
            sha256map = {
              "https://github.com/mlabs-haskell/apropos"."455b1a3ad1eee35de4d3fb8c4a4527071474336c" = "sha256-EC6vnimXA+jBRPQLLs2dltuTx9XoSdkQfh742NnLJSQ=";
              "https://github.com/mlabs-haskell/apropos-tx"."489eeb8c30d62d5c75eafe4242a1f133695f8564" = "sha256-15nFGPhXBy+G0oocb6KQf5KVnT0fuAOoFCdzT+vyeEg=";
              "https://github.com/Plutonomicon/plutarch"."cae000584f5492daa2e13a08632ea69221a8c147" = "sha256-ZEGKBr1q852sx5aOOAqWHKZR+//YByQmGmBKglGr+qQ=";
              "https://github.com/input-output-hk/plutus.git"."983e6af2154c4bdf86ed645062bcb62f304d0a4f" = "sha256-Ga+hIhrgq2kR5Vnso/Edo2wgQpFn167eWGl35oM093U=";
              "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
              "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
              "https://github.com/input-output-hk/cardano-base"."78b3928391b558fb1750228f63301ec371f13528" = "pBUTTcenaSLMovHKGsaddJ7Jh3okRTrtu5W7Rdu6RM4=";
              "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
              "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
              "https://github.com/Srid/sydtest"."9c6c7678f7aabe22e075aab810a6a2e304591d24" = "sha256-P6ZwwfFeN8fpi3fziz9yERTn7BfxdE/j/OofUu+4GdA=";
              "https://github.com/Srid/autodocodec"."42b42a7407f33c6c74fa4e8c84906aebfed28daf" = "sha256-X1TNZlmO2qDFk3OL4Z1v/gzvd3ouoACAiMweutsYek4=";
              "https://github.com/Srid/validity"."f7982549b95d0ab727950dc876ca06b1862135ba" = "sha256-dpMIu08qXMzy8Kilk/2VWpuwIsfqFtpg/3mkwt5pdjA=";
            };
          };
          flake = project.flake { };
        });

        offchain = forAllSystems (system: rec {
          project = plutusProjectIn {
            inherit system;
            subdir = "offchain";
            extraShell = {
              additional = ps: [
                ps.plutus-contract
                ps.plutus-ledger
                ps.plutus-ledger-api
                ps.plutus-ledger-constraints
                ps.plutus-pab
              ];
              DUSD_SCRIPTS = self.onchain-scripts.${system};
              propagatedBuildInputs =
                let pkgs = nixpkgsFor system;
                in [
                # cardano-node and cardano-cli need to be on the PATH to run the
                # cluster + PAB.
                cardano-node.outputs.packages.x86_64-linux."cardano-node:exe:cardano-node"
                cardano-node.outputs.packages.x86_64-linux."cardano-cli:exe:cardano-cli"
              ];
              tools = {
                ghcid = { };
                haskell-language-server = { };
              };
            };
            pkg-def-extras = [
              (hackage: {
                packages = {
                  cryptostore = (((hackage."cryptostore")."0.2.1.0").revisions).default;
                  jwt = (((hackage."jwt")."0.11.0").revisions).default;
                  random = (((hackage.random)."1.2.1").revisions).default;
                };
              })
            ];
            sha256map = {
              # iohk
              "https://github.com/input-output-hk/cardano-addresses"."5a313b60ed64e4374095de65bc13cb080001e520" = "sha256-K7j84d9UzUDH3aekpH5IMXyUpG1ciIfb2t2+0o9VHKI=";
              "https://github.com/input-output-hk/cardano-base"."5c1786f3a2b9b2647489862963003afdc1f27818" = "sha256-cMQjyQDdHQvZwc9MIJ+cPyxFW0rEPPidEytAed5IZns=";
              "https://github.com/input-output-hk/cardano-config"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6" = "sha256-jQbwcfNJ8am7Q3W+hmTFmyo3wp3QItquEH//klNiofI=";
              "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
              "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" = "sha256-lRNfkGMHnpPO0T19FZY5BnuRkr0zTRZIkxZVgHH0fys=";
              "https://github.com/input-output-hk/cardano-node"."${inputs.cardano-node.rev}" = "sha256-e4k1vCsZqUB/I3uPRDIKP9pZ81E/zosJn8kXySAfBcI=";
              "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
              "https://github.com/input-output-hk/cardano-wallet"."f6d4db733c4e47ee11683c343b440552f59beff7" = "sha256-3oeHsrAhDSSKBSzpGIAqmOcFmBdAJ5FR02UXPLb/Yz0=";
              "https://github.com/input-output-hk/ekg-forward"."297cd9db5074339a2fb2e5ae7d0780debb670c63" = "sha256-jwj/gh/A/PXhO6yVESV27k4yx9I8Id8fTa3m4ofPnP0=";
              "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "sha256-z9ut0y6umDIjJIRjz9KSvKgotuw06/S8QDwOtVdGiJ0=";
              "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "sha256-QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
              "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "sha256-uQx+SEYsCH7JcG3xAT0eJck9yq3y0cvx49bvItLLer8=";
              "https://github.com/input-output-hk/ouroboros-network"."4fac197b6f0d2ff60dc3486c593b68dc00969fbf" = "sha256-Cy29MHrYTkN7s3Vvog5/pOzbo7jiqTeDz6OmrNvag6w=";
              "https://github.com/input-output-hk/plutus.git"."4127e9cd6e889824d724c30eae55033cb50cbf3e" =
              "sha256-S8uvyld7ZpPsmxZlWJeRNAPd+mw3PafrtaiiuU8H3KA=";
              "https://github.com/input-output-hk/plutus-apps"."${inputs.plutus-apps.rev}" = "sha256-Aoo+hGLUQTAkuIGTG+mpOE/DSlV8KEe5kvUZZdYez48=";
              "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" = "sha256-/SbnmXrB9Y2rrPd6E79Iu5RDaKAKozIl685HQ4XdQTU=";
              "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "sha256-DH9ISydu5gxvN4xBuoXVv1OhYCaqGOtzWlACdJ0H64I=";
              "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";

              # misc
              "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            };
          };
          flake = project.flake { };
        });

        # this could be done automatically, but would reduce readability
        packages = forAllSystems (system:
          let pkgs = nixpkgsFor system;
          in self.onchain.${system}.flake.packages
          // self.offchain.${system}.flake.packages
          // {
            build-docs = pkgs.stdenv.mkDerivation {
              name = "build-docs";
              src = self;
              buildInputs = with pkgs; [ (texlive.combine { inherit ( texlive ) scheme-basic latexmk todonotes metafont; }) ];
              doCheck = false;
              buildPhase = ''
                HOME=$TMP latexmk -output-directory="tmp" -pdf ./docs/*.tex
                mkdir $out -p
                cp tmp/*.pdf $out
              '';
              installPhase = ''
                ls -lah
              '';
            };
        });

        checks = forAllSystems (system:
             self.onchain.${system}.flake.checks
          // { offchain-test = flakeApp2Derivation system "offchain-test";
             }
          // (lint-utils.mkChecks.${system} lintSpec ./.)
        );
        # We need this attribute because `nix flake check` won't work for Haskell
        # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        #
        # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
        check = forAllSystems (
          system:
            (nixpkgsFor system).runCommand "combined-test" {
              checksss = builtins.attrValues self.checks.${system}
              # This allows us to cache nix-shell (nix develop)
              # https://nixos.wiki/wiki/Caching_nix_shell_build_inputs
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
              ];
            } ''
              echo $checksss
              touch $out
              ''
        );

        # Shell tools common to both onchain and offchain
        commonTools = forAllSystems (system:
          let pkgs = nixpkgsFor system;
          in {
              feedback-loop = pkgs.callPackage ./nix/apps/feedback-loop { inherit projectName; };
            });

        haskellTools = forAllSystems (system: 
          let pkgs = nixpkgsFor system;
          in {
            # Run `ghcid` on the given project (onchain or offchain), using
            # alias 'name' and ghcid arguments 'args'.
            ghcid = subProject: name: args: 
              pkgs.callPackage ./nix/apps/ghcid {
                inherit projectName name args;
                cabalProjectRoot = "${self.flakeRoot.envVar}/${subProject}";
              };
          }
        );

        # In Nix, there is no builtin way to access the project root, where
        # flake.nix lives. To workaround this, we inject it as env var in the
        # `shellHook`.
        flakeRoot = {
          shellHook = ''
            export FLAKE_ROOT=$(pwd)
          '';
          envVar = "$FLAKE_ROOT";
        };
 
        # We are forced to use two devshells.
        # Under ideal circumstances, we could put all the onchain and offchain
        # code in the same project, sharing the same cabal.project, but this is
        # not possible because:
        #
        # On-chain code requires recent versions of plutarch, which uses a
        # more recent version of `plutus` than is in `plutus-apps`.
        #
        # So, in order to remove this hack and use one cabal project instead, we need:
        #
        # Plutarch to be more or less stable so that it can use the version
        # of `plutus` that is in `plutus-apps` at the time, instead of a recent
        # one.
        #
        # There was also the idea of using a plutus-tx (so not Plutarch)
        # dummy-implementation of an on-chain validator until these two
        # conditions are met. We opted not to do this because it would require
        # us to bet that the condition above would be met before we want to launch.
        devShells = forAllSystems (system: let pkgs = nixpkgsFor system; in {
          onchain = self.onchain.${system}.flake.devShell.overrideAttrs (oa: {
            shellHook = oa.shellHook + self.flakeRoot.shellHook;
            buildInputs = pkgs.lib.attrsets.attrValues self.commonTools.${system} ++ 
              [ (self.haskellTools.${system}.ghcid "onchain" "lib" "-c 'cabal repl'")
                (self.haskellTools.${system}.ghcid "onchain" "test" "-c 'cabal repl test:tests'")
                (self.haskellTools.${system}.ghcid "onchain" "test-run" "-c 'cabal repl test:tests' -T :main")
              ];
          });
          offchain = self.offchain.${system}.flake.devShell.overrideAttrs (oa: {
            buildInputs = pkgs.lib.attrsets.attrValues self.commonTools.${system};
            shellHook = oa.shellHook + ''
              # running local cluster + PAB
              export SHELLEY_TEST_DATA="${plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley/"
            '';
          });
        });
        defaultPackage = forAllSystems (system:
             self.packages.${system}."dUSD-onchain:test:tests"
          // self.packages.${system}."dUSD-offchain:exe:tests"
        );
        apps = forAllSystems (system: let
          pkgs = nixpkgsFor.system;
          # Convert a derivation from `commonTools` to a flake app.
          commonToolApp = system: name:
            {
              type = "app";
              # We expect the script name of all common tools to be prefixed with dusd-
              program = "${ self.commonTools.${system}.${name}}/bin/${projectName}-${name}";
            };
        in
          {
            feedback-loop = commonToolApp system "feedback-loop";
            format =  lint-utils.mkApp.${system} lintSpec;  # TODO: Refactor this to be like `commonToolApp`.
            offchain-test = {
              type = "app";
              program = checkedShellScript system "dUSD-offchain-test"
                '' export DUSD_SCRIPTS=${self.onchain-scripts.${system}}
                   cd ${self}
                   ${self.packages.${system}."dUSD-offchain:exe:tests"}/bin/tests;
                '';
            };
          });
      };
}
