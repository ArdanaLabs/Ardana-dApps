{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib dream2nix offchain-lib;
      dusd-browser =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.dusd-browser;

      dusd-ui-components = {
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
              name = "ctl-node-modules";
              paths = [
                config.ctl.nodeModules
              ];
            };
          in
          pkgs.runCommand "build-dusd-ui-components" { }
            ''
              export BROWSER_RUNTIME=1
              cp -r ${dusd-ui-components.ps.modules."DUsd.UI.Components.Home".output { }} homeOutput
              cp ${./home.js} home.js
              cp -r ${nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir -p $out/dist
              webpack --mode=production -c ${../webpack.config.js} -o $out/dist/home --entry ./home.js
            '';
      };

      optimized-images =
        let
          inherit (pkgs.lib.strings) escapeShellArgs;
        in
        pkgs.runCommand "optimize-and-transform-pngs"
          {
            nativeBuildInputs = with pkgs; [ optipng libjxl libwebp parallel ];
          }
          ''
            set -euo pipefail
            mkdir -p $out
            find ${./assets/images} -name "*.png" \
              | parallel ${escapeShellArgs [
                "--will-cite"
                # NOTE: AVIF files were often bigger, skipping
                # TODO: there is a way to run these in parallel
                ''
                  optipng -o9 {} -dir $out
                  cwebp -lossless {} -o $out/{/.}.webp
                  cjxl --quality=100 --effort=9 {} $out/{/.}.jxl
                ''
              ]}
          '';

      fa_sprite_util = pkgs.callPackage ./tools/fa_sprite_util/default.nix { };

      iconsToKeep = [
        # brands
        "discord"
        "linkedin"
        "medium"
        "reddit"
        "telegram"
        "twitter"

        # solid
        "angle-down"
        "angle-up"
        "arrow-right"
        "magnifying-glass"
        "circle-arrow-right"
      ];

      font-awesome-sprites =
        let
          inherit (pkgs.lib.strings) escapeShellArgs;

          fontAwesomeVersion = "6.2.0";
          fontAwesomeYear = "2022";
          fontAwesomeFlags = escapeShellArgs ([
            "--font-awesome-version=${fontAwesomeVersion}"
            "--font-awesome-year=${fontAwesomeYear}"
          ] ++ builtins.map (i: "--icon=${i}") iconsToKeep);

          scourFlags = escapeShellArgs [
            "--indent=none"
            "--enable-comment-stripping"
          ];
        in
        pkgs.runCommand "get-font-awesome"
          {
            nativeBuildInputs = with pkgs; [ fa_sprite_util parallel scour ];
          }
          ''
            set -euo pipefail
            mkdir -p $out
            filenames=("brands" "solid")
            parallel ${escapeShellArgs [ 
              "--will-cite"
              "fa_sprite_util ${fontAwesomeFlags} < ${self.inputs.font-awesome}/sprites/{}.svg | scour -o \"$out/font-awesome-sprite-{}.svg\" ${scourFlags}"
            ]} \
              ::: ''${filenames[@]}
          '';
    in
    {
      packages = {
        "offchain:dusd-browser" =
          pkgs.runCommand "build-dusd-browser"
            { }
            ''
              set -euo pipefail
              mkdir -p $out/assets/{images,scripts}
              cp ${./netlify.toml} $out/netlify.toml
              cp -r ${dusd-browser}/lib/node_modules/dusd-browser/build/* $out/
              cp -r ${font-awesome-sprites}/*.svg $out/assets/images
              cp -f ${optimized-images}/*.{jxl,png,webp} $out/assets/images
              cp -r ${dusd-ui-components.package}/dist/* $out/assets/scripts
            '';
      };

      apps = {
        "offchain:dusd-browser:serve:testnet" =
          cat-lib.makeServeApp self'.packages."offchain:dusd-browser";
        "offchain:dusd-browser:serve:mainnet" =
          cat-lib.makeServeApp self'.packages."offchain:dusd-browser";
      };

      devShells = {
        "offchain:dusd-browser" =
          offchain-lib.makeProjectShell { project = dusd-ui-components; };
      };

      checks = {
        "dusd-browser:lighthouse" =
          pkgs.callPackage ./nixos/tests/dusd-browser-lighthouse.nix {
            lighthouse =
              (dream2nix.lib.makeOutputs { source = self.inputs.lighthouse-src; }).packages.lighthouse;
            dusd-browser = self'.packages."offchain:dusd-browser";
            # TODO these values need to be increased once the improvements were done
            categories = {
              performance = 0.1;
              accessibility = 0.1;
              seo = 0.1;
              best-practices = 0.1;
            };
          };
      };
    };
  flake = {
    effects = { branch, rev, ... }:
      let
        pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;
        hci-effects = self.inputs.hercules-ci-effects.lib.withPkgs pkgs;
        system = "x86_64-linux";
        mkWebsite = projectName: siteId:
          let
            productionMessage = "${projectName} has been deployed for **production**";
            normalMessage = "${projectName} has been deployed for preview";
            productionMentions = "<@591177767467614238>";
            normalMentions = "<@685818055544012885>";
            message = if (branch == "main") then productionMessage else normalMessage;
            mentions = if (branch == "main") then productionMentions else normalMentions;
            discord-sh = (builtins.getFlake "github:matthewcroughan/nixpkgs/b96b41d2a818c4b997b8e6a647b960a01c7f046c").legacyPackages.${system}.discord-sh;
          in
          hci-effects.netlifyDeploy {
            productionDeployment = (branch == "main");
            content = "${self.packages.${system}."offchain:${projectName}"}";
            secretName = "default-netlify";
            secretField = "authToken";
            siteId = siteId;
            secretsMap."ardanaDiscord" = "ardanaDiscord";
            postEffect = ''
              readSecretString ardanaDiscord .webhook > .webhook
              ${discord-sh}/bin/discord.sh \
                --description "${message}" \
                --field "Deploy URL;$(jq -r '.deploy_url' netlify-result.json)" \
                --field "Branch;${branch}" \
                --field "Commit ID;${rev}" \
                --text "${mentions}"
            '';
          };
      in
      {
        dusd-browser = mkWebsite
          "dusd-browser"
          "8a80e71e-e8e8-4330-96e1-6b435aa1d543";
      };
  };
}
