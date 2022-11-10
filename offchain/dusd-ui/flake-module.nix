{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (config) cat-lib dream2nix;
      ui =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.dusd-ui;

      optimized-pngs =
        let
          inherit (pkgs.lib.strings) escapeShellArgs;

          imagesDir = "${ui}/lib/node_modules/dusd-ui/build/assets/images/";
        in
          pkgs.runCommand "optimize-pngs"
            {
              nativeBuildInputs = with pkgs; [ optipng parallel ];
            }
            ''
              set -euo pipefail
              mkdir -p $out

              parallel ${escapeShellArgs [ 
                "--will-cite"
                "optipng -o7 {} -dir \"$out\""
              ]} \
                ::: `find ${imagesDir} -name "*.png"`
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
        dusd-ui =
          pkgs.runCommand "build-dusd-ui"
            { }
            ''
              mkdir -p $out/assets/{images,scripts}
              cp -r ${ui}/lib/node_modules/dusd-ui/build/* $out/
              cp -r ${self'.packages."offchain:dusd-browser"}/dist/* $out/assets/scripts
              cp -r ${font-awesome-sprites}/*.svg $out/assets/images
              cp -f ${optimized-pngs}/*.png $out/assets/images
            '';
      };

      apps = {
        "offchain:dusd-ui:serve:testnet" =
          cat-lib.makeServeApp self'.packages."dusd-ui";
        "offchain:dusd-ui:serve:mainnet" =
          cat-lib.makeServeApp self'.packages."dusd-ui";
      };

      checks = {
        "dusd-ui:lighthouse" =
          pkgs.callPackage ./nixos/tests/dusd-ui-lighthouse.nix {
            lighthouse =
              (dream2nix.lib.makeOutputs { source = self.inputs.lighthouse-src; }).packages.lighthouse;
            dusd-ui = self'.packages."dusd-ui";
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
            content = "${self.packages.${system}.${projectName}}";
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
        dusd-ui = mkWebsite
          "dusd-ui"
          "bf0167e5-b0c2-41b2-9141-3b9f10329c1d";
      };
  };
}
