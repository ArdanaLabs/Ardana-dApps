{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (config) cat-lib dream2nix;
      ui =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.danaswap-ui;
      ui-dev = (dream2nix.lib.makeOutputs {
        source = ./.;
        settings = [{ subsystemInfo.nodejs = 16; }];
        packageOverrides = {
          danaswap-ui = {
            build = {
              buildScript = ''npm run build-dev'';
            };
          };
        };
      }).packages.danaswap-ui;
      font-awesome-sprites =
        # TODO: figure out which icons we _need_, and strip the rest as almost
        # all of these icons are unused dead weight
        # NOTE: "--enable-comment-stripping" can’t be added due to licensing
        # (CC BY 4.0 requires attribution). To enable, the license data
        # should be moved to the proper metadata elements and namespaces (I’m
        # unsure why Font Awesome didn’t do this to begin with)
        pkgs.runCommand "get-font-awesome"
          {
            nativeBuildInputs = with pkgs; [ parallel scour ];
          }
          ''
            set -euo pipefail
            mkdir -p $out
            filenames=("brands" "solid")
            parallel scour \
              -i ${self.inputs.font-awesome}/sprites/{}.svg \
              -o "$out/font-awesome-sprite-{}.svg" \
              --indent=none \
              ::: ''${filenames[@]}
          '';
    in
    {
      packages = {
        danaswap-ui =
          pkgs.runCommand "build-danaswap-ui"
            { }
            ''
              mkdir -p $out/assets/{images,scripts}
              cp -r ${ui}/lib/node_modules/danaswap-ui/build/* $out/
              cp -r ${self'.packages."offchain:danaswap-browser"}/dist/* $out/assets/scripts
              cp -r ${font-awesome-sprites}/*.svg $out/assets/images
            '';
        danaswap-ui-dev =
          pkgs.runCommand "build-danaswap-ui-dev"
            { }
            ''
              mkdir -p $out/assets/{images,scripts}
              cp -r ${ui-dev}/lib/node_modules/danaswap-ui/build/* $out/
              cp -r ${self'.packages."offchain:danaswap-browser"}/dist/* $out/assets/scripts
              cp -r ${font-awesome-sprites}/*.svg $out/assets/images
            '';
      };

      apps = {
        "offchain:danaswap-ui:serve:testnet" =
          cat-lib.makeServeApp self'.packages."danaswap-ui-dev";
        "offchain:danaswap-ui:serve:mainnet" =
          cat-lib.makeServeApp self'.packages."danaswap-ui-dev";
      };

      checks = {
        "danaswap-ui:lighthouse" =
          pkgs.callPackage ./nixos/tests/danaswap-ui-lighthouse.nix {
            lighthouse =
              (dream2nix.lib.makeOutputs { source = self.inputs.lighthouse-src; }).packages.lighthouse;
            danaswap-ui = self'.packages."danaswap-ui";
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
        danaswap-ui = mkWebsite
          "danaswap-ui"
          "a7219476-6e9c-438b-b43c-4b858051ecce";
      };
  };
}
