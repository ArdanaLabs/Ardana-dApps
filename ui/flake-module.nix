{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (config) cat-lib dream2nix;
      ui =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.danaswap-ui;
    in
    {
      packages = {
        danaswap-ui =
          pkgs.runCommand "build-danaswap-ui"
            { }
            ''
              mkdir -p $out/assets/scripts
              cp -r ${ui}/lib/node_modules/danaswap-ui/build/* $out/
              cp -r ${self'.packages."offchain:danaswap-browser"}/dist/* $out/assets/scripts
            '';
      };

      apps = {
        "danaswap-ui:serve" =
          cat-lib.makeServeApp self'.packages."danaswap-ui";
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
