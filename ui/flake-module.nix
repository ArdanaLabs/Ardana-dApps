{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (config) dream2nix;
      ui =
        (dream2nix.lib.makeOutputs { source = ./.; settings = [{ subsystemInfo.nodejs = 16; }]; }).packages.danaswap-ui;
    in
    {
      packages = {
        danaswap-ui =
          pkgs.runCommand "build-danaswap-ui"
            { buildInputs = [ self'.packages."offchain:danaswap-browser" ]; }
            ''
              mkdir -p $out/build/assets/scripts
              cp -r ${ui}/lib/node_modules/danaswap-ui/build $out
              cp -r ${self'.packages."offchain:danaswap-browser"}/dist/* $out/build/assets/scripts
            '';
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
            content = "${self.packages.${system}.${projectName}}/lib/node_modules/${projectName}/build";
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
