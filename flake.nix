{

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/362c651cc9af7d40e2f8e4054a58fd209e81d2c3";
    cardano-ogmios.url = "github:input-output-hk/cardano-ogmios";
    mlabs-ogmios.follows = "cardano-transaction-lib/ogmios";
    ogmios-datum-cache.follows = "cardano-transaction-lib/ogmios-datum-cache";
    #   used for libsodium-vrf
    plutus = {
      url = "github:input-output-hk/plutus";
    };
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus";
    };
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npmlock2nix = {
      flake = false;
      url = "github:nix-community/npmlock2nix";
    };
    ps-tools.follows = "purs-nix/ps-tools";
    # ps-0.14 is the branch for Purescript 0.14
    # which we use because ctl uses it
    purs-nix.url = "github:ursi/purs-nix/760ed36cf6c7e90e8f2d4774d5a23c2973abef38";
    lighthouse-src = {
      url = "github:GoogleChrome/lighthouse/v9.5.0";
      flake = false;
    };
    jquery = {
      url = "github:jquery/jquery/3.6.0";
      flake = false;
    };
    font-awesome = {
      url = "github:FortAwesome/Font-Awesome/6.2.0";
      flake = false;
    };
    treefmt-flake.url = "github:srid/treefmt-flake";
    yubihsm.url = "github:ArdanaLabs/yubihsm-ed-sign?rev=6fc4b462fc400cc2058df81f760228c2088db8d4";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

  };

  outputs = { self, flake-parts, treefmt-flake, ... }:
    (flake-parts.lib.evalFlakeModule
      { inherit self; }
      {
        systems = [ "x86_64-linux" ];
        imports = [
          treefmt-flake.flakeModule
          ./offchain
          ./onchain
          ./docs
          ./nix/flake-modules
          ./price-feeder
        ];
      }
    ).config.flake;
}
