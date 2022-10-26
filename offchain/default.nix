{
  imports = [
    ./flake-module.nix
    ./nix/flake-modules/default.nix
    ./ctl-utils
    ./ctl-utils-test
    ./danaswap-api/flake-module.nix
    ./danaswap-cli
  ];
}
