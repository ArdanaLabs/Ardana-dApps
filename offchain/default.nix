{
  imports = [
    ./flake-module.nix
    ./nix/flake-modules/default.nix
    ./danaswap-api/flake-module.nix
    ./dusd-api/flake-module.nix
    ./dusd-cli
    ./danaswap-cli
    ./dusd-browser/flake-module.nix
  ];
}
