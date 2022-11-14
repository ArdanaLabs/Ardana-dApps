{ lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  pname = "fa_sprite_util";
  src = ./.;
  version = "2022-11-03-unstable";
  minimumSupportedOcamlVersion = "4.08";
  useDune2 = true;
  buildInputs = with ocamlPackages; [
    getopt
    lambdasoup
    markup
  ];
}
