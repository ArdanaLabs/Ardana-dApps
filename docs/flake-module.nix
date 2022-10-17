{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;

      diagram = pkgs.runCommand "diagram"
      { buildInputs = [ pkgs.xdot ]; }
      ''
        mkdir $out
        dot ${./diagram.dot} -Tpng > $out/diagram.png
      ''
      ;
    in
    {
      packages = {
        docs = pkgs.stdenv.mkDerivation {
          name = "build-docs";
          src = ./.;

          buildInputs = with pkgs; [
            (texlive.combine {
              inherit
                (texlive)
                scheme-basic
                latexmk
                todonotes
                metafont;
            })
          ];

          buildPhase = ''
            cp ${diagram}/diagram.png .
            HOME=$TMP latexmk -output-directory="tmp" -pdf ./*.tex
          '';
          doCheck = false;
          installPhase = ''
            mkdir -p $out
            cp tmp/*.pdf $out
          '';
        };
      };
      apps = {
        "docs:feedback-loop" = {
          type = "app";
          program =
            pkgs.writeShellApplication
              {
                name = "feedback-loop";
                runtimeInputs = [ pkgs.entr ];
                text = ''
                  find docs -name "*.tex" | entr nix build .#docs
                '';
              };
        };
      };
    };
  flake = { };
}
