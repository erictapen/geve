{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs { system = "x86_64-linux"; };
in
rec {

  geve = let
    inherit (pkgs.haskell.packages.ghc865) mkDerivation base directory hsnoise svg-builder text;
  in
    mkDerivation {
      pname = "geve";
      version = "0.1.0.0";
      src = builtins.filterSource (path: _: builtins.match ".*\.nix|result.*|flake.lock" path == null) ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        base
        directory
        hsnoise
        svg-builder
        text
      ];
      homepage = "https://erictapen.name/";
      license = pkgs.stdenv.lib.licenses.bsd3;
    };

  pdf = pkgs.runCommand "geve-pdf" {
    buildInputs = with pkgs; [
      inkscape
      pdftk
      geve
    ];
    FONTCONFIG_FILE = pkgs.makeFontsConf {
      fontDirectories = [ pkgs.fira ];
    };
  } ''
    mkdir -p cache $out/pages

    # generate SVG files with the haskell binary
    geve

    # convert every SVG page to a singlepage PDF file
    for svgpage in cache/*.svg; do
      cp $svgpage $out/pages/$(basename $svgpage)
      inkscape --export-pdf="$out/pages/$(basename $svgpage).pdf" "$svgpage"
      inkscape --export-dpi=300 --export-background=ffffff --export-png="$out/pages/$(basename $svgpage).png" "$svgpage"
    done

    # concat all the PDF documents to one final result
    pdftk $out/pages/*.pdf cat output $out/geve.pdf
  '';

}
