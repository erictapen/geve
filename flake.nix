{
  description = "Results of 'Grafische Elemente & visuelle Effekte'";

  outputs =
    { self
    , nixpkgs
    }: let
      forAllSystems = f: nixpkgs.lib.genAttrs
        [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system: f system);
      nixpkgsFor = forAllSystems (
        system: import nixpkgs { inherit system; }
      );
    in
      rec {

        defaultPackage = forAllSystems (system: packages.${system}.pdf);

        packages = forAllSystems (
          system: let
            pkgs = nixpkgsFor.${system};
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

              sputnik-auswahl = pkgs.runCommand "sputnik-auswahl-pdf" {
                buildInputs = with pkgs; [ pdftk ];
              } ''
                mkdir -p $out

                pdftk \
                  ${pdf}/pages/page001.svg.pdf \
                  ${pdf}/pages/page004.svg.pdf \
                  ${pdf}/pages/page005.svg.pdf \
                  ${pdf}/pages/page008.svg.pdf \
                  ${pdf}/pages/page009.svg.pdf \
                  ${pdf}/pages/page013.svg.pdf \
                  ${pdf}/pages/page014.svg.pdf \
                  ${pdf}/pages/page015.svg.pdf \
                  ${pdf}/pages/page016.svg.pdf \
                  ${pdf}/pages/page020.svg.pdf \
                  ${pdf}/pages/page021.svg.pdf \
                  ${pdf}/pages/page029.svg.pdf \
                  ${pdf}/pages/page031.svg.pdf \
                  cat output $out/sputnik.pdf
              '';
            }
        );

        devShell = forAllSystems (
          system: let
            pkgs = nixpkgsFor.${system};
          in
            pkgs.mkShell {
              buildInputs = with pkgs; [
                (
                  pkgs.haskellPackages.ghcWithPackages (
                    p: with p; [
                      diagrams
                      SVGFonts
                      svg-builder
                      JuicyPixels
                      either-unwrap
                      hsnoise
                      Noise
                    ]
                  )
                )
                stack
                librsvg # convert SVG to PDF files
                inkscape
                ormolu # haskell code formatting
                pdftk
                python3Packages.numpy
                python3Packages.matplotlib
              ];
            }
        );

      };
}
