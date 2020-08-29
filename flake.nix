{
  description = "Results of 'Grafische Elemente & visuelle Effekte'";

  outputs =
    { self
    , nixpkgs
    }: rec {

      defaultPackage.x86_64-linux = packages.x86_64-linux.pdf;

      packages.x86_64-linux = import ./default.nix { inherit nixpkgs; };

      devShell.x86_64-linux = let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
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
        };

    };
}
