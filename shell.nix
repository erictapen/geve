with import <nixpkgs> {};

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
