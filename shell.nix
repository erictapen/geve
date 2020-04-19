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
    librsvg
    inkscape
    ormolu
    python3Packages.numpy
    python3Packages.matplotlib
  ];
}
