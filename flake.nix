{
  description = "Results of 'Grafische Elemente & visuelle Effekte'";

  outputs =
    { self
    , nixpkgs
    }: rec {
      defaultPackage.x86_64-linux = packages.x86_64-linux.pdf;
      packages.x86_64-linux = import ./default.nix { inherit nixpkgs; };
    };
}
