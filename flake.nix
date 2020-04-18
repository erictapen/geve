{
  description = "Results of 'Grafische Elemente & visuelle Effekte'";

  outputs =
    { self,
      nixpkgs }: rec {
        defaultPackage.x86_64-linux = let
	    inherit (nixpkgs.pkgs.haskell.packages.ghc865) mkDerivation;
          in mkDerivation {
            pname = "geve";
            version = "0.1.0.0";
            src = ./.;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = [
              base directory hsnoise svg-builder text
            ];
            homepage = "https://erictapen.name/";
            license = stdenv.lib.licenses.bsd3;
          };
    };
