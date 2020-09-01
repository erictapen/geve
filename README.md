# Grafische Element & visuelle Effekte

## Building the book

This book is generated from source using the [Nix](https://nixos.org/nix) tool. If you have Nix installed, you can generate the book either as a normal Nix derivation or as an exerimental [Nix flake](https://github.com/NixOS/rfcs/pull/49). Both methodes have been tested against the `nixos-20.03` branch of [Nixpkgs](https://github.com/NixOS/nixpkgs/).

### Building as a Nix flake

If you use Nix with flake support, you can build this project using `nix build github:erictapen/geve#pdf`. The final PDF file can be found behind the `result` symlink.

## Hacking

If you want to hack on the project, you might want to use the [Stack](https://docs.haskellstack.org/en/stable/README/) build tool. Executing `stack run` should be enough to generate the svg files in the `cache` dir.

If you are using Nix (with experimental flakes support enabled) and want to skip the `stack` part, you should be fine with pulling all necessary dependencies via `nix develop`, then run `runhaskell src/Main.hs`.

### The `cache` dir

After running, the `cache` directory is populated with images. Most of the images are generated only if they can't be found in the cache, so if you modify the source code you might need to delete the files in `cache` before rerunning the program.

### Code formatting

The code is formatted with [ormolu](https://github.com/tweag/ormolu). To format every Haskell source file in the repository, you can run `ormolu --mode inplace $(find src/ -name '*.hs')`.
