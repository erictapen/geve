# Grafische Element & visuelle Effekte

## Building the book

This book is generated from source using the [Nix](https://nixos.org/nix) tool. If you have Nix installed, you can generate the book either as a normal Nix derivation or as an exerimental [Nix flake](https://github.com/NixOS/rfcs/pull/49). Both methodes have been tested against the `nixos-20.03` branch of [Nixpkgs](https://github.com/NixOS/nixpkgs/).

### Buiding the normal Nix way

If you have Nix installed, running `nix build` should be enough to generate the final PDF file. It can then be found in the `result-1` symlink.

### Building as a Nix flake

If you use Nix with flake support, you can also build this project using `nix build`. The final PDF file can be found behind the `result` symlink.

## Hacking

If you want to hack on the project, you might want to use the [Stack](https://docs.haskellstack.org/en/stable/README/) build tool. Executing `stack run` should be enough to generate the svg files in the `cache` dir.

If you are using Nix and want to skip the `stack` part, you should be fine with pulling all necessary dependencies via `nix-shell`, then run `runhaskell src/Main.hs`.

### The `cache` dir

After running, the `cache` directory is populated with images. Most of the images are generated only if they can't be found in the cache, so if you modify the source code you might need to delete the files in `cache` before rerunning the program.

### Code format

The code is formatted with [ormolu](https://github.com/tweag/ormolu). To format every Haskell source file in the repository, you can run `ormolu --mode inplace $(find src/ -name '*.hs')`.
