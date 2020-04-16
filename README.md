# Grafische Element & visuelle Effekte

## Building and running

This project has no non-haskell dependencies, so

```
stack run
```

should be enough to run the project.

If you are using Nix and want to skip the `stack` part, you should be fine with pulling all necessary dependencies via `nix-shell`, then run `runhaskell src/Main.hs`.

## The `cache` dir

After running, the `cache` directory is populated with images. Most of the images are generated only if they can't be found in the cache, so if you modify the source code you might need to delete the files in `cache` before rerunning the program.
