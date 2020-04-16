# Grafische Element & visuelle Effekte

## Building and running

This project has no non-haskell dependencies, so

```
stack run
```

should be enough to run the project.

If you are using Nix and want to skip the `stack` part, you should be fine with pulling all necessary dependencies via `nix-shell`, then run `runhaskell src/Main.hs`.
