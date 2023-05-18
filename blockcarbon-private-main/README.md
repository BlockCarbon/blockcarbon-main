# BlockCarbon Open Source Repository

## Setting up

Clone the repository to your local environment
```git clone https://github.com/BlockCarbon/blockcarbon-private-main.git ```


### Cabal+Nix build

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

Go into the Blockcarbon directory use `nix-shell` to get a bash shell:

```
$ nix-shell
```

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.


## The Plutus Application Backend (PAB) simulator example

1. Build the PAB executable:

```
cabal build blockcarbon-pab
```

2. Run the PAB binary:

```
cabal exec -- blockcarbon-pab
````

## Support/Issues/Community

Issues can be filed in the TBD

For more interactive discussion, you can join the TBD

Thanks!
