{ pure ? false, source-repo-override ? { } }:
let
  packages = import ./. { inherit source-repo-override; };
  inherit (packages) pkgs blockcarbon;
  inherit (blockcarbon) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with blockcarbon; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
