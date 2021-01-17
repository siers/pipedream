with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    base
    containers
    extra
    lens
    network
    primitive
    text
    transformers
    unordered-containers
    vector
    vector-th-unbox
    websockets
  ]);
in
  mkShell {
    buildInputs = [
      ghc
      haskell.packages.ghc884.haskell-language-server
      ws
      cabal-install
    ];
  }
