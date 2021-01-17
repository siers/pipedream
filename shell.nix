with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    base
    containers
    extra
    lens
    matrix
    network
    primitive
    text
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
