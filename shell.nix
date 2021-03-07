with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    base
    containers
    extra
    hip
    lens
    network
    primitive
    text
    transformers
    unordered-containers
    vector
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
