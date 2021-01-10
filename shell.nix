with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    base
    containers
    extra
    matrix
    network
    text
    unordered-containers
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
