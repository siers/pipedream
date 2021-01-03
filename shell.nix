with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    base
    matrix
    containers
    unordered-containers
    hashable
    extra
    tuple
    parallel
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
