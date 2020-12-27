with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc865.ghcWithPackages (packages: with packages; [
    matrix
    tuple
    unordered-containers
  ]);
in
  mkShell {
    buildInputs = [
      ghc
      ws
    ];
  }
