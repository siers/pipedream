with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc865.ghcWithPackages (packages: with packages; [
    matrices
    matrix
  ]);
in
  mkShell {
    buildInputs = [
      ghc
    ];
  }
