with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc865.ghcWithPackages (packages: with packages; [
    extra
    matrix
    parallel
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
