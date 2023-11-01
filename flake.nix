{
  description = "ppmz";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        with nixpkgs.legacyPackages.${system};
        let
          ghc =
            (haskell.packages.ghc884.override {
              all-cabal-hashes = fetchurl {
                url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/12206435934e53eeda06b73e93bdee29a0ff713b.tar.gz";
                sha256 = "0d3xis5mli5yskvyxiqr3z7gzvnnm22zrm6dlcpbj4mdqrp2rh10";
              };
            }).extend (self: super: {
              pomaps = self.callHackage "pomaps" "0.2.0.1" {};
            });

          cabal = ghc.callPackage ./cabal.nix {};
        in
        {
          packages.default = cabal;

          devShells.default =
            mkShell {
              inputsFrom = [ cabal.env ];
              buildInputs = [
                cabal-install
                ghc.haskell-language-server
                ghc.fourmolu
                ws
              ];
            };
        }
      );
}
