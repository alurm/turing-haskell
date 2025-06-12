{
  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.developPackage {
        root = ./.;
      };
    in {
      packages.default = package;
      devShells.default = pkgs.haskellPackages.shellFor {
        packages = hpkgs: [package];
        buildInputs = [
          pkgs.cabal-install
          pkgs.haskell-language-server
          (pkgs.vscode-with-extensions.override {
            vscode = pkgs.vscodium;
            vscodeExtensions = let
              e = pkgs.vscode-extensions;
            in [
              e.mkhl.direnv
              e.haskell.haskell
              e.justusadam.language-haskell
            ];
          })
        ];
      };
    });
}
