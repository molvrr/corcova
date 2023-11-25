{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
          (self: super: { ocamlPackages = super.ocaml-ng.ocamlPackages_5_1; })
        ];

        pkgs' = pkgs.pkgsCross.musl64;
      in {
        devShells.default = pkgs'.mkShell {
          nativeBuildInputs = with pkgs'.ocamlPackages; [
            dune_3
            findlib
            ocaml
            ocaml-lsp
            ocamlformat
          ];

          buildInputs = with pkgs'.ocamlPackages; [
            core
            core_unix
            yojson
            ppx_expect
            utop
          ];
        };

        formatter = pkgs.nixfmt;
      });
}
