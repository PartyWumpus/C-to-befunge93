{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";

    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };
    in
      with pkgs; {
        formatter = pkgs.alejandra;
        devShells.default = mkShell {
          packages = [
            (rust-bin.selectLatestNightlyWith (toolchain:
              toolchain.default.override {
                #extensions = [ "rustc-codegen-cranelift-preview" ];
              }))
            gcc13
            mold
            python3
          ];
        };
      });
}
