# ~/.config/nix/global-tools/flake.nix
{
  description = "Global CLI tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.${system}.default = pkgs.buildEnv {
        name = "global-tools";
        paths = [
          pkgs.awscli2
          pkgs.babashka
          pkgs.bashInteractive
          pkgs.bat
          pkgs.devenv
          pkgs.direnv
          pkgs.eza
          pkgs.fd
          pkgs.gh
          pkgs.git
          pkgs.gnupg
          pkgs.jet
          pkgs.jq
          pkgs.ripgrep         
        ];
      };
    };
}
