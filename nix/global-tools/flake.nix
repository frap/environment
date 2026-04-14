# ~/.config/nix/global-tools/flake.nix
{
  description = "Gas Global tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.${system} = {
        default = pkgs.buildEnv {
          name = "global-tools";
          paths = with pkgs; [
            awscli2
            babashka
            bashInteractive
            bat
            clojure
            devenv
            direnv
            eza
            fd
            gh
            git
            gnupg
            jet
            jq
            ripgrep
            rsync      
          ];
      };

      awscli2 = pkgs.awscli2;
      babashka = pkgs.babashka;
      bashInteractive = pkgs.bashInteractive;
      bat = pkgs.bat;
      clojure = pkgs.clojure;
      devenv =   pkgs.devenv;
      direnv =    pkgs.direnv;
      eza = pkgs.eza;
      fd = pkgs.fd;
      gh = pkgs.gh;
      git = pkgs.git;
      gnupg = pkgs.gnupg;
      jet = pkgs.jet;
      jq = pkgs.jq;
      ripgrep = pkgs.ripgrep;
      rsync = pkgs.rsync; 
    };   
  };
}
