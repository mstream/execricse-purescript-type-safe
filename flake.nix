{
  description = "Purescript Type Level Programming Exercise";

  inputs = {
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix/master";
    flake-utils.url = "github:numtide/flake-utils/main";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs =
    { easy-purescript-nix
    , flake-utils
    , nixpkgs
    , ...
    }:
    let
      name = "purescript-type-level";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

    in
    flake-utils.lib.eachSystem supportedSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      easy-ps = import easy-purescript-nix { inherit pkgs; };

      format-check = pkgs.stdenvNoCC.mkDerivation {
        checkPhase = ''
          purs-tidy check {src,test}
        '';
        doCheck = true;
        dontBuild = true;
        installPhase = ''
          mkdir "$out"
        '';
        name = "format-check";
        nativeBuildInputs = with easy-ps; [ purs-tidy ];
        src = ./.;
      };

      devShellInputs = {
        easy-ps = with easy-ps; [
          psa
          purs
          purs-backend-es
          purs-tidy
          spago
        ];

        node-packages = with pkgs.nodePackages; [ ];

        pkgs = with pkgs; [
          act
          docker
          esbuild
          gh
          git
          imagemagick
          mdbook
          nodejs
        ];
      };
    in
    {
      checks = { inherit format-check; };
      devShells.default = pkgs.mkShell {
        inherit name;
        buildInputs =
          devShellInputs.easy-ps ++
          devShellInputs.node-packages ++
          devShellInputs.pkgs;
        PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
        shellHook = ''
          PS1="\[\e[33m\][\[\e[m\]\[\e[34;40m\]${name}:\[\e[m\]\[\e[36m\]\w\[\e[m\]\[\e[33m\]]\[\e[m\]\[\e[32m\]\\$\[\e[m\] "
        '';
      };
      packages = { inherit docs; };
    }
    );
}
