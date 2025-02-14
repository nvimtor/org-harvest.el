{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    agenix-shell = {
      url = "github:nvimtor/agenix-shell/darwin-support-and-runtime-shell-pkgs";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, agenix-shell, self, ... }: flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      agenix-shell.flakeModules.default
    ];

    agenix-shell = {
      secrets = {
        HARVEST_PAT = {
          file = ./harvest.age;
        };

        HARVEST_ACCOUNT_ID = {
          file = ./harvest-id.age;
        };
      };
    };

    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    perSystem = { lib, config, system, ... }: let
      inherit (lib) getExe;

      overlays = [];

      pkgs = import nixpkgs {
        inherit system overlays;
      };
    in {
      _module.args.pkgs = pkgs;

      agenix-shell = {
        agePackage = pkgs.age;
      };

      devShells = {
        default = pkgs.mkShell {
          shellHook = ''
            source ${getExe config.agenix-shell.installationScript}
          '';

          buildInputs = with pkgs; [
            (python3.withPackages (ps: [
              ps.requests
            ]))
            age
            pyright
          ];
        };
      };
    };
  };
}
