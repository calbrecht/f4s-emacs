{
  description = "Update node-packages.";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";

      nix_bin = pkgs.nixFlakes + /bin/nix;
      git_bin = pkgs.git + /bin/git;
    in
    {
      packages."${system}" = {
        commit-and-push = pkgs.writeScriptBin "commit-and-push" ''
          #!${pkgs.stdenv.shell}
          set -xeu

          user_name=''${git_user_name:-$(${git_bin} config user.name)}
          user_mail=''${git_user_mail:-$(${git_bin} config user.email)}

          ${git_bin} config user.name "$user_name"
          ${git_bin} config user.email "$user_mail"

          ${nix_bin} run github:calbrecht/f4s-update#repo-inputs
          ${git_bin} push >&2
        '';

        auto-update = pkgs.writeScriptBin "auto-update" ''
          #!${pkgs.stdenv.shell}
          set -xeu

          . ${self.packages."${system}".commit-and-push}/bin/commit-and-push || \
          true
        '';
      };
    };
}
