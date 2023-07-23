{
  description = "Emacs setup flake.";

  nixConfig = {
    flake-registry = https://github.com/calbrecht/f4s-registry/raw/main/flake-registry.json;
  };

  inputs = {
    systems.url = github:nix-systems/x86_64-linux;
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.systems.follows = "systems";
    };
    emacs-overlay = {
      url = github:nix-community/emacs-overlay;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    init-leafs = {
      url = path:/home/alab/.emacs.i/init-leafs.el;
      flake = false;
    };
    nodejs = {
      url = flake:f4s-nodejs;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fixups = {
      url = flake:f4s-fixups;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust = {
      url = flake:f4s-rust;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rnix-lsp = {
      url = github:nix-community/rnix-lsp/ff18e04551a39ccdab0ff9c83926db3807b23478;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.fixups.overlays.default
          self.overlays.default
        ];
      };
    in {
      packages = {
        inherit (pkgs) emacs-git-ide;
      };
    }) // {
      overlays = let
        useLatestNodeJS = true;
      in {
        default = self.overlays.emacs;
        emacs = final: prev:
        let
          libclangLib = with final; "${lib.getLib llvmPackages.libclang}/lib";
          libclangIncludes = with final; "${libclangLib}/clang/${lib.getVersion llvmPackages.clang}/include";
          libcxxIncludes = with final; "${lib.getDev llvmPackages.libcxx}/include/c++/v1";
        in
        {
          emacs-overlay = (prev.emacs-overlay or { }) //
            (inputs.emacs-overlay.overlay final prev);

          inherit (final.emacs-overlay)
             emacs-git emacsPackagesFor emacsWithPackagesFromUsePackage;

          nodePackages = (prev.nodePackages or { }) //
            (inputs.nodejs.overlay final prev).nodePackages;

          nodePackages_latest = (prev.nodePackages_latest or { }) //
            (inputs.nodejs.overlay final prev).nodePackages_latest;

          rustStable = (prev.rustStable or { }) //
            (inputs.rust.overlay final prev).rustStable;

          tree-sitter = (prev.tree-sitter.overrideAttrs (old: {
            postPatch = (old.postPatch or "") + ''
              #${prev.tree}/bin/tree .
              substituteInPlace cli/src/generate/templates/build.rs --replace \
                ".include(&src_dir);" ".include(&src_dir).include(\"${libclangIncludes}\");"

              substituteInPlace cli/loader/src/lib.rs --replace \
                ".host(BUILD_TARGET);" ".host(BUILD_TARGET).include(\"${libcxxIncludes}\");"
            '';
          }));

          irony-server = (prev.irony-server.override {
            irony = final.emacsPackages.melpaPackages.irony;
          });

          emacs-git-nox = ((final.emacs-git.override {
            withX = false;
            withGTK2 = false;
            withGTK3 = false;
          }).overrideAttrs (oa: {
            name = "${oa.name}-nox";
          }));

          emacsNodePackages = prev.lib.attrValues {
            inherit (if useLatestNodeJS then final.nodePackages_latest else final.nodePackages)
              eslint eslint_d import-js jsonlint prettier standardx tslint trepan-ni
              typescript typescript-language-server bash-language-server intelephense
              yaml-language-server;
          };

          emacsExtraPathPackages = with final; [
            #TODO crate2nix
            #inputs.rnix-lsp.defaultPackage."${system}"
            nixpkgs-fmt
            fd
            jsonnet-language-server
            fzf
            ripgrep
            shellcheck
            stdenv.cc.bintools.bintools_bin
            diffutils
            llvmPackages.clang
            llvmPackages.bintools
            pkg-config
            tree-sitter
            irony-server
            (php81.override {
              packageOverrides = final: prev: {
                extensions = prev.extensions // {
                  fileinfo = prev.extensions.fileinfo.overrideAttrs (old: {
                    doCheck = false;
                  });
                };
              };
            })
            perlPackages.AnyEvent
            perlPackages.ClassAccessorFast
            perlPackages.DataSExpression
            perlPackages.DBI
            perlPackages.DBDSQLite
            perlPackages.DBDMariaDB
            perlPackages.DBDPg
            perlPackages.RPCEPCService
          ];

          emacsPackagesOverride = emacsPackages: let
            optionalOverrideAttrs = name: fn:
              prev.lib.optionalAttrs (builtins.hasAttr name emacsPackages) {
                "${name}" = emacsPackages."${name}".overrideAttrs fn;
              };
          in
            emacsPackages
            // optionalOverrideAttrs "tsc" (old: {
              postPatch = (old.postPatch or "") + ''
                substituteInPlace core/tsc-dyn-get.el --replace \
                "tsc-dyn-dir tsc--dir" "tsc-dyn-dir \"/ws/emacs-tree-sitter/result/lib\""
              '';
            })
            // optionalOverrideAttrs "irony" (old: {
              postPatch = ''
                #${prev.tree}/bin/tree $src
                sed -i '/define-minor-mode/,/:group/ { s/nil/:init-value nil/ ; s/irony-lighter/:lighter irony-lighter/ ; s/irony-mode-map/:keymap irony-mode-map/ }' irony.el
              '' + (old.postPatch or "");
              doCheck = false;
            })
            # lives in ~/.emacs.d/git now
            #tree-sitter-langs = emacsPackages.tree-sitter-langs.overrideAttrs (old: let
            #  grammars = (pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars));
            #in {
            #  propagatedNativeBuildInputs = (old.propagatedNativeBuildInputs or []) ++ [
            #    grammars
            #  ];
            #  #postPatch = (old.postPatch or "") + ''
            #  #  substituteInPlace tree-sitter-langs.el --replace \
            #  #  "(defvar tree-sitter-langs--testing)" \
            #  #  "(defvar tree-sitter-langs--testing) (setq tree-sitter-langs--testing t)"
            #  #
            #  #  substituteInPlace tree-sitter-langs-build.el --replace \
            #  #    "(concat (file-name-as-directory tree-sitter-langs-grammar-dir) \"bin/\")" \
            #  #    "\"${grammars}/\""
            #  #'';
            #});
          ;

          emacsPackages = (final.emacsPackagesFor final.emacs-git-nox)
            .overrideScope' (
              eself: esuper:
              let
                blockElpa = [ ];
                blockNongnu = [ "solarized-theme" ];
                blockMelpa = [ "php-mode" ];
                overridePkgs = pkgs: block: prev.lib.filterAttrs
                                              (n: v: !(builtins.elem n block))
				                                      (final.emacsPackagesOverride pkgs);
                elpaPackages = overridePkgs esuper.elpaPackages blockElpa;
                nongnuPackages = overridePkgs esuper.nongnuPackages blockNongnu;
                melpaPackages = overridePkgs esuper.melpaPackages blockMelpa;
              in
              elpaPackages // { inherit elpaPackages; }
              // nongnuPackages // { inherit nongnuPackages; }
              // melpaPackages // { inherit melpaPackages; }
          );

          emacsGitNoxWithPackages = (final.emacsWithPackagesFromUsePackage {
            config = builtins.readFile inputs.init-leafs.outPath;
            package = final.emacs-git-nox;
            alwaysEnsure = true;

            extraEmacsPackages = epkgs: with epkgs; [
              # meh, this break doom-modeline
              #all-the-icons
              tsc
              # lives in ~/.emacs.d/git now
              #tree-sitter-langs
              tree-sitter
            ];

            override = _: final.emacsPackages;
          });

          emacsGitLoadPath = prev.writeText "eval-when-compile-load-path.el" ''
            (eval-when-compile
              (let ((default-directory "${final.emacsGitNoxWithPackages.deps.outPath}/share/emacs/site-lisp"))
                (normal-top-level-add-subdirs-to-load-path)))
          '';

          emacs-git-ide = with final; prev.symlinkJoin {
            name = "emacs";
            paths = [
              emacsGitNoxWithPackages
              (if useLatestNodeJS then nodejs_latest else nodejs)
              rustStable.rust
            ]
            ++ emacsExtraPathPackages;
            buildInputs = [
              prev.makeWrapper
              rustStable.rust-src
            ];
            postBuild = ''
              unlink $out/share/emacs
              mkdir -p $out/share/emacs/site-lisp
              cp ${emacsGitLoadPath} $out/share/emacs/site-lisp/eval-when-compile-load-path.el
              wrapProgram $out/bin/emacs \
                --set LIBCLANG_PATH "${libclangLib}" \
                --set BINDGEN_EXTRA_CLANG_ARGS "-isystem ${libclangIncludes}" \
                --set RUST_SRC_PATH "${rustStable.rust-src}/lib/rustlib/src/rust/library" \
                --prefix PERL5LIB : "$out/lib/perl5/site_perl/5.34.0:$out/lib/perl5/site_perl/5.34.0/x86_64-linux-thread-multi" \
                --prefix PATH : $out/bin:${prev.lib.makeBinPath emacsNodePackages}
            '';
          };
        };
      };
    };
}
