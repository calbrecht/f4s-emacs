{
  description = "Emacs setup flake.";

  nixConfig = {
    flake-registry = https://github.com/calbrecht/f4s-registry/raw/main/flake-registry.json;
  };

  inputs = {
    emacs-overlay.url = flake:emacs-overlay;
    fixups.url = flake:f4s-fixups;
    flake-parts.url = flake:flake-parts;
    init-leafs.flake = false;
    init-leafs.url = path:/home/alab/.emacs.i/init-leafs.el;
    nixpkgs.url = flake:nixpkgs;
    nodejs.url = flake:f4s-nodejs;
    rnix-lsp.url = github:nix-community/rnix-lsp/ff18e04551a39ccdab0ff9c83926db3807b23478;
    rust.url = flake:f4s-rust;
    systems.url = github:nix-systems/x86_64-linux;
  };

  outputs = inputs: let
    useLatestNodeJS = true;
    inherit (builtins)
      elem
      hasAttr
      readFile
    ;
    inherit (inputs.nixpkgs.lib)
      attrValues
      extends
      filterAttrs
      flip
      foldl
      foldl'
      getDev
      getLib
      getVersion
      makeBinPath
      optionalAttrs
      recursiveUpdate
    ;
  in inputs.flake-parts.lib.mkFlake { inherit inputs; } (top: {
    systems = (import inputs.systems);
    flake.overlays.default = final: prev: foldl' (flip extends) (_: prev) [
      inputs.emacs-overlay.overlay
      inputs.nodejs.overlay
      inputs.rust.overlay
      top.config.flake.overlays.emacs
    ] final;
    flake.overlays.emacs = final: prev:
    let
      inherit (prev) llvmPackages;
      libclangLib = "${getLib llvmPackages.libclang}/lib";
      libclangIncludes = "${libclangLib}/clang/${getVersion llvmPackages.clang}/include";
      libcxxIncludes = "${getDev llvmPackages.libcxx}/include/c++/v1";
    in
    {
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

      emacsNodePackages = attrValues {
        inherit (if useLatestNodeJS then prev.nodePackages_latest else prev.nodePackages)
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
          optionalAttrs (hasAttr name emacsPackages) {
            "${name}" = emacsPackages."${name}".overrideAttrs fn;
          };
      in foldl recursiveUpdate {} [
        emacsPackages
        (optionalOverrideAttrs "tsc" (old: {
          postPatch = (old.postPatch or "") + ''
            substituteInPlace core/tsc-dyn-get.el --replace \
            "tsc-dyn-dir tsc--dir" "tsc-dyn-dir \"/ws/emacs-tree-sitter/result/lib\""
          '';
        }))
        (optionalOverrideAttrs "irony" (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            sed -i '/define-minor-mode/,/:group/ { s/nil/:init-value nil/ ; s/irony-lighter/:lighter irony-lighter/ ; s/irony-mode-map/:keymap irony-mode-map/ }' irony.el
          '' + (old.postPatch or "");
          doCheck = false;
        }))
      ];

      emacsPackages = (prev.emacsPackagesFor prev.emacs-git-nox)
        .overrideScope' (eself: esuper:
        let
          blockElpa = [ ];
          blockNongnu = [ "solarized-theme" ];
          blockMelpa = [ "php-mode" ];
          overridePkgs = emacsPkgs: block:
            filterAttrs (n: _: !(elem n block)) (final.emacsPackagesOverride emacsPkgs);
          elpaPackages = overridePkgs esuper.elpaPackages blockElpa;
          nongnuPackages = overridePkgs esuper.nongnuPackages blockNongnu;
          melpaPackages = overridePkgs esuper.melpaPackages blockMelpa;
        in foldl recursiveUpdate {} [
          elpaPackages { inherit elpaPackages; }
          nongnuPackages { inherit nongnuPackages; }
          melpaPackages { inherit melpaPackages; }
        ]
      );

      emacsGitNoxWithPackages = (final.emacsWithPackagesFromUsePackage {
        config = readFile inputs.init-leafs.outPath;
        package = prev.emacs-git-nox;
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
            --prefix PATH : $out/bin:${makeBinPath emacsNodePackages}
        '';
      };
    };
    perSystem = { config, system, pkgs, lib, ... }: {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.fixups.overlays.default
          top.config.flake.overlays.default
        ];
      };
      packages = {
        inherit (pkgs) emacs-git-ide;
      };
      legacyPackages = pkgs;
    };
  });
}
