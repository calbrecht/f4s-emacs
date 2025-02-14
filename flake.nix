{
  description = "Emacs setup flake.";

  nixConfig = {
    flake-registry = "https://github.com/calbrecht/f4s-registry/raw/main/flake-registry.json";
  };

  inputs = {
    emacs-overlay.url = "flake:emacs-overlay";
    fixups.url = "flake:f4s-fixups";
    flake-parts.url = "flake:flake-parts";
    init-leafs.flake = false;
    init-leafs.url = "path:/home/alab/.emacs.i/init-leafs.el";
    nixpkgs.url = "flake:nixpkgs";
    nodejs.url = "flake:f4s-nodejs";
    nil.url = "github:oxalica/nil/70df371289962554cf7a23ed595b23a2ce271960";
    rust.url = "flake:f4s-rust";
    systems.url = "github:nix-systems/x86_64-linux";
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
      inputs.nil.overlays.nil
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
      #tree-sitter = (prev.tree-sitter.overrideAttrs (old: {
      #  postPatch = (old.postPatch or "") + ''
      #    #${prev.tree}/bin/tree .
      #    substituteInPlace cli/src/generate/templates/build.rs --replace \
      #      ".include(&src_dir);" ".include(&src_dir).include(\"${libclangIncludes}\");"
      #
      #    substituteInPlace cli/loader/src/lib.rs --replace \
      #      ".host(BUILD_TARGET);" ".host(BUILD_TARGET).include(\"${libcxxIncludes}\");"
      #  '';
      #}));

      irony-server = (prev.irony-server.override {
        irony = final.emacsPackages.melpaPackages.irony;
      });

      emacsNodePackages = attrValues {
        inherit (if useLatestNodeJS then prev.nodePackages_latest else prev.nodePackages)
        eslint eslint_d
        #import-js
        jsonlint prettier standardx tslint trepan-ni
        typescript typescript-language-server bash-language-server intelephense
        yaml-language-server;
      };

      emacsExtraPathPackages = with final; [
        git
        nix
        wl-clipboard
        nixfmt-rfc-style
        #TODO crate2nix
        diffutils
        fd
        fzf
        irony-server
        jsonnet-language-server
        llvmPackages.bintools
        llvmPackages.clang
        nil
        nixpkgs-fmt
        pkg-config
        ripgrep
        shellcheck
        stdenv.cc.bintools.bintools_bin
        #tree-sitter
        perlPackages.AnyEvent
        perlPackages.ClassAccessorFast
        perlPackages.DBDMariaDB
        perlPackages.DBDPg
        perlPackages.DBDSQLite
        perlPackages.DBI
        perlPackages.DataSExpression
        perlPackages.RPCEPCService
        (php81.override {
          packageOverrides = final: prev: {
            extensions = prev.extensions // {
              fileinfo = prev.extensions.fileinfo.overrideAttrs (_: { doCheck = false; });
              simplexml = prev.extensions.simplexml.overrideAttrs (_: { doCheck = false; });
              dom = prev.extensions.dom.overrideAttrs (_: { doCheck = false; });
            };
          };
        })
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
        (optionalOverrideAttrs "rg" (old: {
          ignoreCompilationError = true;
        }))
        #(optionalOverrideAttrs "" (old: {
        #  postPatch = ''
        #    #${prev.tree}/bin/tree $src
        #    substituteInPlace .el --replace \
        #    "'create-solarized-theme-file 'solarized-create-theme-file)" \
        #    "'create-solarized-theme-file 'solarized-create-theme-file \"0\")"
        #  '' + (old.postPatch or "");
        #}))
      ];

      emacsPackages = (prev.emacsPackagesFor prev.emacs-git-nox)
        .overrideScope (eself: esuper:
        let
          blockAll = [ "docker" "aio" ];
          blockElpa = [ ];
          blockNongnu = [ "solarized-theme" ];
          blockMelpa = [ "php-mode" ];
          overridePkgs = emacsPkgs: block:
          filterAttrs (n: _: !(elem n block) && !(elem n blockAll))
            (final.emacsPackagesOverride emacsPkgs);
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
          mu4e
          #tsc for tree-sitter
          # lives in ~/.emacs.d/git now
          #tree-sitter-langs
          #tree-sitter
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
