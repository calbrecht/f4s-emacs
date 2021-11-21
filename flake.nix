{
  description = "Emacs setup flake.";

  inputs = {
    emacs-overlay = { url = github:nix-community/emacs-overlay; };
    init-leafs = { url = path:/home/alab/.emacs.i/init-leafs.el; flake = false; };
    nodejs = { url = github:calbrecht/f4s-nodejs; inputs.nixpkgs.follows = "nixpkgs"; };
    f4s = { url = github:calbrecht/f4s; };
    rust = { url = github:calbrecht/f4s-rust; };
  };

  outputs = { self, nixpkgs, ... }@inputs:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ 
        inputs.f4s.overlays.fixups
        self.overlay
      ];
    };
    useLatestNodeJS = true;
  in
  {
    defaultPackage."${system}" = pkgs.emacs28-git-ide;

    legacyPackages."${system}" = pkgs;

    overlay = final: prev: let
      libclangLib = with final; "${lib.getLib llvmPackages.libclang}/lib";
      libclangIncludes = with final; "${libclangLib}/clang/${lib.getVersion llvmPackages.clang}/include";
      libcxxIncludes = with final; "${lib.getDev llvmPackages.libcxx}/include/c++/v1";
    in {
      emacs-overlay = (prev.emacs-overlay or { }) //
        (inputs.emacs-overlay.overlay final prev);

      nodePackages = (prev.nodePackages or { }) //
        (inputs.nodejs.overlay final prev).nodePackages;

      nodePackages_latest = (prev.nodePackages_latest or { }) //
        (inputs.nodejs.overlay final prev).nodePackages_latest;

      rustNightly = (prev.rustNightly or { }) //
       (inputs.rust.overlay final prev).rustNightly;

      emacsGcc-nox = ((final.emacs-overlay.emacsGcc.override {
        withX = false;
        withGTK2 = false;
        withGTK3 = false;
      }).overrideAttrs ( oa: {
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
        #TODO rnix-lsp
        nixpkgs-fmt
        fd
        fzf
        stdenv.cc.bintools.bintools_bin
        diffutils
        llvmPackages.clang
        llvmPackages.bintools
        pkg-config
        (tree-sitter.overrideAttrs (old: {
          postPatch = (old.postPatch or "") + ''
            #${prev.tree}/bin/tree .
            substituteInPlace cli/src/generate/templates/build.rs --replace \
              ".include(&src_dir);" ".include(&src_dir).include(\"${libclangIncludes}\");"

            substituteInPlace cli/loader/src/lib.rs \
              --replace \
              ".host(BUILD_TARGET);" ".host(BUILD_TARGET).include(\"${libcxxIncludes}\");"
          '';
        }))
        irony-server
        php80
        perlPackages.AnyEvent
        perlPackages.ClassAccessorFast
        perlPackages.DataSExpression
        perlPackages.DBI
        perlPackages.DBDSQLite
        perlPackages.DBDMariaDB
        perlPackages.DBDPg
        perlPackages.RPCEPCService
      ];

      #emacs28-git = ((prev.emacsPackagesGen final.emacsGit-nox).emacsWithPackages)
      #  (epkgs: (with epkgs.melpaStablePackages; [
      #  ]) ++ (with epkgs.melpaPackages; [
      #    leaf
      #    leaf-keywords
      #  ]) ++ (with epkgs.elpaPackages; [
      #  ]) ++ [
      #  ]);

      melpaPackagesOverride = melpaPackages: melpaPackages // {
        tsc = melpaPackages.tsc.overrideAttrs (old: {
          postPatch = (old.postPatch or "") + ''
            substituteInPlace core/tsc-dyn-get.el --replace \
            "tsc-dyn-dir tsc--dir" "tsc-dyn-dir \"/ws/emacs-tree-sitter/result/lib\""
          '';
        });
        # lives in ~/.emacs.d/git now
        #tree-sitter-langs = melpaPackages.tree-sitter-langs.overrideAttrs (old: let
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
        solarized-theme = melpaPackages.solarized-theme.overrideAttrs (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            substituteInPlace solarized.el --replace \
            "'create-solarized-theme-file 'solarized-create-theme-file)" \
            "'create-solarized-theme-file 'solarized-create-theme-file \"0\")"
          '' + (old.postPatch or "");
        });
        #share/emacs/site-lisp/elpa/all-the-icons-20200923.1339/all-the-icons.el
        #(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon)
        all-the-icons = melpaPackages.all-the-icons.overrideAttrs (old: {
          postPatch = ''
            ${prev.tree}/bin/tree $src
            substituteInPlace all-the-icons.el --replace \
            "(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon)" \
            "(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon \"0\")"
          '' + (old.postPatch or "");
        });
        #share/emacs/site-lisp/elpa/php-mode-20201120.1807/php-local-manual.el
        #(define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search)
        php-mode = melpaPackages.php-mode.overrideAttrs (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            substituteInPlace lisp/php-local-manual.el --replace \
            "'php-search-local-documentation \#'php-local-manual-search)" \
            "'php-search-local-documentation \#'php-local-manual-search \"0\")"
          '' + (old.postPatch or "");
        });
        #share/emacs/site-lisp/elpa/racer-20191001.2344/racer.el
        #(define-obsolete-function-alias 'racer-turn-on-eldoc 'eldoc-mode)
        #(define-obsolete-function-alias 'racer-activate 'racer-mode)
        racer = melpaPackages.racer.overrideAttrs (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            substituteInPlace racer.el --replace \
            "(define-obsolete-function-alias 'racer-turn-on-eldoc 'eldoc-mode)" \
            "(define-obsolete-function-alias 'racer-turn-on-eldoc 'eldoc-mode \"0\")" \
            --replace \
            "(define-obsolete-function-alias 'racer-activate 'racer-mode)" \
            "(define-obsolete-function-alias 'racer-activate 'racer-mode \"0\")"
          '' + (old.postPatch or "");
        });
        #share/emacs/site-lisp/elpa/nix-mode-20201229.138/nix-prettify-mode.el
        #(define-obsolete-function-alias 'global-nix-prettify-mode 'nix-prettify-global-mode)

        #share/emacs/site-lisp/elpa/nix-mode-20201229.138/nix-mode-autoloads.el
        #(define-obsolete-function-alias 'global-nix-prettify-mode 'nix-prettify-global-mode)
        nix-mode = melpaPackages.nix-mode.overrideAttrs (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            substituteInPlace nix-prettify-mode.el --replace \
            "'global-nix-prettify-mode 'nix-prettify-global-mode)" \
            "'global-nix-prettify-mode 'nix-prettify-global-mode \"0\")"
          '' + (old.postPatch or "");
        });
        #share/emacs/site-lisp/elpa/lsp-mode-20201231.1252/lsp-ocaml.el
        #(define-obsolete-variable-alias 'lsp-merlin 'lsp-ocaml-lsp-server)
        #(define-obsolete-variable-alias 'lsp-merlin-command 'lsp-ocaml-lsp-server-command)

        irony = melpaPackages.irony.overrideAttrs (old: {
          postPatch = ''
            #${prev.tree}/bin/tree $src
            sed -i '/define-minor-mode/,/:group/ { s/nil/:init-value nil/ ; s/irony-lighter/:lighter irony-lighter/ ; s/irony-mode-map/:keymap irony-mode-map/ }' irony.el
          '' + (old.postPatch or "");
          doCheck = false;
        });
      };

      emacsPackages = (final.emacs-overlay.emacsPackagesFor final.emacsGcc-nox)
        .overrideScope' (eself: esuper:
        let
          melpaPackages = final.melpaPackagesOverride esuper.melpaPackages;
        in
        melpaPackages // { inherit melpaPackages; });

      irony-server = (prev.irony-server.override {
        irony = final.emacsPackages.melpaPackages.irony;
      });

      emacs28-git-pkgs = (final.emacs-overlay.emacsWithPackagesFromUsePackage {
        config = builtins.readFile inputs.init-leafs.outPath;
        package = final.emacsGcc-nox;
        alwaysEnsure = true;

        # Optionally provide extra packages not in the configuration file.
        extraEmacsPackages = epkgs: with epkgs; [
          # meh, this break doom-modeline
          #all-the-icons
          tsc
          # lives in ~/.emacs.d/git now
          #tree-sitter-langs
          tree-sitter
        ];

        # Optionally override derivations.
        override = epkgs:
        { inherit (epkgs.elpaPackages) spinner ; }
        // final.emacsPackages;
      });

      emacs28-load-path = prev.writeText "eval-when-compile-load-path.el" ''
        (eval-when-compile
          (let ((default-directory "${final.emacs28-git-pkgs.deps.outPath}/share/emacs/site-lisp"))
            (normal-top-level-add-subdirs-to-load-path)))
      '';

      emacs28-git-ide = with final; prev.symlinkJoin {
        name = "emacs";
        paths = [
          emacs28-git-pkgs
          (if useLatestNodeJS then nodejs_latest else nodejs)
          rustNightly.rust
        ]
        ++ emacsExtraPathPackages;
        buildInputs = [
          prev.makeWrapper
          rustNightly.rust-src
        ];
        postBuild = ''
          unlink $out/share/emacs
          mkdir -p $out/share/emacs/site-lisp
          cp ${emacs28-load-path} $out/share/emacs/site-lisp/eval-when-compile-load-path.el
          wrapProgram $out/bin/emacs \
            --set LIBCLANG_PATH "${libclangLib}" \
            --set BINDGEN_EXTRA_CLANG_ARGS "-isystem ${libclangIncludes}" \
            --set RUST_SRC_PATH "${rustNightly.rust-src}/lib/rustlib/src/rust/library" \
            --prefix PERL5LIB : "$out/lib/perl5/site_perl/5.34.0:$out/lib/perl5/site_perl/5.34.0/x86_64-linux-thread-multi" \
            --prefix PATH : $out/bin:${prev.lib.makeBinPath emacsNodePackages}
        '';
      };
    };
  };
}
