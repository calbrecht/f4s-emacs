{
  description = "Emacs setup flake.";

  inputs = {
    emacs-overlay = { url = github:nix-community/emacs-overlay; };
    init-leafs = { url = path:/home/alab/.emacs.i/init-leafs.el; flake = false; };
    nodejs = { url = github:calbrecht/f4s-nodejs; inputs.nixpkgs.follows = "nixpkgs"; };
    rust = { url = github:calbrecht/f4s-rust; };
  };

  outputs = { self, nixpkgs, ... }@inputs:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ self.overlay ];
    };
    useLatestNodeJS = true;
  in
  {
    defaultPackage."${system}" = pkgs.emacs28-git-ide;

    overlay = final: prev: {
      emacs-overlay = (prev.emacs-overlay or { }) //
        (inputs.emacs-overlay.overlay final prev);

      nodePackages = (prev.nodePackages or { }) //
        (inputs.nodejs.overlay final prev).nodePackages;

      nodePackages_latest = (prev.nodePackages_latest or { }) //
        (inputs.nodejs.overlay final prev).nodePackages_latest;

      rustNightly = (prev.rustNightly or { }) //
        (inputs.rust.overlay final prev).rustNightly;

      emacsGit-nox = final.emacs-overlay.emacsGit-nox;

      emacsNodePackages = prev.lib.attrValues {
        inherit (if useLatestNodeJS then final.nodePackages_latest else final.nodePackages)
          eslint eslint_d import-js jsonlint prettier standardx tslint typescript trepan-ni;
      };

      emacsExtraPathPackages = with final; [
        #TODO crate2nix
        #TODO rnix-lsp
        nixpkgs-fmt
        fd
        fzf
        stdenv.cc.bintools.bintools_bin
        diffutils
        llvmPackages.libclang
        llvmPackages.clang
        llvmPackages.bintools
        pkg-config
      ];

      #emacs28-git = ((prev.emacsPackagesGen final.emacsGit-nox).emacsWithPackages)
      #  (epkgs: (with epkgs.melpaStablePackages; [
      #  ]) ++ (with epkgs.melpaPackages; [
      #    leaf
      #    leaf-keywords
      #  ]) ++ (with epkgs.elpaPackages; [
      #  ]) ++ [
      #  ]);

      emacs28-git-pkgs = (final.emacs-overlay.emacsWithPackagesFromUsePackage {
        config = builtins.readFile inputs.init-leafs.outPath;
        package = final.emacsGit-nox;
        alwaysEnsure = false;

        # Optionally provide extra packages not in the configuration file.
        extraEmacsPackages = epkgs: [
          # meh, this break doom-modeline
          #epkgs.all-the-icons
        ];

        # Optionally override derivations.
        override = epkgs: epkgs // {
          solarized-theme = epkgs.melpaPackages.solarized-theme.overrideAttrs (old: {
            postPatch = ''
              #${prev.tree}/bin/tree $src
              substituteInPlace solarized.el --replace \
                "'create-solarized-theme-file 'solarized-create-theme-file)" \
                "'create-solarized-theme-file 'solarized-create-theme-file \"0\")"
            '' + (old.postPatch or "");
          });
          #share/emacs/site-lisp/elpa/all-the-icons-20200923.1339/all-the-icons.el
          #(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon)
          all-the-icons = epkgs.melpaPackages.all-the-icons.overrideAttrs (old: {
            postPatch = ''
              ${prev.tree}/bin/tree $src
              substituteInPlace all-the-icons.el --replace \
                "(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon)" \
                "(define-obsolete-function-alias 'define-icon 'all-the-icons-define-icon \"0\")"
            '' + (old.postPatch or "");
          });
          #share/emacs/site-lisp/elpa/php-mode-20201120.1807/php-local-manual.el
          #(define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search)
          php-mode = epkgs.melpaPackages.php-mode.overrideAttrs (old: {
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
          racer = epkgs.melpaPackages.racer.overrideAttrs (old: {
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
          nix-mode = epkgs.melpaPackages.nix-mode.overrideAttrs (old: {
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
        };
      });

      emacs28-load-path = prev.writeText "eval-when-compile-load-path.el" ''
        (let ((default-directory "${final.emacs28-git-pkgs.deps.outPath}/share/emacs/site-lisp"))
          (normal-top-level-add-subdirs-to-load-path))
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
          wrapProgram $out/bin/emacs --set \
            RUST_SRC_PATH "${rustNightly.rust-src}/lib/rustlib/src/rust/library" \
            --prefix PATH : $out/bin:${prev.lib.makeBinPath emacsNodePackages}}
        '';
      };
    };
  };
}
