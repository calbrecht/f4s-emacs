{
  description = "Emacs setup flake.";

  inputs = {
    emacs-overlay = {
      url = github:nix-community/emacs-overlay;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    init-leafs = {
      url = path:/home/alab/.emacs.i/init-leafs.el;
      flake = false;
    };
    nodejs = {
      url = github:calbrecht/f4s-nodejs;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    f4s-overlays = {
      url = github:calbrecht/f4s-overlays;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust.follows = "rust";
      inputs.nodejs.follows = "nodejs";
      inputs.emacs.follows = "";
    };
    rust = {
      url = github:calbrecht/f4s-rust;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ 
        inputs.f4s-overlays.overlays.fixups
        self.overlay
      ];
    };
    useLatestNodeJS = true;
  in
  {
    defaultPackage."${system}" = pkgs.emacs-git-ide;

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
        tree-sitter
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
        esuper.elpaPackages
        // { inherit (esuper) elpaPackages; }
        // melpaPackages
        // { inherit melpaPackages; });

      emacsGitWithPackages = (final.emacs-overlay.emacsWithPackagesFromUsePackage {
        config = builtins.readFile inputs.init-leafs.outPath;
        package = final.emacsGcc-nox;
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
          (let ((default-directory "${final.emacsGitWithPackages.deps.outPath}/share/emacs/site-lisp"))
            (normal-top-level-add-subdirs-to-load-path)))
      '';

      emacs-git-ide = with final; prev.symlinkJoin {
        name = "emacs";
        paths = [
          emacsGitWithPackages
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
          cp ${emacsGitLoadPath} $out/share/emacs/site-lisp/eval-when-compile-load-path.el
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
