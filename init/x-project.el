;; COMMIT: add bookmark-in-project and project-x
(leaf project-x
  :quelpa (project-x
           :fetcher github
           :repo "karthink/project-x")
  :require t
  :custom (project-x-local-identifier
           . '(".project" ; anything
               "TAGS" "GTAGS"                   ; tags
               "configure.ac" "configure.in"    ; autoconf
               "cscope.out"                     ; cscope
               "SConstruct"                     ; scons
               "meson.build"                    ; meson
               "default.nix" "flake.nix"        ; nix
               "WORKSPACE"                      ; bazel
               "debian/control"                 ; debian
               "Makefile" "GNUMakefile" "CMakeLists.txt"  ; Make & CMake
               "composer.json"   ; PHP
               "rebar.config" "mix.exs"      ; Erlang & Elixir
               "Gruntfile.js" "gulpfile.js" "package.json" "angular.json" ; KS
               ;; Python
               "manage.py" "requirements.txt" "setup.py" "tox.ini" "Pipfile"
               "poetry.lock"

               "pom.xml" "build.gradle" "gradlew" "application.yml"    ; Java & friends
               "build.sbt" "build.sc"                                  ; Scala
               "project.clj" "build.boot" "deps.edn" ".bloop"          ; Clojure
               "Gemfile"                                               ; Ruby
               "shard.yml"                                             ; Crystal
               "Cask" "Eldev" "Keg" "Eask"                             ; Emacs
               "DESCRIPTION"                                           ; R
               "bower.json" "psc-package.json" "spago.dhall"           ; PureScript
               "stack.yaml"                                            ; Haskell
               "Cargo.toml"                                            ; Rust
               "info.rkt"                                              ; Racket
               "pubspec.yaml"                                          ; Dart
               "dune-project"                                          ; OCaml
               "go.mod"                                                ; Go
               ))
  :config
  (setq project-x-save-interval 150)
  (setq project-x-window-list-file
        (no-littering-expand-var-file-name "project-x-window-list.el"))
  (project-x-mode 1))

(leaf bookmark-in-project
  :ensure t
  :bind (("C-x r M-n" . bookmark-in-project-jump-next)
         ("C-x r M-p" . bookmark-in-project-jump-previous)
         ("C-x r M-m" . bookmark-in-project-toggle)
         ("C-x r M-b" . bookmark-in-project-jump)))
