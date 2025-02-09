(define-module (hikaco home hikari)
  #:use-module (hikaco packages pragmasevka)
  #:use-module (hikaco packages wideriver)
  #:use-module (hikaco packages brave)
  #:use-module (hikaco packages lamdera)
  #:use-module (hikaco packages lld-as-ld)
  #:use-module (hikaco packages mlton)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services gnupg)
  #:use-module (saayix packages lsp)
  #:use-module (guix gexp))

(define %base-packages
  (map specification->package+output
       (list "librewolf"
      	     "fish"
             "emacs-lucid"
             "fish-foreign-env"
	     "bibata-cursor-theme"
	     "hicolor-icon-theme"
             "adwaita-icon-theme"
	     "htop"
	     "unzip"
	     "pipewire"
	     "wireplumber"
             "pavucontrol"
             "font-google-noto"
             "font-google-noto-sans-cjk"
             "font-google-noto-emoji"
             "font-google-material-design-icons"
             "font-google-roboto"
             "ghc"
             "cabal-install"
             "libffi"
             "mumi"
             "notmuch"
             "xdg-utils"
             "offlineimap3"
             "msmtp"
             "gnupg"
             "pinentry"
             "ugrep"
             "glib"
             "python"
             "sdl3"
             "ninja"
             "clang-toolchain"
             "linux-libre-headers"
             "ncurses"
             "emacs-lsp-booster"
             "eza"
             "codeberg-cli")))

(define-public %hikari
  (home-environment
   (packages (cons* guile-lsp-server git (list git "send-email") lld19-as-ld-wrapper wideriver pragmasevka brave mlton %base-packages))
   (services
    (list (service home-dbus-service-type)
	  (service home-bash-service-type
            (home-bash-configuration
             (bashrc (list (local-file "./files/prompt.sh")))
             (aliases '(("ls" . "eza")
                        ("la"  . "eza -lbhHigUmuSa --time-style=long-iso --git --color-scale")))))  

          (simple-service 'emacs-daemon home-shepherd-service-type
                          (list
                           (shepherd-service
                            (provision '(emacs-daemon))
                            (start 
                             #~(make-forkexec-constructor 
                                (list #$(file-append (specification->package "emacs-lucid") "/bin/emacs") "--fg-daemon")))
                            (stop #~(make-kill-destructor)))))

          (service home-xdg-configuration-files-service-type
                   `(("foot/foot.ini",              (local-file "./files/foot.ini"))
                     ("i3status-rust/config.toml",  (local-file "./files/i3status-rust.toml"))
                     ("i3bar-river/config.toml",    (local-file "./files/i3bar-river.toml"))
                     ("gtk-3.0/gtk.css",            (local-file "./files/gtk.css"))
                     ("gtk-3.0/settings.ini",       (local-file "./files/gtk-settings.ini"))
                     ("fuzzel/fuzzel.ini",          (local-file "./files/fuzzel.ini"))))
	  (service home-pipewire-service-type)
          (service home-gpg-agent-service-type)))))
