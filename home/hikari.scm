(define-module (hikaco home hikari)
  #:use-module (hikaco packages pragmasevka)
  #:use-module (hikaco packages wideriver)
  #:use-module (hikaco packages brave)
  #:use-module (hikaco packages lamdera)
  #:use-module (hikaco packages mlton)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services gnupg)
  #:use-module (guix gexp))

(define %base-packages
  (map specification->package+output
       (list "librewolf"
      	     "fish"
             "emacs-pgtk"
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
             "gcc-toolchain"
             "ghc"
             "cabal-install"
             "libffi"
             "mumi"
             "notmuch"
             "xdg-utils"
             "offlineimap3"
             "git"
             "git:send-email"
             "msmtp"
             "gnupg"
             "pinentry"
             "ripgrep"
             "glib"
             "python"
	           "elm"
             "sdl3"
             "ninja"
             "clang")))

(define-public %hikari
  (home-environment
   (packages (cons* wideriver pragmasevka brave lamdera mlton %base-packages))
   (services
    (list (service home-dbus-service-type)
          (service home-fish-service-type)
	    
          (simple-service 'emacs-daemon home-shepherd-service-type
            (list
              (shepherd-service
                (provision '(emacs-daemon))
                (start 
                  #~(make-forkexec-constructor 
                     (list #$(file-append (specification->package "emacs-pgtk") "/bin/emacs") "--fg-daemon")))
                (stop #~(make-kill-destructor)))))

          (service home-files-service-type
              `((".bashrc",                    (local-file "./files/bashrc"))))
 
          (service home-xdg-configuration-files-service-type
              `(("foot/foot.ini",              (local-file "./files/foot.ini"))
                ("i3status-rust/config.toml",  (local-file "./files/i3status-rust.toml"))
                ("i3bar-river/config.toml",    (local-file "./files/i3bar-river.toml"))
                ("gtk-3.0/gtk.css",            (local-file "./files/gtk.css"))
                ("gtk-3.0/settings.ini",       (local-file "./files/gtk-settings.ini"))
                ("emacs/init.el",              (local-file "./files/emacs.el"))
                ("fuzzel/fuzzel.ini",          (local-file "./files/fuzzel.ini"))))
	  (service home-pipewire-service-type)
    (service home-gpg-agent-service-type)))))
