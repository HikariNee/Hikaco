(define-module (hikaco packages wideriver)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix utils))

(define-public wideriver
  (package
    (name "wideriver")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-courtis/wideriver")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16i0mzgxn32nrh5ajn0kb4xdwmsjg03amhasxhwyvspar5y4flhg"))))
    (build-system gnu-build-system)

    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "PREFIX="
                                          #$output)
                           (string-append "CC="
                                          #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs (map specification->package+output (list "pkg-config")))
    (inputs (map specification->package+output (list "wayland" "wayland-protocols" "wlroots")))
    (home-page "https://github.com/alex-courtis/wideriver")
    (synopsis "A set of riverWM layouts")
    (description
     "Tiling window manager for the river wayland compositor, inspired by dwm and xmonad.")
    (license gpl3)))
