;; BROKEN 
(define-module (hikaco packages cromite)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages backup)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (guix licenses)
  #:use-module (guix build utils))

(define-public cromite
  (package
   (name "cromite")
   (version "v133.0.6943.60-3fc3e3c23494d21b4bb997d1e408b25014ce973d")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/uazo/cromite/releases/download/"
                         version
                         "/"
                         "chrome-lin64.tar.gz"))
     (sha256
      (base32 "191l6fdh8h21azivjq26h598dhcq7qj96pl8z7pkpdfxgi2jkz19"))))
   (build-system chromium-binary-build-system)
   (arguments
    (list
     #:validate-runpath? #f ;; This should really be enabled some day, but meh.
     #:wrapper-plan
     #~'("chrome"
         "chrome_sandbox"
         "chrome_crashpad_handler"
         "libEGL.so"
         "libGLESv2.so"
         "libqt5_shim.so"
         "libqt6_shim.so"
         "libvk_swiftshader.so"
         "libvulkan.so.1")
     #:install-plan
     #~'(("." "share"))
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'binary-unpack)
                      (add-before 'install-wrapper 'install-exe
				  (lambda _
				    (let* ((bin (string-append #$output "/bin/"))
					   (old-exe (string-append #$output "/share/" "chrome"))
					   (exe (string-append bin "cromite"))
                                           (gtk-share #$(file-append gtk+ "/share")))
				      (mkdir-p bin)
                                      (symlink old-exe exe)
				      (wrap-program exe
						    '("CHROME_WRAPPER" = (#$name))
						    `("XDG_DATA_DIRS" ":" prefix (,gtk-share))))))

                      (add-after 'install-exe 'create-desktop-entry
                                 (lambda _
                                   (let* ((bin (string-append #$output "/bin/"))
                                          (exe (string-append bin "cromite"))
                                          (share (string-append #$output "/share/"))
                                          (appdir (string-append share "/applications")))
                                     (mkdir-p appdir)
                                     (make-desktop-entry-file
                                      (string-append appdir "/" #$name ".desktop")
                                      #:name "Cromite"
                                      #:comment "A private web browser"
                                      #:exec exe
                                      #:icon (string-append share "product_logo_48.png")
                                      #:mime-type (string-append
                                                  "application/pdf;"
                                                  "application/rdf+xml;"
                                                  "application/rss+xml;"
                                                  "application/xhtml+xml;"
                                                  "application/xhtml_xml;"
                                                  "application/xml;"
                                                  "image/gif;"
                                                  "image/jpeg;"
                                                  "image/png;"
                                                  "image/webp;"
                                                  "text/html;"
                                                  "text/xml;"
                                                  "x-scheme-handler/http;"
                                                  "x-scheme-handler/https"))))))))
   (inputs
    (list bzip2
          curl
          flac
          font-liberation
          gdk-pixbuf
          gtk
      	  gtk+
          openh264
          nspr
          harfbuzz
          libexif
          libglvnd
          libpng
          libva
          libxscrnsaver
          opus
          pciutils
          pipewire
          qtbase-5
          qtbase
          snappy
      	  nss
          dbus
      	  util-linux
          xdg-utils
          libarchive
          ffmpeg-7
          (list gcc-12 "lib")
          wget))

   (synopsis
    "Cromite a Bromite fork with ad blocking and privacy enhancements; take back your browser!")
   (description
    "Cromite is a Chromium fork based on Bromite with built-in support for ad blocking and an eye for privacy.")
   (home-page "https://github.com/uazo/cromite")
   (license gpl3)))


