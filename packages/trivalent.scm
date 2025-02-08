;; BROKEN 
(define-module (hikaco packages trivalent)
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

(define-public trivalent
  (package
   (name "trivalent")
   (version "132.0.6834.159-434590")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://download.copr.fedorainfracloud.org/results/secureblue/trivalent/fedora-41-x86_64//08591121-trivalent/trivalent"
	   "-"
	   version
	   ".x86_64"
           ".rpm"))
     (sha256
      (base32 "1550l340c0rjv1b12rh7q7pcnc0q7cs0jh5ixnxvwbqpx1rs186r"))))
   (build-system chromium-binary-build-system)
   (arguments
    (list
     #:validate-runpath? #f ;; This should really be enabled some day, but meh.
     #:wrapper-plan
     #~(let ((path "usr/lib64/trivalent/"))
         (map (lambda (file)
                (string-append path file))
              '("chrome_crashpad_handler"
                "trivalent")))
     #:install-plan
     #~'(("usr/share/" "/share")
         ("usr/lib64/" "/share")
         ("etc" "etc"))
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'binary-unpack)
                      
                      (add-after 'unpack 'binary-unpack
                                 (lambda _
                                   (let* ((files (filter (lambda (f)
                                                           (not (string=? (basename f) "environment-variables")))
                                                         (find-files (getcwd))))
                                          (binary-file (car files)))
                                     (when (and (= 1 (length files)) (string-suffix? ".rpm" binary-file))
                                       (mkdir "binary")
                                       (chdir "binary")
                                       (invoke "bsdtar" "-xf" binary-file)))))
                      
                      (add-before 'install 'patch-assets
                                  (lambda _
                                    (let* ((bin (string-append #$output "/bin/"))
                                           (usr/share "./usr/share/")
                                           (binlib64 "./usr/lib64/")
                                           (old-exe "/usr/bin/trivalent")
                                           (exe (string-append bin "trivalent")))
                                      
				      (substitute* (string-append usr/share "/applications/" #$name ".desktop")
						   (("^Exec=.*") (string-append "Exec=" (string-append bin "trivalent") "\n")))
				      (substitute* (string-append usr/share "/gnome-control-center/default-apps/" #$name ".xml")
						   ((old-exe) exe)))))
                      
		      (add-before 'install-wrapper 'install-exe
				  (lambda _
				    (let* ((bin (string-append #$output "/bin/"))
					   (old-exe (string-append "/usr/bin/" "trivalent"))
					   (exe (string-append bin "trivalent"))
                                           (oldexe (string-append #$output "/share/trivalent/trivalent"))
					   (gtk-share #$(file-append gtk+ "/share"))
                                           (pred (lambda (x y)
                                                   (string= (car (cddr (string-split x #\.))) y))))
				      (mkdir-p bin)
                                      (system* "patchelf" "--remove-needed" "libgcc_s.so.1" oldexe)
                                      ;; Trivalent builds against a newer version of ffmpeg, guix doesn't have it yet (?). This is a hacky fix.
                                      ;; (for-each (lambda (file)
                                      ;;             (if (or (pred file "61")
                                      ;;                     (pred file "59")
                                      ;;                     (pred file "7"))
                                      ;;                 (system* "patchelf" "--remove-needed" file oldexe)
                                      ;;                 (system* "patchelf" "--add-needed" file oldexe)))
                                      ;;           '("libavformat.so.61" "libavcodec.so.61" "libavutil.so.59" "libopenh264.so.7"
                                      ;;             "libavformat.so.60" "libavcodec.so.60" "libavutil.so.58" "libopenh264.so.6"))
                                      
                                      (symlink oldexe exe)
				      (wrap-program exe
						    '("CHROME_WRAPPER" = (#$name))
						    `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))))))))
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
    "Brave is a free and open-source web browser developed by Brave Software, Inc.")
   (description
    "based on the Chromium web browser, Brave is a privacy-focused browser, which automatically blocks most advertisements and website trackers in its default settings.")
   (home-page "https://github.com/brave/brave-browser")
   (license bsd-3)))


