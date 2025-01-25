(define-module (hikaco packages brave)
  #:use-module (ice-9 regex)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
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
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build utils)
  #:use-module (nonguix build-system chromium-binary))

(define-public brave
  (package
   (name "brave-browser")
   (version "1.73.105")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/brave/brave-browser/releases/download/"
	   "v"
	   version
	   "/"
	   name
	   "_"
	   version
	   "_amd64.deb"))
     (sha256
      (base32 "0gcp3b7ywicwp6g4gwxiiqw85q7jih0cnq3g4dgcamyai6ingjd9"))))
   (build-system chromium-binary-build-system)
   (arguments
    (list
     #:substitutable? #f
     #:validate-runpath? #f
     #:wrapper-plan
     #~(let ((path "opt/brave.com/brave/"))
	 (map (lambda (file) (string-append path file))
	      '("brave"		
		"chrome_crashpad_handler"
		"chrome-management-service"
		"chrome-sandbox"
		"libEGL.so"
		"libGLESv2.so"
		"libqt5_shim.so"
		"libqt6_shim.so"
		"libvk_swiftshader.so"
		"libvulkan.so.1")))

     #:install-plan
     #~'(("opt/" "/share")
	 ("usr/share/" "/share"))

     ;; This doesn't work, because I set wrapper-plan above (?), no point in removing it though.
     #:patchelf-plan
     #~(("brave" #$(file-append nss "/lib/nss/")))
     
     #:phases
     #~(modify-phases %standard-phases
		      (add-before 'install 'patch-assets
				  (lambda _
				    (let* ((bin (string-append #$output "/bin"))
					   (share (string-append #$output "/share"))
					   (opt "./opt")
					   (usr/share "./usr/share")
					   (old-exe (string-append "/opt/brave.com/brave/" #$name))
					   (exe (string-append bin "/" #$name)))
				      (substitute* (string-append opt "/brave.com/brave/" #$name)
						   (("CHROME_WRAPPER") "WRAPPER"))
				      (substitute* (string-append usr/share "/applications/" #$name ".desktop")
						   (("^Exec=.*") (string-append "Exec=" (string-append bin "/brave") "\n")))
				      (substitute* (string-append usr/share "/gnome-control-center/default-apps/" #$name ".xml")
						   ((old-exe) exe)))))
		      
		      (add-before 'install-wrapper 'install-icons
				  (lambda _
				    (let* ((icons (string-append #$output "/share/brave.com/brave"))
					   (pl (string-append #$output "/share/icons/hicolor"))
				           (files (find-files icons "product_logo_.*\\.*")))
				      				      
				      (for-each (lambda (file)
						  (let* ((icon-name (basename file))
							 (icon-size (string-drop-right (string-drop icon-name 13) 4))
							 (target (string-append pl "/" icon-size "x" icon-size "/" "apps")))
						    (install-file file target)
						    (rename-file
						     (string-append target "/" icon-name)
						     (string-append target "/" "brave-browser.png"))))
				       files))))
				  
		      (add-before 'install-wrapper 'install-exe
				  (lambda _
				    (let* ((bin (string-append #$output "/bin/"))
					   (old-exe (string-append "/opt/brave.com/brave/" #$name))
					   (exe (string-append bin "brave"))
					   (gtk-share #$(file-append gtk+ "/share")))
				      (mkdir-p bin)
				      (symlink (string-append #$output "/share/brave.com/brave/brave") exe)
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
	   util-linux
           xdg-utils
           wget))

    (synopsis
     "Brave is a free and open-source web browser developed by Brave Software, Inc.")
    (description
     "based on the Chromium web browser, Brave is a privacy-focused browser, which automatically blocks most advertisements and website trackers in its default settings.")
   (home-page "https://github.com/brave/brave-browser")
   (license mpl2.0)))
