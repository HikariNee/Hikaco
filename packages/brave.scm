(define-module (hikaco packages brave)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary))

(define-public brave
  (package
   (name "brave-browser")
   (version "1.73.105")
   (source
    (origin
     (method uri-fetch)
     (uri (string-append
	   "https://github.com/brave/brave-browser/releases/download/"
	   version
	   "/"
	   name
	   "-browser_"
	   version
	   "_amd64.deb"))
     (sha256 (base32 "0gcp3b7ywicwp6g4gwxiiqw85q7jih0cnq3g4dgcamyai6ingjd9"))))
   (build-system chromium-binary-build-system)
   (arguments
    (list
     #:substitutable? #f
     #:wrapper-plan
     #~(let ((path (string-append "opt/brave.com/brave/")))
	 (map (lambda (file)
		(string-append path file))
	      '("brave"
		"brave-browser"
	        "chrome-sandbox"
                "chrome_crashpad_handler"
		"libEGL.so"
		"libGLESv2.so"
		"libqt5_shim.so"
                "libqt6_shim.so"
		"libvk_swiftshader.so"
		"libvulkan.so.1")))
     #:phases
     #~(modify-phases %standard-phases
		      (add-before 'install 'patch-assets
				  (lambda _
				    (let* ((bin (string-append #$output "/bin"))
					   (share (string-append #$output "/share"))
					   (opt "./opt")
					   (usr/share "./usr/share")
					   (old-exe (string-append "/opt/brave.com/brave/" #$name))
					   (exe (string-append bin "/brave-browser")))
				      (substitute* (string-append opt "/brave/brave.com/" #$name)
						   (("CHROME_WRAPPER") "WRAPPER"))
				      (substitute* (string-append usr/share "/applications/" #$name ".desktop")
                                                   (("^Exec=.*") (string-append "Exec=" exe "\n")))
                                       (substitute* (string-append usr/share "/gnome-control-center/default-apps/" #$name ".xml")
                                                   ((old-exe) exe))
                                       (substitute* (string-append usr/share "/menu/" #$name ".menu")
                                                   (("/opt") share)
                                                   ((old-exe) exe)))))		      
             (add-after 'install 'install-icons
                (lambda _
                  (define (format-icon-size name)
                    (car
                      (string-split
                       (string-drop-right (string-drop name 13) 4)
                       #\_)))
                  (let ((icons (string-append #$output "/share/icons/hicolor"))
                        (share (string-append #$output "/share/brave/" #$name)))
                    (for-each (lambda (icon)
                                (let* ((icon-name (basename icon))
                                       (icon-size (format-icon-size icon-name))
                                       (target (string-append icons "/" icon-size "x" icon-size "/apps/" #$name ".png")))
                                  (mkdir-p (dirname target))
                                  (rename-file icon target)))
                              (find-files share "product_logo_.*\\.*")))))
	     (add-before 'install-wrapper 'install-exe
			 (lambda _
			   (let* ((bin (string-append #$output "/bin"))
				  (exe (string-append bin "/" #$name))
				  (share (string-append #$output "/share"))
				  (target (string-append share "/brave.com/brave/" ))))))
))))
