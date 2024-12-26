(use-modules (gnu)
	     (gnu services networking)
	     (gnu services ssh)
	     (gnu services desktop)
	     (gnu services dbus)
	     (gnu services shepherd)
	     (gnu packages ssh)
	     (gnu packages zig-xyz)
	     (gnu packages terminals)
	     (gnu packages shells)
	     (gnu packages dns)
	     (gnu packages fontutils)
	     (gnu packages librewolf)
	     (gnu packages admin)
	     (gnu packages mail)
	     (gnu packages linux)
	     (gnu packages wm)
             (gnu packages gnome-xyz)
             (gnu packages kde-frameworks)
	     (gnu packages rust-apps)
	     (gnu packages xdisorg)
	     (gnu packages image)
	     (gnu packages version-control)
	     (gnu packages emacs)
	     (gnu packages compression)
             (guix gexp)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (guix licenses)
             (guix utils)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

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

(define nonguix-pub-key
  (plain-file "nonguix-pub-key.pub"
    "(public-key 
       (ecc 
         (curve Ed25519)
           (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define bordeaux-inria-key 
  (plain-file "bordeaux-inria-key.pub"
    "(public-key
       (ecc
        (curve Ed25519)
         (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))"))

(operating-system
  (kernel linux-xanmod)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (host-name "Hikaco")
  (timezone "Asia/Kolkata")
  (locale "en_GB.utf8")

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (timeout 3)
                (terminal-outputs '(console))
                (targets '("/dev/sda"))))

  (initrd-modules (cons* "i915" %base-initrd-modules))
  (kernel-arguments
   (list
    "mitigations=off"
    "loglevel=3"
    "tsc=reliable"
    "page_alloc.shuffle=1"))
  
  (file-systems (append (list (file-system
                               (device "/dev/sda4")
                               (mount-point "/")
                               (type "ext4"))                               
                              (file-system
                               (mount-point "/tmp")
                               (device "none")
                               (type "tmpfs")
                               (check? #f))) 

                      %base-file-systems))

  (swap-devices 
    (list (swap-space (target "/dev/sda3"))))

  (users (cons (user-account
                (name "hikari")
                (group "users")
		(supplementary-groups '("wheel" "audio" "video")))
               %base-user-accounts))

  (packages (cons* 
	      river foot fnott i3status-rust wl-clipboard grim slurp fontconfig fuzzel wideriver
               %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (service dhcp-client-service-type)
                          (service nftables-service-type)
                          (service dbus-root-service-type)
                          (service elogind-service-type)
                          (service polkit-service-type)
                          (service openntpd-service-type 
                            (openntpd-configuration
                              (servers '("0.arch.pool.ntp.org"))))

                          (service openssh-service-type
                           (openssh-configuration
                            (openssh openssh-sans-x)
                            (port-number 2222))))
		    
		    (modify-services %base-services
                      (guix-service-type config => (guix-configuration
                        (inherit config)
	                (substitute-urls
			  (append 
                            (list 
                              "https://substitutes.nonguix.org"
                              "https://guix.bordeaux.inria.fr")
	                    %default-substitute-urls))
		          (authorized-keys
			    (append 
                              (list 
                               nonguix-pub-key
                               bordeaux-inria-key)
			      %default-authorized-guix-keys))))))))

	
