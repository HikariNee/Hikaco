(define-module (hikaco system)
  #:use-module (hikaco home hikari)
  #:use-module (hikaco keys)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages networking)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services guix)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(operating-system
 (kernel linux-6.6)
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
                              (type "ext4"))) 
                       %base-file-systems))

 (swap-devices 
  (list (swap-space (target "/dev/sda3"))))

 (users (cons (user-account
               (name "hikari")
               (group "users")
	       (supplementary-groups '("wheel" "audio" "video")))
              %base-user-accounts))

 (packages (append 
            (map specification->package+output (list
	                                        "river"
                                                "foot"
	                                        "i3status-rust"
       	                                        "wl-clipboard"
	                                        "grim"
	                                        "slurp"
	                                        "fontconfig"
	                                        "fuzzel"))
            %base-packages))

 ;; Add services to the baseline: a DHCP client and
 ;; an SSH server.
 (services (append (list (service network-manager-service-type
                                  (network-manager-configuration
                                   (shepherd-requirement '(udev))))
                         (service nftables-service-type)
                         (service dbus-root-service-type)
                         (service elogind-service-type)
                         (service polkit-service-type)
                         (service guix-home-service-type
                                  `(("hikari", %hikari)))
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
                                                                     "https://substitutes.nonguix.org")
	                                                            %default-substitute-urls))
		                                                  (authorized-keys
			                                           (append 
                                                                    (list 
                                                                     nonguix-pub-key)
			                                            %default-authorized-guix-keys))))))))


