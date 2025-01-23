(define-module (hikaco packages lamdera)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build utils)
  #:use-module (nonguix build-system binary))

(define-public lamdera
  (package
   (name "lamdera")
   (version "1.3.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://static.lamdera.com/bin/lamdera-" version "-linux-x86_64"))
     (sha256
      (base32 "1ypcbia2z1y8nxvwwsxqrjyliksgl02qviiikrs52mhmk3x9p9hm"))))
   (build-system binary-build-system)
   (arguments
    (list
     #:install-plan
     #~ '(("lamdera"  "bin/"))
     #:phases #~(modify-phases %standard-phases
			    (add-before 'install 'rename-file
					(lambda _ 
					  (let* ()
					    (rename-file "lamdera-1.3.2-linux-x86_64" "lamdera")
					    (chmod "lamdera" #o755)))))))
   (home-page "https://dashboard.lambera.app")
   (synopsis "Binary build of the Lambera elm compiler.")
   (description "meow")
   (license bsd-3)))
