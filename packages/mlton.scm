(define-module (hikaco packages mlton)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (guix utils))

(define mlton-inputs
  '("glibc" "gmp"))

(define-public mlton
  (package
   (name "mlton")
   (version "20241230")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/MLton/mlton/releases/download/on" version "-release/mlton-" version "-1" ".amd64-linux.ubuntu-24.04_glibc2.39.tgz"))
     (sha256
      (base32 "14qm92xgj02hgqa50g3i721ng88bsfmjympz5kxyw6hnfy6fgmcm"))))
   (build-system binary-build-system)
   (arguments
    (list
     #:patchelf-plan
     #~'(("./lib/mlton/mlton-compile" #$mlton-inputs)
         ("./bin/mllex"  #$mlton-inputs)
         ("./bin/mlyacc" #$mlton-inputs)
         ("./bin/mlprof" #$mlton-inputs)
         ("./bin/mlnlffigen" #$mlton-inputs))
     #:install-plan
     #~'(("./lib/" "lib/")
         ("./bin/" "bin/")
         ("./share/" "share/"))
     #:phases
     #~(modify-phases %standard-phases
                      (add-after 'install 'replace-cc-with-gcc
                                 (lambda _
                                   (let* ((mlton (string-append #$output "/bin/mlton")))
                                     (substitute* mlton
                                                  (("\\$CC") #$(file-append gcc "/bin/gcc"))
                                                  (("\\$gmpCCOpts") (string-append "-cc-opt " "-I" #$gmp "/include"))
                                                  (("\\$gmpLinkOpts") (string-append "-link-opt " "-L" #$gmp "/lib")))))))))
   (inputs (list gmp glibc gcc))
   (synopsis "MLton is a whole-program optimizing compiler for the Standard ML programming language. ")
   (description "MLton generates small executables with excellent runtime performance.")
   (home-page "https://github.com/MLton/mlton")
   (license hpnd)))
