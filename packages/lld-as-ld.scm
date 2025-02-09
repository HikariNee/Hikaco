;; this module only exists because lld-as-ld-wrapper is out of date.
(define-module (hikaco packages lld-as-ld)
  #:use-module (gnu packages llvm)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (guix gexp))

(define* (make-lld-wrapper lld #:key lld-as-ld?)
  "Return a LLD wrapper.  When LLD-AS-LD? is true, create a 'ld' symlink that
points to 'lld'."
  (package
   (inherit lld)
   (name (if lld-as-ld? "lld-as-ld-wrapper" "lld-wrapper"))
   (source #f)
   (native-inputs '())
   (inputs (list (make-ld-wrapper "ld.lld-wrapper" #:binutils lld
                                  #:linker "ld.lld")
                 (make-ld-wrapper "lld-wrapper" #:binutils lld #:linker
                                  "lld")))
   (propagated-inputs '())
   (build-system trivial-build-system)
   (arguments
    (list #:builder
          #~(let ((ld.lld (string-append #$(this-package-input
                                            "ld.lld-wrapper")
                                         "/bin/ld.lld"))
                  (lld (string-append #$(this-package-input "lld-wrapper")
                                      "/bin/lld")))
              (mkdir #$output)
              (mkdir (string-append #$output "/bin"))
              (symlink ld.lld (string-append #$output "/bin/ld.lld"))
              (symlink lld (string-append #$output "/bin/lld"))
              (when #$lld-as-ld?
                (symlink ld.lld (string-append #$output "/bin/ld"))))))
   (synopsis "LLD linker wrapper")
   (description "This is a linker wrapper for LLD; like @code{ld-wrapper}, it
wraps the linker to add any missing @code{-rpath} flags, and to detect any
misuse of libraries outside of the store.")))

(define-public lld19-as-ld-wrapper
  (make-lld-wrapper lld-19 #:lld-as-ld? #t))
