(define-module (hikaco packages scx)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix utils))

(define-public scx-scheds
  (package
   (name "scx-scheds")
   (version "1.0.8")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
             (url "https://github.com/sched-ext/scx")
             (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0bz9v5siis42hvr9qqblvrpvqc5cjfs56c4z3v3jzw72zgss4yvr"))))
   (build-system meson-build-system)
   (arguments
    (list
     #:configure-flags (list 
       "-D" #~(string-append "libbpf_a=" #$(file-append libbpf "/lib/libbpf.a")) 
       "-D" #~(string-append "libbpf_h=" #$(file-append libbpf "/include/bpf")) 
       "-D" "enable_rust=false")))
   (native-inputs (map specification->package+output (list "clang" "libbpf" "bpftool" "jq" "pkg-config")))
   (synopsis "h")
   (description "b")
   (home-page "c")
   (license gpl2)))
