(define-module (hikaco packages pragmasevka)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module (guix licenses)
  #:use-module (guix utils))

(define-public pragmasevka
  (package
   (name "pragmasevka")
   (version "v1.6.6")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri "https://github.com/shytikov/pragmasevka/releases/download/v1.6.6/Pragmasevka_NF.zip")
     (sha256
      (base32 "12m8zyqclwzgr4ydzk7bcjk20zkv3mlfjbcmm3w31ys66l24l8al"))))
   (build-system font-build-system)
   (home-page "https://github.com/shytikov/pragmasevka/")
   (synopsis "Pragmata Pro doppelgänger made of Iosevka SS08")
   (description "Pragmata Pro doppelgänger")
   (license silofl1.1)))
