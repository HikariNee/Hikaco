(define-module (hikaco services river))

(define-syntax call/river
  (syntax-rules ()
    ((call/river 'spawn mod key program) `(system* "riverctl" "map" "normal" ,mod ,key "spawn" ,program))))

(call/river 'spawn "Super" "E" "brave")
