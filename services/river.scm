(define-module (hikaco services river)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages zig-xyz))

(define-syntax call/river
  (syntax-rules ()
    ((call/river 'spawn mod key program)
     `(system* "riverctl" "map" "normal" ,mod ,key "spawn" ,program))
     ((call/river 'focus-view mod key opt)
     `(system* "riverctl" "map" "normal" ,mod ,key "focus-view" ,opt))
     ((call/river 'swap mod key opt)
     `(system* "riverctl" "map" "normal" ,mod ,key "swap" ,opt))
     ((call/river 'focus-output mod key opt)
      `(system* "riverctl" "map" "normal" ,mod ,key "focus-output" ,opt))
     ((call/river 'move mod key direction int)
      `(system* "riverctl" "map" "normal" ,mod ,key "move" ,direction ,int))
     ((call/river 'map-pointer mod key opt)
      `(system* "riverctl" "map-pointer" ,mod ,key ,opt))
     ((call/river 'set-background-color color)
      `(system* "riverctl" "background-color" ,color))
     ((call/river 'set-border-color-focused color)
      `(system* "riverctl" "border-color-focused" ,color))
     ((call/river 'set-border-color-unfocused color)
      `(system* "riverctl" "border-color-unfocused" ,color))
     ((call/river 'set-repeat a b)
      `(system* "riverctl" "set-repeat" ,a ,b))
     ((call/river 'set-default-layout layout)
      `(system* "riverctl" "default-layout" ,layout))))

(define-public (river-spawn-alist alist)
  (let ((fsplit (lambda (x) (string-split (car x) #\-))))
    (map
     (lambda (x)
       (call/river 'spawn (car (fsplit x)) (car (cdr (fsplit x))) (cdr x)))
     alist)))

(define-record-type* <river-configuration>
  river-configuration make-river-configuration
  river-configuration?
  (keybinds river-configuration-keybinds))

(define-public (home-river-shepherd-service config)
  (define config-file #~(gexp->script (river-spawn-alist (river-keybinds config))))
  (list (shepherd-service
         (documentation "RiverWM.")
         (requirement '(user-processes elogind dbus polkit))
         (provision 'riverwm)
         (start #~(make-forkexec-constructor #$(file-append river "-c" config-file)))
         (stop #~(make-kill-destructor)))))

