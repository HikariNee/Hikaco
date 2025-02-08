;; package  --- Summary
;;; Commentary:

;;; Code:
;; --------------------------------- BOOTSTRAP ------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'package)
(eval-and-compile
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)

(setopt leaf-alias-keyword-alist '((:ensure . :straight)))
;; ------------------------------- END BOOTSTRAP -------------------------------------

(leaf standard-themes
  :ensure t
  :init
  (load-theme 'standard-light :no-confirm))

(leaf emacs
  :hook ((c++-ts-mode . eglot-ensure))
  :config
  (add-to-list 'default-frame-alist '(font . "Pragmasevka Nerd Font-13"))

  ;; Some minor modes I like.
  (electric-pair-mode)
  (display-time-mode)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (delete-selection-mode)
  (electric-indent-mode)
  (global-auto-revert-mode)
  (fido-mode)
  (fido-vertical-mode)
  (global-display-line-numbers-mode)

  ;; Customize variables are yucky.
  (setopt custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; QoL
  (setopt electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setopt create-lockfiles nil)
  (setopt make-backup-files nil)
  (setopt select-enable-clipboard t)
  (setopt load-prefer-newer t)
  (setopt use-short-answers t)
  (setopt package-install-upgrade-built-in t)
  (setopt flymake-no-changes-timeout 2)
  (setopt completion-auto-help nil)
  (setopt inhibit-startup-echo-area-message (user-login-name))
  (setopt inhibit-startup-screen t)
  (setopt backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setopt auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  ;; Default buffer isn't nice.
  (setopt initial-major-mode 'fundamental-mode)
  (setopt initial-scratch-message "")
  
  (setopt sentence-end-double-space nil)
  
  ;; Disable Tabs!
  (setq-default indent-tabs-mode nil)
  (setopt tab-always-indent 'complete)

  ;; Eglot stuff.
  (setopt eglot-sync-connect 1)
  (setopt eglot-send-changes-idle-time 45)
  (setopt eglot-autoshutdown t)
  (setopt eglot-connect-timeout 5)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))


;; Popup childframes are nice ++ Eyecandy!
(leaf mini-echo
  :ensure t
  :config
  (mini-echo-mode))

(leaf eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :init
  (eldoc-box-hover-mode))

(leaf flymake-popon
  :ensure t
  :init
  (global-flymake-popon-mode))

;; Treesitter modes are better, apparently.
(leaf treesit-auto
  :ensure t
  :commands global-treesit-auto-mode
  :config
  (global-treesit-auto-mode))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Simpler undo.
(leaf undo-fu
  :ensure t
  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

;; Email client!
(leaf notmuch
  :ensure t)

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode))

;; More QoL things.
(leaf consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("M-g M-g" . consult-goto-line)
   ("M-s M-d" . consult-fd)
   ("M-s M-r" . consult-grep)
   ("C-y" . consult-yank-from-kill-ring)
   ("M-g M-f" . consult-flymake)
   ("M-g M-e" . consult-compile-error)
   ("C-s" . consult-line))
  :init
  (setopt consult-grep-args
          '("ugrep" (consult--grep-exclude-args)
            "--null --line-buffered --color=never --ignore-case --with-filename --line-number -I -r")))

(leaf puni
  :ensure t
  :init
  (puni-global-mode))

(leaf orderless
  :ensure t
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf corfu
  :ensure t
  :config
  (setopt corfu-auto t)
  (keymap-unset corfu-map "RET")
  :init
  (global-corfu-mode))

(leaf cape
  :ensure t)

;; Programming modes! (Despair)
(leaf haskell-mode
  :ensure t)

(leaf geiser
  :ensure t)

(leaf geiser-guile
  :ensure t
  :after geiser
  :config
  (add-to-list 'geiser-guile-load-path "~/opt/guix")
  (add-to-list 'geiser-guile-load-path "~/.config/sysguix/hikaco"))

(leaf eglot-booster
 :ensure (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
 :after eglot
 :config (eglot-booster-mode))

(provide 'init)
;;; init.el ends here.
