;;; package --- init.el
;;; Commentary:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop
                   (apply #'call-process
                          `("git" nil ,buffer t "clone" ,@
                            (when-let ((depth (plist-get order :depth)))
                              (list
                               (format "--depth=%d" depth)
                               "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
                 ((zerop
                   (call-process "git"
                                 nil
                                 buffer
                                 t
                                 "checkout"
                                 (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop
                   (call-process emacs
                                 nil
                                 buffer
                                 nil
                                 "-Q"
                                 "-L"
                                 "."
                                 "--batch"
                                 "--eval"
                                 "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error
           "%s"
           (with-current-buffer buffer
             (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package (elpaca-use-package-mode))

(use-package
 emacs
 :hook
 ((after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . savehist-mode)
  (prog-mode . display-line-numbers-mode))

 :init
 (add-to-list 'default-frame-alist '(font . "Pragmasevka Nerd Font-13"))
 (electric-pair-mode 1)
 (display-time-mode 1)
 (scroll-bar-mode 0)
 (tool-bar-mode 0)
 (menu-bar-mode 0)
 (blink-cursor-mode -1)
 (pixel-scroll-precision-mode)

 (setopt history-length 25)

 (savehist-mode 1)
 (save-place-mode 1)
 (delete-selection-mode 1)
 (electric-indent-mode 1)
 (global-auto-revert-mode 1)

 (setopt custom-file (locate-user-emacs-file "custom-vars.el"))
 (load custom-file 'noerror 'nomessage)


 (setopt electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 (setopt undo-limit (* 64 1024 1024)) ; 64 mb
 (setopt read-process-output-max (* 1024 1024 4))

 (setopt undo-strong-limit (* 96 1024 1024))
 (setopt undo-outer-limit (* 90 1024 1024))

 (setopt fill-column 80)
 (setopt frame-resize-pixelwise t)
 (setopt split-width-threshold 80)
 (setopt create-lockfiles nil)
 (setopt initial-scratch-message "")

 (setopt make-backup-files nil)
 (setopt auto-save-list-file-prefix "~/.config/emacs/autosave/")
 (setopt auto-save-file-name-transforms '((".*" "~/.config/emacs/autosave" t)))
 (setopt inhibit-startup-message t)
 (setopt load-prefer-newer t)
 (setopt select-enable-clipboard t)
 (setopt confirm-nonexistent-file-or-buffer t)
 (setopt fast-but-imprecise-scrolling t)
 (setopt auto-save-default t)
 (setopt use-short-answers t)
 (setopt warning-minimum-level :emergency)


 (setopt auto-revert-avoid-polling t)
 (setopt auto-revert-interval 5)
 (setopt initial-major-mode 'fundamental-mode)
 (setopt sentence-end-double-space nil)
 (setopt x-underline-at-descent-line nil)
 (setopt pgtk-wait-for-event-timeout 0)

 (setopt display-line-numbers-width 3)
 (setopt left-fringe-width 5)
 (setopt right-fringe-width 5)
 (setopt truncate-lines t)
 (setopt indent-tabs-mode nil)
 (setopt tab-width 2)

 (setopt treesit-font-lock-level 4)
 (setopt inhibit-startup-echo-area-message (user-login-name))
 (setopt tab-always-indent 'complete)
 (setopt
  read-extended-command-predicate #'command-completion-default-include-p)

 (setopt send-mail-function 'sendmail-send-it)
 (setopt sendmail-program "/usr/local/bin/msmtp")
 (setopt mail-specify-envelope-from t)
 (setopt message-sendmail-envelope-from 'header)
 (setopt mail-envelope-from 'header))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package
 jinx
 :ensure t
 :hook (after-init . global-jinx-mode)
 :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages)))

(use-package elisp-autofmt :ensure t)

(use-package
 treesit-auto
 :ensure t
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode 1))

(use-package
 rainbow-delimiters
 :ensure t
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package
 snow
 :ensure t
 :if (= 12 (string-to-number (format-time-string "%m")))
 :init (snow))

(use-package
 exec-path-from-shell
 :ensure t
 :hook (after-init . exec-path-from-shell-initialize))

(use-package
 project
 :config
 (setopt project-vc-extra-root-markers '(".project.el" ".projectile")))

(use-package
 undo-fu
 :ensure t
 :bind
 ("C-z" . undo-fu-only-undo)
 ("C-S-z" . undo-fy-only-redo))

(use-package notmuch :ensure t)

(use-package
 corfu
 :ensure t
 :custom
 (corfu-auto t)
 (corfu-preselect 'first)
 (corfu-cycle t)
 (corfu-popupinfo-delay '(0.8 . 0.8))
 (corfu-quit-no-match 'separator)
 :config (keymap-unset corfu-map "RET")
 :init
 (global-corfu-mode 1)
 (corfu-popupinfo-mode 1))

(use-package
 kind-icon
 :ensure t
 :after corfu
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package
 orderless
 :ensure t
 :custom
 (completion-styles '(orderless flex))
 (completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia :ensure t :init (marginalia-mode))

(use-package
 doom-modeline
 :ensure t
 :init (doom-modeline-mode)
 :custom
 (doom-modeline-project-detection 'auto)
 (doom-modeline-height 25)
 (doom-modeline-bar-width 1)
 (doom-modeline-icon t)
 (doom-modeline-major-mode-icon t)
 (doom-modeline-major-mode-color-icon t)
 (doom-modeline-buffer-state-icon t)
 (doom-modeline-buffer-modification-icon t)
 (doom-modeline-minor-modes nil)
 (doom-modeline-enable-word-count nil)
 (doom-modeline-buffer-encoding t)
 (doom-modeline-indent-info nil)
 (doom-modeline-checker-simple-format t)
 (doom-modeline-vcs-max-length 12)
 (doom-modeline-env-version t)
 (doom-modeline-irc-stylize 'identity)
 (doom-modeline-github-timer nil)
 (doom-modeline-gnus-timer nil))

(use-package nerd-icons :ensure t)

(use-package
 standard-themes
 :ensure t
 :custom
 (standard-themes-bold-constructs t)
 (standard-themes-italic-constructs t)
 :init
 (load-theme 'standard-light :no-confirm))

(use-package vertico :ensure t :init (vertico-mode))

(use-package
 consult
 :ensure t
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :bind
 (("C-x b" . consult-buffer)
  ("M-g M-g" . consult-goto-line)
  ("M-s d" . consult-fd)
  ("M-s r" . consult-ripgrep)
  ("C-y" . consult-yank-from-kill-ring)
  ("M-g f" . consult-flymake)
  ("M-g e" . consult-compile-error)
  ("C-s" . consult-line)))

(use-package haskell-mode :ensure t)

(use-package
 geiser
 :ensure t
 :custom
 (geiser-default-implementation 'guile)
 (geiser-active-implementation '(guile))
 (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package
 geiser-guile
 :ensure t
 :after geiser
 :init
 (add-to-list 'geiser-guile-load-path "~/opt/guix")
 (add-to-list 'geiser-guile-load-path "~/.config/sysguix/hikaco"))

(use-package puni :ensure t :init (puni-global-mode))

(use-package
 eglot
 :hook
 ((haskell-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure))

 :init
 (setopt
  eglot-sync-connect
  1
  eglot-connect-timeout
  5
  eglot-autoshutdown
  t
  eglot-send-changes-idle-time
  45)

 (setopt
  completion-category-overrides
  '((eglot (styles orderless)) (eglot-capf (styles orderless)))

  eglot-workspace-configuration
  '((haskell
     (plugin
      (stan (globalOn . :json-false))
      (splice (globalOn . :json-false))
      (eval (globalOn . :json-false)))
     (formattingProvider "fourmolu")))))
(provide 'init)
;;; init.el ends here
