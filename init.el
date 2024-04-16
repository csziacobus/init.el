;;;; -*- lexical-binding: t; -*-
;;;; user init file

;; Turn off mouse interface early in startup to avoid momentary display
(tooltip-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)

;;;; Settings

;; some simple settings
(setq column-number-mode t
      ;; make backups of version-controlled files
      vc-make-backup-files t
      ;; no splash screen
      inhibit-startup-message t
      backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups"))))
      load-prefer-newer t
      tab-always-indent 'complete
      mac-command-modifier 'meta)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Make prompts 'y or n' instead of "yes" or "no"
(fset 'yes-or-no-p #'y-or-n-p)

;; show full pathname
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

;;; package initialization

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;;; use-package macro bootstrapping
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package diminish)

(use-package bind-key)

;; Save point position between sessions

(use-package saveplace
  :init (save-place-mode t))

(use-package page-break-lines
  :disabled
  :config (page-break-lines-mode))

;;; Functions
;; Display function next to major mode
(use-package which-func
  :init (which-function-mode))

;; magit
(use-package magit
  :bind ("C-c v" . magit-status))

(use-package forge
  :after magit)

;;;; Bindings
(use-package eshell
  :bind ("C-c C-s" . eshell))

;;;; Tex and LaTex
(use-package tex-site
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    ;; latex pdf
    (setq latex-run-command "pdflatex")
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (turn-on-auto-fill)
                (flyspell-mode)
                (LaTeX-math-mode)
                (turn-on-reftex)
                (TeX-fold-mode 1)))
    (setq reftex-plug-into-AUCTeX t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (company-tng-configure-default)
  :bind (("TAB" . company-indent-or-complete-common)))

(use-package company-quickhelp
  :after company
  :hook (after-init . company-quickhelp-mode))

(use-package slime
  :defer t
  :bind (("C-c SPC" . slime)
         ("C-c s" . slime-selector)
         ("M-/" . completion-at-point))
  :config
  (progn
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-fancy
                   slime-company
                   slime-indentation
                   ;slime-js
                   slime-xref-browser
                   ; slime-asdf
                   slime-presentations
                   slime-mrepl
                   slime-sprof
                   slime-mdot-fu))
    ;; customizations
    (setq lisp-lambda-list-keyword-parameter-alignment t
          lisp-lambda-list-keyword-alignment t
          slime-load-failed-fasl 'always
          slime-repl-history-remove-duplicates t
          slime-repl-history-trim-whitespaces t
          slime-lisp-implementations
          '((sbcl ("sbcl"))
            (ccl ("~/bin/ccl64"))
            (ecl ("ecl"))
            (clisp ("clisp"))
            (cmucl ("cmucl"))
            (clisp-dev ("~/clisp/src/full/lisp.run" "-M/home/charliezhang/clisp/src/full/lispinit.mem"))
            (sbcl-dev ("~/sbcl/run-sbcl.sh"))
            (clasp-dev ("~/clasp/build/clasp"))
            (bclasp ("~/clasp/build/boehm/iclasp-boehm" "-i/home/charliezhang/clasp/build/boehm/fasl/bclasp-boehm-image.fasl")))
          slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))
    (set (make-local-variable lisp-indent-function)
         'common-lisp-indent-function)
    ;; autodoc fix
    (eldoc-add-command 'slime-space)
    (make-directory "/tmp/slime-fasls/" t)
    (put 'set-macro-character 'common-lisp-indent-function 1)
    (put 'set-dispatch-macro-character 'common-lisp-indent-function 2)
    (add-hook 'slime-repl-mode-hook (lambda () (font-lock-mode -1)))))

(use-package slime-company
  :after (slime company))

;;; add equalp and fix mdot
; (use-package cl-lib)

;; highlight numbers
(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

;; display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :config (global-prettify-symbols-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-mode-hook . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode-hook . flycheck-rust-setup))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package paredit
  :preface
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  :hook ((emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          slime-repl-mode)
         . paredit-mode)
  :hook (slime-repl-mode . override-slime-repl-bindings-with-paredit))

(use-package multiple-cursors
  :disabled
  :bind (("C-M-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package helm
  :defer 1
  :diminish helm-mode
  :config
  (progn
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match    t)
    (helm-mode)
    (helm-autoresize-mode))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c i" . helm-semantic-or-imenu))
  :bind (:map helm-map
              ;; rebind tab to do persistent action
              ("<tab>" . helm-execute-persistent-action)
              ;; make TAB works in terminal
              ("C-i" . helm-execute-persistent-action)
              ;; list actions using C-z
              ("C-z" . helm-select-action)))

(use-package helm-projectile
  :after helm
  :init
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-bibtex
  :after helm)

;; JS
(use-package skewer-mode
  :hook (js2-mode-hook . skewer-mode))

(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-program "/home/charliezhang/firefox/firefox")

;; ELDOC
(use-package eldoc
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode)
         . eldoc-mode))

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :bind (:map elpy-mode-map
              ("C-c C-c" . elpy-shell-send-top-statement)
              ("C-x C-e" . elpy-shell-send-statement)
              ("C-c C-r" . elpy-shell-send-region-or-buffer)))

;; spelling
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package org
  :defer t
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-return-follows-link t)
  (add-to-list 'org-export-backends 'beamer)
  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode 1)
                             (flyspell-mode 1)))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-file-apps
                    (remove '("\\.pdf\\'" . default) org-file-apps))
              (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))))

(use-package org-ref)

(use-package lsp-mode
  :ensure t
  :hook ((rust-mode . lsp))
  :commands lsp)

(use-package helm-lsp :ensure t)

(use-package ripgrep :ensure t)

(use-package sml-mode
  :mode "\\.sml\\'"
  :config (add-hook 'sml-mode-hook (lambda ()
                                     (electric-indent-local-mode -1))))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (add-hook 'before-save-hook
                                           'haskell-mode-format-imports
                                           nil
                                           'local))))

(use-package clang-format
  :config (setq clang-format-style-option "llvm"))

;; zenburn theme
(use-package zenburn-theme
  :demand t
  :config (set-face-attribute 'region nil :background "#666"))

(load-theme 'zenburn t)

;; FACES
(use-package paren
  :hook (paredit-mode . show-paren-mode)
  :config
  (progn
    (set-face-background 'show-paren-match "#0066ff")
    (set-face-foreground 'show-paren-match "#def")))

(setq-default indent-tabs-mode nil)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;; Unfill/fill
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

(global-set-key [(control c) (d)] 'compile)

(put 'erase-buffer 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'eww-browse-url)
 '(custom-safe-themes
   '("2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(helm-external-programs-associations '(("pdf" . "evince")))
 '(org-agenda-files '("~/Dropbox/berkeley/ling55/paper/paper.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(lsp-mode lsp wgrep forge magit-gh-pulls clang-format cov geiser m2-mode m2 flycheck-rust elisp-slime-nav skewer-mode skewer elpy-mode markdown-mode slime-company company-quickhelp rainbow-delimiters-mode company org-ref haskell-mode org helm-bibtex elpy mu4e sml-mode sml git-commit helm-descbinds multiple-cursors page-break-line zenburn-theme use-package rust-mode rainbow-delimiters paredit magit highlight-symbol highlight-numbers helm auctex slime))
 '(preview-scale-function 4.0)
 '(safe-local-variable-values
   '((c-file-offsets
      (innamespace . 0)
      (substatement-open . 0)
      (c . c-lineup-dont-change)
      (inextern-lang . 0)
      (comment-intro . c-lineup-dont-change)
      (arglist-cont-nonempty . c-lineup-arglist)
      (block-close . 0)
      (statement-case-intro . ++)
      (brace-list-intro . ++)
      (cpp-define-intro . +))
     (c-auto-align-backslashes)
     (whitespace-style quote
                       (face trailing empty tabs))
     (whitespace-action)))
 '(send-mail-function 'smtpmail-send-it)
 '(slime-autodoc-delay 0.2)
 '(slime-autodoc-use-multiline-p nil)
 '(slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 139 :width normal))))
 '(slime-repl-inputed-output-face ((t (:foreground "coral")))))
