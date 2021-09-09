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
      tab-always-indent 'complete)

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
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package slime
  :defer t
  :bind (("C-c SPC" . slime)
         ("C-c s" . slime-selector)
         ("M-/" . completion-at-point))
  :config
  (progn
    (use-package ac-slime)
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-fancy
                   slime-indentation
                                        ;slime-js
                   slime-xref-browser
                   slime-asdf
                   slime-presentations
                   slime-presentation-streams
                   slime-mrepl
                   slime-mdot-fu
                   ))
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
            (sbcl-dev ("~/sbcl/run-sbcl.sh")))
          slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/")
          slime-load-failed-fasl 'always)
    (set (make-local-variable lisp-indent-function)
         'common-lisp-indent-function)
    ;; autodoc fix
    (eldoc-add-command 'slime-space)
    (make-directory "/tmp/slime-fasls/" t)
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (put 'set-macro-character 'common-lisp-indent-function 1)
    (put 'set-dispatch-macro-character 'common-lisp-indent-function 2)
    (add-hook 'slime-repl-mode-hook (lambda () (font-lock-mode -1)))))

;;; add equalp and fix mdot
(use-package cl
  :after slime)

;; highlight numbers
(use-package highlight-numbers
  :commands highlight-numbers-mode
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :config (global-prettify-symbols-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :commands cargo-minor-mode
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package paredit
  :commands my-paredit-hook
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       #'my-paredit-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'my-paredit-hook)
    (add-hook 'ielm-mode-hook             #'my-paredit-hook)
    (add-hook 'lisp-mode-hook             #'my-paredit-hook)
    (add-hook 'lisp-interaction-mode-hook #'my-paredit-hook)
    (add-hook 'scheme-mode-hook           #'my-paredit-hook)
    (add-hook 'slime-repl-mode-hook #'my-paredit-hook)

    (modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
    (modify-syntax-entry ?] ")[" lisp-mode-syntax-table))
  :config
  (progn
    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (defun paredit-wrap-square-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-square))

    (defun paredit-wrap-curly-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-curly))

    (defun paredit-kill-region-or-backward-word ()
      (interactive)
      (if (region-active-p)
          (kill-region (region-beginning) (region-end))
        (paredit-backward-kill-word)))

    (defvar electrify-return-match
      "[\]}\)\"]"
      "If this regexp matches the text after the cursor, do an \"electric\" return.")

    (defun electrify-return-if-match (arg)
      "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
      (interactive "P")
      (let ((case-fold-search nil))
        (if (looking-at electrify-return-match)
            (save-excursion (newline-and-indent)))
        (newline arg)
        (indent-according-to-mode)))

    (defun my-paredit-hook ()
      (enable-paredit-mode)
      (eldoc-mode)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)
      (local-set-key (kbd "RET") 'electrify-return-if-match)
      (eldoc-add-command 'electrify-return-if-match)
      (show-paren-mode t)
      (define-key paredit-mode-map (kbd "M-(")
        'paredit-wrap-round)
      (define-key paredit-mode-map (kbd "M-)")
        'paredit-wrap-round-from-behind)
      (define-key paredit-mode-map (kbd "M-]")
        'paredit-wrap-square)
      (define-key paredit-mode-map (kbd "M-[")
        'paredit-wrap-square-from-behind)
      (define-key paredit-mode-map (kbd "M-s-[")
        'paredit-wrap-curly)
      (define-key paredit-mode-map (kbd "M-s-]")
        'paredit-wrap-curly-from-behind)
      ;; CAVE: Zaps X Server by default
      (define-key paredit-mode-map (kbd "M-C-<backspace>")
        'backward-kill-sexp)
      (define-key paredit-mode-map (kbd "\\")
        nil)
      (define-key paredit-mode-map (kbd "C-w")
        'paredit-kill-region-or-backward-word))

    ;; Stop SLIME's REPL from grabbing DEL,
    ;; which is annoying when backspacing over a '('
    (defun override-slime-repl-bindings-with-paredit ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))

    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)))

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
    (require 'helm-config)
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
  :after helm)

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;; ELDOC
(use-package eldoc
  :commands eldoc-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)))

;; spelling
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package org
  :defer t
  :config (add-hook 'org-mode-hook (lambda ()
                                     (auto-fill-mode 1)
                                     (flyspell-mode 1))))

(use-package sml-mode
  :mode "\\.sml\\'"
  :config (add-hook 'sml-mode-hook (lambda () (electric-indent-mode nil))))

(use-package clang-format
  :config (setq clang-format-style-option "llvm"))

;; zenburn theme
(use-package zenburn-theme
  :demand t
  :config (set-face-attribute 'region nil :background "#666"))

;; FACES
(use-package paren
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

(global-set-key [(control c) (c)] 'compile)
(global-set-key [(control x) (c)] 'toggle-window-split)
(put 'erase-buffer 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(custom-safe-themes
   (quote
    ("6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (clang-format cov sml-mode sml git-commit helm-descbinds multiple-cursors page-break-line zenburn-theme use-package rust-mode rainbow-delimiters paredit magit highlight-symbol highlight-numbers helm auctex slime)))
 '(preview-scale-function 2.0)
 '(slime-autodoc-delay 0.2)
 '(slime-autodoc-use-multiline-p nil)
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 139 :width normal))))
 '(slime-repl-inputed-output-face ((t (:foreground "coral")))))
