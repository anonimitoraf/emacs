;;; init.el --- Description -*- lexical-binding: t; -*-

(add-to-list 'load-path "./sections")

(require 'cl-lib)
(require '+appearance)

;; Startup optimizations
(setq gc-cons-threshold 10000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Install straight.el (for painless package installation)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Install packages
(setq +packages '(use-package
                   evil
                   vertico
                   orderless
                   consult
                   lsp-mode
                   lsp-ui
                   ;; lang modes
                   typescript-mode
                   web-mode))
(dolist (+p +packages)
  (straight-use-package +p))

;; --- General Settings ---

(use-package emacs
  :init
  (recentf-mode t)
  (++setup-font)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.[j|t]sx\\'" . web-mode)))

;; --- Evil ---
(use-package evil
  :config (evil-mode t))

;; --- Vertico ---
(use-package vertico
  :config (vertico-mode t))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

;; --- Orderless ---
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; --- Consult ---
(use-package consult
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window))

;; --- LSP ---
(use-package lsp-mode
  :hook ((prog-mode . lsp))
  :commands lsp)
;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; --- (HEADING) ---
;; TODOs
;; - LSP
;; - Keybindings (SPC leader)
;; - Embark
;; - Marginalia
;; - Theme
;; - Better jumper
;; - Magit
;; - Org
;; - Window/buffer manipulation keybindings
