;;; init.el --- Description -*- lexical-binding: t; -*-

;; (setq debug-on-error t)

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
                   vertico-posframe
                   orderless
                   consult
                   lsp-mode
                   lsp-ui
                   corfu
                   corfu-doc
                   kind-icon
                   cape
                   general
                   keycast
                   ;; lang modes
                   typescript-mode
                   web-mode))
(dolist (+p +packages)
  (straight-use-package +p))

;; --- Built-in Settings ---

(use-package emacs
  :init
  (setq minibuffer-message-timeout 0.0
        inhibit-message t)
  (recentf-mode t)
  (++setup-font)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.[j|t]sx\\'" . web-mode))
  (general-define-key
   :states '(normal)
   "<escape>" #'keyboard-quit))

;; --- Evil ---
(use-package evil
  :config (evil-mode t))

;; --- Vertico ---
(use-package vertico-multiform
  :after vertico
  :load-path "straight/repos/vertico/extensions/"
  :config (vertico-multiform-mode))
(use-package vertico-buffer
  :after vertico
  :load-path "straight/repos/vertico/extensions/")
(use-package vertico-posframe
  :after vertico
  :init
  (setq vertico-posframe-border-width 1
        vertico-posframe-parameters '((left-fringe . 10)
                                      (right-fringe . 10))))
(use-package vertico
  :init
  (when (display-graphic-p)
    (require 'vertico-posframe)
    ;; Configure the display per command.
    ;; Use a buffer with indices for imenu
    ;; and a flat (Ido-like) menu for M-x.
    (setq vertico-multiform-commands
          '((execute-extended-command posframe)
            (helpful-callable posframe)
            (helpful-variable posframe)
            (find-file posframe)
            (projectile-find-file posframe)
            (projectile-switch-project posframe)
            (consult-recent-file posframe)
            (consult-bookmark buffer)
            (consult-imenu buffer)
            (yas-insert-snippet posframe)
            (lsp-execute-code-action posframe)))
    ;; Configure the display per completion category.
    ;; Use the grid display for files and a buffer
    ;; for the consult-grep commands.
    (setq vertico-multiform-categories
          '((consult-grep buffer))))
  :config
  (vertico-mode t)
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   ":" #'execute-extended-command)
  (general-define-key
   :keymaps '(override vertico-map)
   "C-l" #'vertico-insert
   "C-;" #'vertico-exit))
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
(use-package lsp-ui :commands lsp-ui-mode)

;; --- Corfu and co. ---
(use-package corfu-doc
  :config
  (setq corfu-doc-delay 0.2
        corfu-doc-max-width 80
        corfu-doc-max-height 40))

(use-package corfu
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.01
        corfu-separator ?\s
        corfu-quit-at-boundary nil
        corfu-quit-no-match t
        corfu-preview-current nil
        corfu-preselect-first t
        corfu-on-exact-match nil
        corfu-echo-documentation nil
        corfu-scroll-margin 10)
  (general-define-key
   :states '(normal insert visual)
   :keymaps 'global-map
   "C-SPC" #'completion-at-point)
  (general-define-key
   :states '(normal insert visual)
   :keymaps 'corfu-map
   "<escape>" #'corfu-quit
   "ESC" #'corfu-quit)
  (general-define-key
   :states '(insert)
   :keymaps '(corfu-map override)
        "C-j" #'corfu-next
        "C-k" #'corfu-previous
        "C-l" #'corfu-insert
        "C-;" #'corfu-insert
        "TAB" #'corfu-insert
        "<tab>" #'corfu-insert)
  (global-corfu-mode +1)
  ;; (global-company-mode -1)
  (dolist (+m '(prog-mode-hook text-mode-hook))
    (add-hook +m (lambda () (corfu-doc-mode +1)))))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; See https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some))

;; --- (HEADING) ---
;; TODOs
;; - Corfu
;; - Keybindings (SPC leader)
;; - Embark
;; - Marginalia
;; - Theme
;; - Better jumper
;; - Magit
;; - Org
;; - Window/buffer manipulation keybindings
;; - Vterm
