;;; init.el --- Description -*- lexical-binding: t; -*-

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
(setq ++packages '(use-package evil))
(dolist (++p ++packages)
  (straight-use-package ++p))

(use-package evil
  :config (evil-mode t))
