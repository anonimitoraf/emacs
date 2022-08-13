(defvar ++font-size nil)
(defun ++screen-pixels->font-size (width-x-height)
  "Given WIDTH_X_HEIGHT, returns the adjusted font size"
  (let ((default-font-size 16))
    (cond ((member width-x-height
                   '((3440 1440))) 18)
          ((member width-x-height
                   '((1920 1080))) 14)
          ;; My Flux mac
          ((member width-x-height
                   '((1440 900))) 16)
          (t (progn
               (message (concat "Unhandled screen resolution " (prin1-to-string width-x-height) ". "
                                "Defaulting to font size " (prin1-to-string default-font-size)))
               default-font-size)))))
;; Stolen from https://github.com/hlissner/doom-emacs/issues/1500
(defun ++get-frame-list (&optional frame)
  "Return a list consisting of FRAME and all of FRAME's child frames."
  (let ((frame (or frame (selected-frame))))
    (cons (selected-frame)
          (cl-loop for fr in (frame-list)
                   if (eq (frame-parameter fr 'parent-frame) frame)
                   collect fr))))

(defun ++configure-font-size ()
  (let ((new-font-size (++screen-pixels->font-size
                        (cddr (frame-monitor-attribute 'geometry)))))
    (message "New font size: %s" new-font-size)
    (unless (equal new-font-size ++font-size)
      (setq ++font (font-spec :family "Fantasque Sans Mono" :size new-font-size))
      (set-frame-font ++font t (++get-frame-list)))
    (setq ++font-size new-font-size)))

(defun ++setup-font ()
  (++configure-font-size)
  (when (display-graphic-p)
    (setq ++adjust-font-timer (run-with-idle-timer 1 1 #'++configure-font-size))))

(tool-bar-mode -1)

(provide '+appearance)
