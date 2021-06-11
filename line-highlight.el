;;; https://stackoverflow.com/questions/14454219/how-to-highlight-a-particular-line-in-emacs

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun remove-all-highlight ()
  (interactive)
  (remove-overlays (point-min) (point-max)))

(defun toggle-highlight-line (ln color)
  (interactive)
  (goto-line ln)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
        (overlay-put overlay-highlight 'face `(:background ,color))
        (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
