;;; tsg-debugger.el --- TSG Debugger -*- coding: utf-8; lexical-binding: t; -*-

(require 'rx)
(load-file "line-highlight.el")
(load-file "tsg-mode.el")

(defun get-or-gen-buffer (x) (or (get-buffer x) (generate-new-buffer x)))
(defvar *prog-buf* (get-or-gen-buffer "*PROGRAM*") "buffer with program")
;; manual update of the buffer with program
;; (setq *prog-buf* (get-or-gen-buffer "*PROGRAM*"))

(defvar *ghci-buf* (get-buffer "*shell*<1>") "buffer with GHCI")
;; manual update of the buffer with GHCI
;; (setq *ghci-buf* (get-buffer "*shell*<1>"))

(defun tsg/cur-theme ()
  (condition-case nil
      *my-current-theme*
    (error 'leuven)))

(defun tsg/col (x)
  " colors: https://flaviocopes.com/rgb-color-codes/ "
  (let* ((all-colors
          (list
           'leuven      '(:line "#7FFFD4" :break "#F08080" :line+break "#DDA0DD")
                          ;;     AQUA_MARINE      LIGHT_CORAL           PLUM
           'tron-legacy '(:line "#00008B" :break "#800000" :line+break "#9932CC")))
                          ;;     DARK_BLUE        MAROON                DARK_ORCHID
         (cur-theme (tsg/cur-theme))
         (colors (plist-get all-colors cur-theme)))
    (plist-get colors x)))

(defun tsg/fix-program (program)
  (let* ((lines (s-split "\n" program))
         (plines (--map (cadr (s-match (rx ")\t" (group (1+ any))) it)) lines)))
    (s-join "\n" plines)))

(defun tsg/eof () (goto-char (point-max)))
(defun tsg/send-command (cmd)
  (tsg/eof)
  (insert cmd)
  (comint-send-input))

(defun tsg/load-program ()
  (interactive)
  (switch-to-buffer *ghci-buf*)
  (tsg/send-command "p")
  (let* ((_ (search-backward "0)PROGRAM"))
         (_ (forward-line))
         (sa (point))
         (_ (search-forward "====="))
         (_ (beginning-of-line))
         (sb (point))
         ;; (_ (message "sa: %d, sb: %d" sa sb))
         (program (buffer-substring-no-properties sa sb))
         (fprogram (tsg/fix-program program))
         (_ (tsg/eof)))
    (with-current-buffer *prog-buf*
      (read-only-mode -1)
      (erase-buffer)
      (insert fprogram)
      (read-only-mode 1))))


(defun tsg/parse-info (info)
  (let* ((line-raw (cadr (s-match (rx "current line: " (group (1+ (any "0-9")))) info)))
         (line (cl-parse-integer line-raw))
         (bp-raw (cadr (s-match (rx "breakpoints2: " (group (1+ any))) info)))
         (bp (append (json-parse-string bp-raw) ()))
         (env-idx (s-index-of "env:" info))
         (env (substring info env-idx (- (length info) 8)))
         (_ (message "#tsg/parse-info, env is: %s" env))
         (raw (substring info 0 (- (length info) 7)))
         )
    `(("cur-line" . ,line) ("break-lines" . ,bp) ("env" . ,env) ("raw" . ,raw))))

(defun tsg/load-info ()
  (interactive)
  (with-current-buffer *ghci-buf*
    (let* ((_ (search-backward "current line"))
           (_ (message "point! %d" (point)))
           (_ (beginning-of-line))
           (sa (point))
           (_ (tsg/eof))
           (_ (beginning-of-line))
           (sb (point))
           (info-raw (buffer-substring-no-properties sa sb))
           ;; (_ (message "info-raw: %s" info-raw))
           (info (tsg/parse-info info-raw))
           (_ (tsg/eof)))
      info)))

(defun tsg/colorize-program (info)
  (interactive)
  (with-current-buffer *prog-buf*
    (remove-all-highlight)
    (let* ((line (get-by-key "cur-line" info))
           (breaks (cdr (assoc "break-lines" info)))
           (line-col (tsg/col (if (member line breaks) :line+break :line)))
           (_ (toggle-highlight-line line line-col))
           (breaks2 (--filter (not (eq it line)) breaks))
           (break-col (tsg/col :break))
           (_ (--map (toggle-highlight-line it break-col) breaks2))
           (_ (goto-line line)))
      "done")))

(defun tsg/dispatch (cmd0 &optional arg)
  (let* ((cur-buf (current-buffer))
         (cmd (if (null arg) cmd0 (concat cmd0 " " (int-to-string arg))))
         (_ (message "#dispatch-command, cmd: %s" cmd))
         (_ (switch-to-buffer *ghci-buf*))
         (_ (tsg/send-command cmd))
         (_ (sleep-for 0 50))
         (_ (switch-to-buffer cur-buf)))
    (tsg/repl t)))

(defun tsg/repl (&optional tailp)
  (interactive)
  (if (not tailp) (tsg/load-program))
  (let* ((i (tsg/load-info))
         (hydra-info (concat (get-by-key "raw" i) "\n"))
         (_ (tsg/colorize-program i))
         (_ (switch-to-buffer *prog-buf*))
	       (_ (tsg-mode)))
    (call-interactively
     (eval
      `(defhydra my-tsg-hydra (:exit t)
         ,hydra-info
         ("b" (tsg/dispatch "b" (read-number "enter line: ")) "toggle breakpoint")
         ("r" (tsg/dispatch "r") "run")
         ("n" (tsg/dispatch "n") "do 1 step")
         ("s" (tsg/dispatch "s" (read-number "enter # steps: ")) "do N steps")
         ("j" (tsg/dispatch "j" (read-number "enter line: ")) "run untill line")
         ("t" (tsg/dispatch "t") "toggle vars view")
         ("q" nil "exit")
         ("<ESC>" nil "exit")
         )))))

(global-set-key (kbd "s-l") 'tsg/repl)
