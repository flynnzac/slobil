;;; wob.el --- WOB Emacs Lisp Mode               -*- lexical-binding: t; -*-


;; WOB is a REGISTER BASED ENVIRONMENT AND LANGUAGE
;; Copyright 2019 Zach Flynn <zlflynn@gmail.com>

;; This file is part of WOB.

;; WOB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; WOB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with WOB (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (C) 2019  

;; Author:  Zach Flynn <zlflynn@gmail.com>
;; Keywords: languages

(require 'smie)
(require 'isend-mode)
(defvar wob-mode-syntax-table nil "Syntax table for `wob-mode'.")

(defvar wob-indent 2)

(defvar wob-mode-map nil "Keymap for `wob-mode'")

(progn
  (setq wob-mode-map (make-sparse-keymap))
  (define-key wob-mode-map (kbd "C-c C-s") 'wob-start)
  (define-key wob-mode-map (kbd "C-c C-a") 'wob-associate)
  (define-key wob-mode-map (kbd "C-c C-r") 'isend-send)
  (define-key wob-mode-map (kbd "C-c C-b") 'wob-send-buffer))

(defvar wob-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("(" insts ")")
            ("[" insts "]")
            ("{" insts "}")
            (exp))
      (insts (insts "." insts) (inst)))
    '((assoc ".")))))

(setq wob-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?\( "()" st)
        (modify-syntax-entry ?\) ")(" st)
        (modify-syntax-entry ?[ "(]" st)
                             (modify-syntax-entry ?] ")[" st)
        (modify-syntax-entry ?\{ "(}" st)
        (modify-syntax-entry ?\} "){" st)
        (modify-syntax-entry ?\{ "(>" st)
        (modify-syntax-entry ?\} ")<" st)

        (modify-syntax-entry ?- "w" st)
        (modify-syntax-entry ?# "w" st)
        (modify-syntax-entry ?/ "_" st)
        (modify-syntax-entry ?\\ "_" st)
        (modify-syntax-entry ?. "." st)
        (modify-syntax-entry ?' "w 1b" st)
        (modify-syntax-entry ?\s "- 2b" st)
        (modify-syntax-entry ?\n "> b" st)
        st))

(setq wob-font-lock-keywords
      (let* (
	           (register-regexp "\\(\/[^ \t\r\n\v\f]*\\)[ \t\r\n\v\f]*")
	           (boolean-regexp (regexp-opt '("True" "False") 'words))
             (op-def-regexp (regexp-opt '("<" ">") 'symbols))
             )
	      `(
	        (,register-regexp . (1 font-lock-variable-name-face))
	        (,boolean-regexp . (1 font-lock-constant-face))
          (,op-def-regexp . font-lock-function-name-face)
	        )))

(define-derived-mode wob-mode prog-mode "wob"
  "Major mode for editing code in the WOB language"
  (setq-local font-lock-defaults '((wob-font-lock-keywords)))
  (set-syntax-table wob-mode-syntax-table)
  (smie-setup wob-grammar #'ignore)
  (setq-local smie-indent-basic wob-indent)
  (setq-local comment-start "' ")
  (use-local-map wob-mode-map)
  )

(defvar wob-path "/usr/local/bin/wob")

(defun wob-start (b)
  "Starts an wob process in a certain buffer."
  (interactive "sBuffer (default: wob): ")
  (if (string= b "") (setq b "wob"))
  (let ((text-buffer (current-buffer))
        (starred-name (concat "*" b "*")))
    (ansi-term wob-path b)
    (with-current-buffer text-buffer
      (isend-associate starred-name))))

(defun wob-associate (b)
  "Associates an wob code buffer with a certain wob process buffer."
  (interactive "bBuffer: ")
  (let ((text-buffer (current-buffer)))
    (isend-associate b)))

(defun wob-send-buffer ()
  "Sends whole buffer to current process associated with the buffer."
  (interactive)
  (mark-whole-buffer)
  (isend-send))


(provide 'wob)



