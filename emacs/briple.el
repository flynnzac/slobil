;;; briple.el --- BRIPLE Emacs Lisp Mode               -*- lexical-binding: t; -*-


;; BRIPLE is a REGISTER BASED ENVIRONMENT AND LANGUAGE
;; Copyright 2019 Zach Flynn <zlflynn@gmail.com>

;; This file is part of BRIPLE.

;; BRIPLE is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BRIPLE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BRIPLE (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (C) 2019  

;; Author:  Zach Flynn <zlflynn@gmail.com>
;; Keywords: languages

(require 'smie)
(require 'isend-mode)
(defvar briple-mode-syntax-table nil "Syntax table for `briple-mode'.")

(defvar briple-indent 2)

(defvar briple-mode-map nil "Keymap for `briple-mode'")

(progn
  (setq briple-mode-map (make-sparse-keymap))
  (define-key briple-mode-map (kbd "C-c C-s") 'briple-start)
  (define-key briple-mode-map (kbd "C-c C-a") 'briple-associate)
  (define-key briple-mode-map (kbd "C-c C-r") 'isend-send)
  (define-key briple-mode-map (kbd "C-c C-b") 'briple-send-buffer))

(defvar briple-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("(" insts ")")
            ("[" insts "]")
            ("{" insts "}")
            (exp))
      (insts (insts "." insts) (inst)))
    '((assoc ".")))))

(setq briple-mode-syntax-table
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

(setq briple-font-lock-keywords
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

(define-derived-mode briple-mode prog-mode "briple"
  "Major mode for editing code in the BRIPLE language"
  (setq-local font-lock-defaults '((briple-font-lock-keywords)))
  (set-syntax-table briple-mode-syntax-table)
  (smie-setup briple-grammar #'ignore)
  (setq-local smie-indent-basic briple-indent)
  (setq-local comment-start "' ")
  (use-local-map briple-mode-map)
  )

(defvar briple-path "/usr/local/bin/briple")

(defun briple-start (b)
  "Starts an briple process in a certain buffer."
  (interactive "sBuffer (default: briple): ")
  (if (string= b "") (setq b "briple"))
  (let ((text-buffer (current-buffer))
        (starred-name (concat "*" b "*")))
    (ansi-term briple-path b)
    (with-current-buffer text-buffer
      (isend-associate starred-name))))

(defun briple-associate (b)
  "Associates an briple code buffer with a certain briple process buffer."
  (interactive "bBuffer: ")
  (let ((text-buffer (current-buffer)))
    (isend-associate b)))

(defun briple-send-buffer ()
  "Sends whole buffer to current process associated with the buffer."
  (interactive)
  (mark-whole-buffer)
  (isend-send))


(provide 'briple)



