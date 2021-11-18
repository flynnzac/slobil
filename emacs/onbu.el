;;; onbu.el --- ONBU Emacs Lisp Mode               -*- lexical-binding: t; -*-


;; ONBU is a REGISTER BASED ENVIRONMENT AND LANGUAGE
;; Copyright 2019 Zach Flynn <zlflynn@gmail.com>

;; This file is part of ONBU.

;; ONBU is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ONBU is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ONBU (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (C) 2019  

;; Author:  Zach Flynn <zlflynn@gmail.com>
;; Keywords: languages

(require 'smie)
(require 'isend-mode)
(defvar onbu-mode-syntax-table nil "Syntax table for `onbu-mode'.")

(defvar onbu-indent 2)

(defvar onbu-mode-map nil "Keymap for `onbu-mode'")

(progn
  (setq onbu-mode-map (make-sparse-keymap))
  (define-key onbu-mode-map (kbd "C-c C-s") 'onbu-start)
  (define-key onbu-mode-map (kbd "C-c C-a") 'onbu-associate)
  (define-key onbu-mode-map (kbd "C-c C-r") 'isend-send)
  (define-key onbu-mode-map (kbd "C-c C-b") 'onbu-send-buffer))

(defvar onbu-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("(" insts ")")
            ("[" insts "]")
            ("{" insts "}")
            (exp))
      (insts (insts "." insts) (inst)))
    '((assoc ".")))))

(setq onbu-mode-syntax-table
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

(setq onbu-font-lock-keywords
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

(define-derived-mode onbu-mode prog-mode "onbu"
  "Major mode for editing code in the ONBU language"
  (setq-local font-lock-defaults '((onbu-font-lock-keywords)))
  (set-syntax-table onbu-mode-syntax-table)
  (smie-setup onbu-grammar #'ignore)
  (setq-local smie-indent-basic onbu-indent)
  (setq-local comment-start "' ")
  (use-local-map onbu-mode-map)
  )

(defvar onbu-path "/usr/local/bin/onbu")

(defun onbu-start (b)
  "Starts an onbu process in a certain buffer."
  (interactive "sBuffer (default: onbu): ")
  (if (string= b "") (setq b "onbu"))
  (let ((text-buffer (current-buffer))
        (starred-name (concat "*" b "*")))
    (ansi-term onbu-path b)
    (with-current-buffer text-buffer
      (isend-associate starred-name))))

(defun onbu-associate (b)
  "Associates an onbu code buffer with a certain onbu process buffer."
  (interactive "bBuffer: ")
  (let ((text-buffer (current-buffer)))
    (isend-associate b)))

(defun onbu-send-buffer ()
  "Sends whole buffer to current process associated with the buffer."
  (interactive)
  (mark-whole-buffer)
  (isend-send))


(provide 'onbu)



