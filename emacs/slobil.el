;;; slobil.el --- SLOBIL Emacs Lisp Mode               -*- lexical-binding: t; -*-

;; SLOBIL
;; Copyright 2023 Zach Flynn

;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author:  Zach Flynn <zlflynn@gmail.com>
;; Keywords: languages

(require 'smie)
(require 'isend-mode)
(defvar slobil-mode-syntax-table nil "Syntax table for `slobil-mode'.")

(defvar slobil-indent 2)

(defvar slobil-mode-map nil "Keymap for `slobil-mode'")

(progn
  (setq slobil-mode-map (make-sparse-keymap))
  (define-key slobil-mode-map (kbd "C-c C-s") 'slobil-start)
  (define-key slobil-mode-map (kbd "C-c C-a") 'slobil-associate)
  (define-key slobil-mode-map (kbd "C-c C-r") 'isend-send)
  (define-key slobil-mode-map (kbd "C-c C-b") 'slobil-send-buffer))

(defvar slobil-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("(" insts ")")
            ("[" insts "]")
            ("{" insts "}")
            (exp))
      (insts (insts "." insts) (inst)))
    '((assoc ".")))))

(setq slobil-mode-syntax-table
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

(setq slobil-font-lock-keywords
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

(define-derived-mode slobil-mode prog-mode "slobil"
  "Major mode for editing code in the SLOBIL language"
  (setq-local font-lock-defaults '((slobil-font-lock-keywords)))
  (set-syntax-table slobil-mode-syntax-table)
  (smie-setup slobil-grammar #'ignore)
  (setq-local smie-indent-basic slobil-indent)
  (setq-local comment-start "' ")
  (use-local-map slobil-mode-map)
  )

(defvar slobil-path "/usr/local/bin/slobil")

(defun slobil-start (b)
  "Starts an slobil process in a certain buffer."
  (interactive "sBuffer (default: slobil): ")
  (if (string= b "") (setq b "slobil"))
  (let ((text-buffer (current-buffer))
        (starred-name (concat "*" b "*")))
    (ansi-term slobil-path b)
    (with-current-buffer text-buffer
      (isend-associate starred-name))))

(defun slobil-associate (b)
  "Associates an slobil code buffer with a certain slobil process buffer."
  (interactive "bBuffer: ")
  (let ((text-buffer (current-buffer)))
    (isend-associate b)))

(defun slobil-send-buffer ()
  "Sends whole buffer to current process associated with the buffer."
  (interactive)
  (mark-whole-buffer)
  (isend-send))


(provide 'slobil)



