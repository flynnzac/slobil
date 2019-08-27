;;; arbel.el --- ARBEL Emacs Lisp Mode               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  Zach Flynn <zlflynn@gmail.com>
;; Keywords: languages


(defvar arbel-mode-syntax-table nil "Syntax table for `arbel-mode'.")

(require 'smie)
(defvar arbel-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("(" insts ")")
            ("[" insts "]")
            ("{" insts "}")
            (exp))
      (insts (insts "." insts) (inst))
      '((assoc "."))))))

(setq arbel-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?\( "()" st)
        (modify-syntax-entry ?\) ")(" st)
        (modify-syntax-entry ?[ "(]" st)
                             (modify-syntax-entry ?] ")[" st)
        (modify-syntax-entry ?\{ "(}" st)
        (modify-syntax-entry ?\} "){" st)
        (modify-syntax-entry ?- "w" st)
        (modify-syntax-entry ?# "w" st)
        (modify-syntax-entry ?$ "_" st)
        (modify-syntax-entry ?\\ "_" st)
        (modify-syntax-entry ?. "." st)
        (modify-syntax-entry ?! "<" st)
        (modify-syntax-entry ?\n "> " st)
        st))

(setq arbel-font-lock-keywords
      (let* (
	           (x-functions
              '("set"
			          "add"
			          "multiply"
			          "subtract"
                "divide"
                "if"
                "compute"
                "reg"
                "get"
                "mov"
                "del"
                "exit"
                "answer"
                "sit"
                "exist"
                "gt"
                "lt"
                "eq"
                "lteq"
                "gteq"
                "print"
                "character"
                "count-characters"
                "concat"
                "source"
                "do-to-all"
                "next"
                "last"
                "in"
                "while"
                "list"
                "to-register"
                "collapse"
                "join"
                "string-eq"
                "string-gt"
                "string-lt"
                "exist-in"
                "reg-eq"
                "reg-lt"
                "reg-gt"
                "go-in"
                "go-out"
                "save"
                "load"
                "to-string"
                "to-number"
                "ref"
                "output-code"
                "clear-code"
                "error"
                "is-integer"
                "is-decimal"
                "is-string"
                "is-register"
                "is-registry"
                "is-instruction"
                "is-file"
                "is-nothing"
                "open-text-file"
                "read"
                "close"
                "and"
                "or"
                "not"
                "read-line"
                "write"
                "input"
                "shell"
                "link"

			          ))
	           (x-functions-regexp (regexp-opt x-functions 'words))
             (register-regexp "\\(\$[^\s]*\\)\s*")
             (reference-regexp "\\(\\\\[^\s]*\\)\s*")
             (comment-regexp "^[\s]*rem.*?$"))
	      `(
	        (,x-functions-regexp . font-lock-builtin-face)
	        (,register-regexp . (1 font-lock-function-name-face))
          (,reference-regexp . (1 font-lock-constant-face))
          (,comment-regexp . font-lock-comment-face)
	        )))

(define-derived-mode arbel-mode prog-mode "arbel mode"
  "Major mode for editing code in the ARBEL language"
  (setq-local font-lock-defaults '((arbel-font-lock-keywords)))
  (set-syntax-table arbel-mode-syntax-table)
  (smie-setup arbel-grammar #'ignore)
  (setq-local smie-indent-basic 2)
  (setq-local comment-start "!")
  )

(provide 'arbel)



