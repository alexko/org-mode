;;; ob-lua.el --- org-babel functions for lua evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation

;; Author: Eric Schulte, Dan Davison, Alex Kosorukoff
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.4

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating lua source code.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function org-remove-indentation "org" )
(declare-function lua-shell "ext:lua-mode" (&optional argprompt))
(declare-function run-lua "ext:lua" (&optional cmd noshow new))

(add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua"))

(defvar org-babel-default-header-args:lua '())

(defvar org-babel-lua-command "lua"
  "Name of command for executing lua code.")

(defvar org-babel-lua-mode (if (featurep 'xemacs) 'lua-mode 'lua)
  "Preferred lua mode for use in running lua interactively.")

(defvar org-src-preserve-indentation)

(defun org-babel-execute:lua (body params)
  "Execute a block of Lua code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-lua-initiate-session
		   (cdr (assoc :session params))))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
	 (return-val (when (and (eq result-type 'value) (not session))
		       (cdr (assoc :return params))))
	 (preamble (cdr (assoc :preamble params)))
         (full-body
	  (org-babel-expand-body:generic
	   (concat body (if return-val (format "return %s" return-val) ""))
	   params (org-babel-variable-assignments:lua params)))
         (result (org-babel-lua-evaluate
		  session full-body result-type result-params preamble)))
    (or (cdr (assoc :file params))
        (org-babel-reassemble-table
         result
         (org-babel-pick-name (cdr (assoc :colname-names params))
			      (cdr (assoc :colnames params)))
         (org-babel-pick-name (cdr (assoc :rowname-names params))
			      (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:lua (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  (let* ((session (org-babel-lua-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:lua params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:lua (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:lua session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:lua (params)
  "Return list of lua statements assigning the block's variables"
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-lua-var-to-lua (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-lua-var-to-lua (var)
  "Convert an elisp value to a lua variable.
Convert an elisp value, VAR, into a string of lua source code
specifying a variable of the same value."
  (if (listp var)
      (concat "{" (mapconcat #'org-babel-lua-var-to-lua var ", ") "}")
    (if (equal var 'hline)
	"None"
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       var))))

(defun org-babel-lua-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))

(defvar org-babel-lua-buffers '((:default . nil)))

(defun org-babel-lua-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-lua-buffers)))

(defun org-babel-lua-initiate-session-by-key (&optional session)
  "Initiate a lua session.
If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require org-babel-lua-mode)
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (lua-buffer (org-babel-lua-session-buffer session)))
      (cond
       ((and (eq 'lua org-babel-lua-mode)
	     (fboundp 'run-lua)) ; lua.el
	(run-lua))
       ((and (eq 'lua-mode org-babel-lua-mode)
	     (fboundp 'lua-shell)) ; lua-mode.el
	;; `lua-shell' creates a buffer whose name is the value of
	;; `lua-which-bufname' with '*'s at the beginning and end
	(let* ((bufname (if lua-buffer
			    (replace-regexp-in-string ;; zap surrounding *
			     "^\\*\\([^*]+\\)\\*$" "\\1" lua-buffer)
			  (concat "Lua-" (symbol-name session))))
	       (lua-which-bufname bufname))
	  (lua-shell)
	  (setq lua-buffer (concat "*" bufname "*"))))
       (t
	(error "No function available for running an inferior lua.")))
      (setq org-babel-lua-buffers
	    (cons (cons session lua-buffer)
		  (assq-delete-all session org-babel-lua-buffers)))
      session)))

(defun org-babel-lua-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-lua-session-buffer
     (org-babel-lua-initiate-session-by-key session))))

(defvar org-babel-lua-eoe-indicator "'org_babel_lua_eoe'"
  "A string to indicate that evaluation has completed.")
(defvar org-babel-lua-wrapper-method
  "
function main()
%s
end

f = io.open('%s', 'w')
f:write( tostring(main()) )
f:close()")

(defvar org-babel-lua-pp-wrapper-method
  "
require(\"json\")
function main()
%s
end
f=io.open('%s', 'w')
f:write( json.encode(main()) )
f:close()")

(defun org-babel-lua-evaluate
  (session body &optional result-type result-params preamble)
  "Evaluate BODY as lua code."
  (if session
      (org-babel-lua-evaluate-session
       session body result-type result-params)
    (org-babel-lua-evaluate-external-process
     body result-type result-params preamble)))

(defun org-babel-lua-evaluate-external-process
  (body &optional result-type result-params preamble)
  "Evaluate BODY in external lua process.
If RESULT-TYPE equals 'output then return standard output as a
string. If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (output (org-babel-eval org-babel-lua-command
			    (concat (if preamble (concat preamble "\n") "") body)))
    (value (let ((tmp-file (org-babel-temp-file "lua-")))
	     (org-babel-eval org-babel-lua-command
			     (concat
			      (if preamble (concat preamble "\n") "")
			      (format
			       (if (member "pp" result-params)
				   org-babel-lua-pp-wrapper-method
				 org-babel-lua-wrapper-method)
			       (mapconcat
				(lambda (line) (format "\t%s" line))
				(split-string
				 (org-remove-indentation
				  (org-babel-trim body))
				 "[\r\n]") "\n")
			       (org-babel-process-file-name tmp-file 'noquote))))
	     ((lambda (raw)
		(if (or (member "code" result-params)
			(member "pp" result-params))
		    raw
		  (org-babel-lua-table-or-string raw)))
	      (org-babel-eval-read-file tmp-file))))))

(defun org-babel-lua-evaluate-session
  (session body &optional result-type result-params)
  "Pass BODY to the Lua process in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string. If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (flet ((dump-last-value
	  (tmp-file pp)
	  (mapc
	   (lambda (statement) (insert statement) (comint-send-input))
	   (if pp
	       (list
		"require 'json'"
		(format "io:open('%s', 'w').write(json.encode(_))"
			(org-babel-process-file-name tmp-file 'noquote)))
	     (list (format "io:open('%s', 'w').write(str(_))"
			   (org-babel-process-file-name tmp-file 'noquote))))))
	 (input-body (body)
		     (mapc (lambda (statement) (insert statement) (comint-send-input))
			   (split-string (org-babel-trim body) "[\r\n]+"))
		     (comint-send-input) (comint-send-input)))
    (case result-type
      (output
       (mapconcat
	#'org-babel-trim
	(butlast
	 (org-babel-comint-with-output
	     (session org-babel-lua-eoe-indicator t body)
	   (let ((comint-process-echoes nil))
	     (input-body body)
	     (insert org-babel-lua-eoe-indicator)
	     (comint-send-input))) 2) "\n"))
      (value
       ((lambda (results)
	  (if (or (member "code" result-params) (member "pp" result-params))
	      results
	    (org-babel-lua-table-or-string results)))
	(let ((tmp-file (org-babel-temp-file "lua-")))
	  (org-babel-comint-with-output
	      (session org-babel-lua-eoe-indicator t body)
	    (let ((comint-process-echoes nil))
	      (input-body body)
	      (dump-last-value tmp-file (member "pp" result-params))
	      (comint-send-input) (comint-send-input)
	      (insert org-babel-lua-eoe-indicator)
	      (comint-send-input)))
	  (org-babel-eval-read-file tmp-file)))))))

(defun org-babel-lua-read-string (string)
  "Strip 's from around lua string"
  (if (string-match "^'\\([^\000]+\\)'$" string)
      (match-string 1 string)
    string))

(provide 'ob-lua)

;; arch-tag: f19b6c3d-dfcb-4a1a-9ce0-45ade1ebc212

;;; ob-lua.el ends here
