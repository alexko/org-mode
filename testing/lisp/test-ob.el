;;; test-ob.el --- tests for ob.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte, Martyn Jago

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))
  (require 'org-test)

(ert-deftest test-org-babel/src-name-regexp ()
  (should(equal "^[ \t]*#\\+\\(srcname\\|source\\|function\\):[ \t]*"
		org-babel-src-name-regexp))
  (mapcar (lambda (name) 
	    (should (org-test-string-exact-match
		     org-babel-src-name-regexp
		     (concat
		      "   \t #+"
		      name
		      ":    \t src-name \t blah blah blah ")))
	    (should (string-match
		     org-babel-src-name-regexp
		     (concat 
		      "#+" (upcase name)
		      ": src-name")))
	    ;;TODO This should fail no?
	    (should (org-test-string-exact-match
		     org-babel-src-name-regexp
		     (concat
		      "#+" name ":")))
	    ;;TODO Check - should this pass?
	    (should (not (org-test-string-exact-match
			  org-babel-src-name-regexp
			  (concat
			   "#+" name " : src-name")))))
	  '("srcname" "source" "function"))
  (should (not  (org-test-string-exact-match
		 org-babel-src-name-regexp
		 "#+invalid-name: src-name"))))

(ert-deftest test-org-babel/multi-line-header-regexp ()
  (should(equal "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
		org-babel-multi-line-header-regexp))
  ;;TODO can be optimised - and what about blah4 blah5 blah6?
  (should (string-match
	   org-babel-multi-line-header-regexp
	   "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))
  (should
   (equal
    "blah1 blah2 blah3 \t"
    (match-string
     1
     "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n")))
  
  ;;TODO Check - should this fail?
  (should (not (org-test-string-exact-match
	   org-babel-multi-line-header-regexp
	   "   \t #+headers : blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))))

(ert-deftest test-org-babel/src-name-w-name-regexp ()
  (should(equal
	  (concat org-babel-src-name-regexp "\\("
		  org-babel-multi-line-header-regexp "\\)*"
		  "\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)")
	  org-babel-src-name-w-name-regexp))
  (should (org-test-string-exact-match
	   org-babel-src-name-w-name-regexp
	   (concat
	    "#+srcname: src-name "
	    "#+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))))

(ert-deftest test-org-babel/src-block-regexp ()
  (let ((test-block
	 (concat
	  "#+begin_src language -n-r-a-b -c :argument-1 yes :argument-2 no\n"
	  "echo this is a test\n"
	  "echo Currently in ' $PWD\n"
	  "#+end_src"))
	(language "language")
	(flags "-n-r-a-b -c ")
	(arguments ":argument-1 yes :argument-2 no")
	(body "echo this is a test\necho Currently in ' $PWD\n"))
    (should (string-match org-babel-src-block-regexp test-block))
    (should (string-match org-babel-src-block-regexp (upcase test-block)))
    (should (equal language (match-string 2 test-block)))
    ;;TODO Consider refactoring
    (should (equal flags (match-string 3 test-block)))
    (should (equal arguments (match-string 4 test-block)))
    (should (equal body (match-string 5 test-block)))
    ;;no switches
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
     	     (replace-regexp-in-string flags "" test-block)))
    ;;no header arguments
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
	     (replace-regexp-in-string arguments "" test-block)))
    ;; should be valid with no body
    (should (org-test-string-exact-match
	     org-babel-src-block-regexp
	     (replace-regexp-in-string body "" test-block)))))

(ert-deftest test-org-babel/get-header ()
  (should (not (org-babel-get-header
		org-babel-default-header-args :doesnt-exist)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session nil)))
  (should (not (org-babel-get-header
		org-babel-default-header-args :SESSION)))
  (should (equal '((:tangle . "no"))
		 (org-babel-get-header
		  org-babel-default-header-args :tangle)))
  ;; with OTHERS option
  (should (equal org-babel-default-header-args
		 (org-babel-get-header
		  org-babel-default-header-args :doesnt-exist 'others)))
  (should (equal org-babel-default-header-args
		 (org-babel-get-header
		  org-babel-default-header-args nil 'others)))
  (should (null
	   (assoc :noweb
		  (org-babel-get-header
		   org-babel-default-header-args :noweb 'others)))))

(ert-deftest test-org-babel/default-inline-header-args ()
  (should(equal
	  '((:session . "none") (:results . "replace") (:exports . "results"))
	  org-babel-default-inline-header-args)))

;;; ob-get-src-block-info
(ert-deftest test-org-babel/get-src-block-info-language ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel/get-src-block-info-body ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel/get-src-block-info-tangle ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

(ert-deftest test-org-babel/elisp-in-header-arguments ()
  "Test execution of elisp forms in header arguments."
  ;; at the babel.org:elisp-forms-in-header-arguments header
  (org-test-at-id "22d67284-bf14-4cdc-8319-f4bd876829d7"
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (should (= 4 (org-babel-execute-src-block))))))

(ert-deftest test-org-babel/simple-named-code-block ()
  "Test that simple named code blocks can be evaluated."
  (org-test-at-id "0d82b52d-1bb9-4916-816b-2c67c8108dbb"
    (org-babel-next-src-block 1)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/simple-variable-resolution ()
  "Test that simple variable resolution is working."
  (org-test-at-id "f68821bc-7f49-4389-85b5-914791ee3718"
    (org-babel-next-src-block 2)
    (should (= 4 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/multi-line-header-arguments ()
  "Test that multi-line header arguments and can be read."
  (org-test-at-id "b77c8857-6c76-4ea9-8a61-ddc2648d96c4"
    (org-babel-next-src-block)
    (let ((results (org-babel-execute-src-block)))
      (should(equal 'a (cadr (assoc 1 results))))
      (should(equal 'd (cadr (assoc 4 results)))))))

(ert-deftest test-org-babel/parse-header-args ()
  (org-test-at-id "7eb0dc6e-1c53-4275-88b3-b22f3113b9c3"
    (org-babel-next-src-block)
    (let* ((info (org-babel-get-src-block-info))
	   (params (nth 2 info)))
      (message "%S" params)
      (should(equal "example-lang" (nth 0 info)))
      (should(string= "the body" (org-babel-trim (nth 1 info))))
      (should-not (member '(:session\ \ \ \ ) params))
      (should(equal '(:session) (assoc :session params)))
      (should(equal '(:result-type . output) (assoc :result-type params)))
      (should(equal '(num . 9) (cdr (assoc :var params)))))))

(ert-deftest test-org-babel/parse-header-args2 ()
  (org-test-at-id "2409e8ba-7b5f-4678-8888-e48aa02d8cb4"
    (should (string-match (regexp-quote "this is simple")
			  (org-babel-ref-resolve "simple-subtree")))
    (org-babel-next-src-block)
    (should (= 14 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/inline-src-blocks ()
  (org-test-at-id "54cb8dc3-298c-4883-a933-029b3c9d4b18"
    (macrolet ((at-next (&rest body)
		 `(progn
		    (move-end-of-line 1)
		    (re-search-forward org-babel-inline-src-block-regexp nil t)
		    (goto-char (match-beginning 1))
		    (save-match-data ,@body))))
      (at-next (should (equal 1 (org-babel-execute-src-block))))
      (at-next (should (equal 2 (org-babel-execute-src-block))))
      (at-next (should (equal 3 (org-babel-execute-src-block)))))))

(ert-deftest test-org-babel/org-babel-get-inline-src-block-matches ()
  (org-test-at-id "0D0983D4-DE33-400A-8A05-A225A567BC74"
    (let ((test-point (point)))
      (should (fboundp 'org-babel-get-inline-src-block-matches))
      (should (re-search-forward "src_" nil t)) ;; 1
      (should (= (+ test-point 140) (match-end 0)))
      (should (org-babel-get-inline-src-block-matches))
      (should (re-search-forward "}" nil (point-at-bol))) ;; 1
      (should-not (org-babel-get-inline-src-block-matches))
      (should (re-search-forward "in" nil t)) ;; 2
      (should-not (org-babel-get-inline-src-block-matches))
      (should (re-search-forward "echo" nil t)) ;; 2
      (should (org-babel-get-inline-src-block-matches))
      (should (re-search-forward "blocks" nil t)) ;; 3
      (backward-char 8) ;; 3
      (should (org-babel-get-inline-src-block-matches))
      (forward-char 1) ;;3
      (should-not (org-babel-get-inline-src-block-matches))
      (should (re-search-forward ":results" nil t)) ;; 4
      (should (org-babel-get-inline-src-block-matches))
      (end-of-line)
      (should-not (org-babel-get-inline-src-block-matches))
    )))

(ert-deftest test-org-babel/inline-src_blk-default-results-replace-line-1 ()
  (with-temp-buffer

    ;; src_ at bol line 1...
    (let ((test-line "src_sh{echo 1}"))
      (insert test-line)
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string=
       	       (concat test-line " =1=")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char) (org-ctrl-c-ctrl-c)
      (should (string=
       	       (concat test-line " =1= =1=")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "1}")
      (should-error (org-ctrl-c-ctrl-c))
      (backward-char) ;; last char of block body
      (org-ctrl-c-ctrl-c)
      (should (string=
       	       (concat test-line " =1= =1= =1=")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

    ;; src_ follows space line 1...
    (let ((test-line " src_emacs-lisp{ 1 }"))
      (beginning-of-line)
      (insert (concat test-line "\n"))
      (goto-char (point-min))
      (should-error (org-ctrl-c-ctrl-c))
      (forward-char) (org-ctrl-c-ctrl-c) 
      (should (string=
	       (concat test-line " =1=")
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "{ 1 ") (org-ctrl-c-ctrl-c)
      (should (string=
	       (concat test-line " =1= =1=")
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char)
      (should-error (org-ctrl-c-ctrl-c))
      )))

(ert-deftest test-org-babel/inline-src_blk-default-results-replace-line-2 ()
  (with-temp-buffer

    ;; src_ at bol line 2...
    (let ((test-line " src_emacs-lisp{ \"x\" }"))
      (insert (concat "\n" test-line))
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-min))
      (should-error (org-ctrl-c-ctrl-c))
      (forward-line)
      (should-error (org-ctrl-c-ctrl-c))
      (forward-char) (org-ctrl-c-ctrl-c)
      (should (string=
	       (concat test-line " =x=")
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

    (let ((test-line "Some text prior to block src_emacs-lisp{ \"y\" }"))
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src") (org-ctrl-c-ctrl-c)
      (should (string=
       	       (concat test-line " =y= end")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-ctrl-c-ctrl-c)
      (should (string=
	       (concat test-line " =y= =y= end")
       	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char)
      (should-error (org-ctrl-c-ctrl-c))
      )))

(ert-deftest test-org-babel/inline-src_blk-manual-results-replace ()
  (with-temp-buffer

    (let ((test-line " src_emacs-lisp[:results replace]{ \"x\" }"))
      (insert (concat "\n" test-line))
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-min))
      (should-error (org-ctrl-c-ctrl-c))
      (forward-line)
      (should-error (org-ctrl-c-ctrl-c))
      (forward-char) (org-ctrl-c-ctrl-c)
      (should (string=
      	       (concat test-line " =x=")
      	       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    
    (let ((test-line " Some text prior to block src_emacs-lisp[:results replace]{ \"y\" }"))
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src") (org-ctrl-c-ctrl-c)
      (should (string=
    	       (concat test-line " =y= end")
    	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-ctrl-c-ctrl-c)
      (should (string=
    	       (concat test-line " =y= =y= end")
    	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char)
      (should-error (org-ctrl-c-ctrl-c)))
    ))

(ert-deftest test-org-babel/inline-src_blk-results-silent ()
  (with-temp-buffer

    (let ((test-line "src_emacs-lisp[ :results silent ]{ \"x\" }"))
      (insert test-line)
      (should-error (org-ctrl-c-ctrl-c))
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string= test-line
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (let ((test-line " Some text prior to block src_emacs-lisp[ :results silent ]{ \"y\" }"))
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src_") (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " end")
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " end")
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char)
      (should-error (org-ctrl-c-ctrl-c)))
      ))

(ert-deftest test-org-babel/inline-src_blk-results-raw ()
  (with-temp-buffer

    (let ((test-line "src_emacs-lisp[ :results raw ]{ \"x\" }"))
      (insert test-line)
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " x")
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (let ((test-line " Some text prior to block src_emacs-lisp[ :results raw ]{ \"the\" }"))
      (goto-char (point-max))
      (insert (concat "\n" test-line " end"))
      (re-search-backward "src_") (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " the end")
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (re-search-forward "\" ") (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " the the end")
		       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (forward-char)
      (should-error (org-ctrl-c-ctrl-c)))
      ))

(ert-deftest test-org-babel/inline-src_blk-results-file ()
  (with-temp-buffer

    (let ((test-line "src_emacs-lisp[ :results file ]{ \"~/test-file\"  }"))
      (insert test-line)
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " [[file:~/test-file]]")
		       (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-org-babel/inline-src_blk-results-scalar ()
  (with-temp-buffer

    (let ((test-line "src_emacs-lisp[ :results scalar ]{ \"x\"  }"))
      (insert test-line)
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line  " =\"x\"=")
		       (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-org-babel/inline-src_blk-results-verbatim ()
  (with-temp-buffer

    (let ((test-line "src_emacs-lisp[ :results verbatim ]{ \"x\"  }"))
      (insert test-line)
      (goto-char (point-min)) (org-ctrl-c-ctrl-c)
      (should (string= (concat test-line " =\"x\"=")
		       (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'test-ob)

;;; test-ob ends here
