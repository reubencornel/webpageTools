;; Load
(load "src/pagegen.lisp")

(defparameter *commands* '("NO-NAVALPHA" "NO-NAVBETA"))

(defun read-file(file)
  (assert (probe-file file))
  (let ((file-string ""))
    (with-open-file (in file)
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line) file-string)
	(setf file-string (concatenate 'string file-string
				       (coerce '(#\newline) 'string)
				       line))))))

(defun group-by(predicate list)
  (assert (and (functionp predicate)
	       (consp list)))
  (labels ((iterate (predicate list &optional (acc nil) (last-val nil))
	     (assert (functionp predicate))
	     (assert (or (consp list)
			 (null list)))
	     (cond ((null list) acc)
		   ((funcall predicate (car list))
		    (if (null last-val)
			(iterate predicate
				 (cdr list)
				 (append acc (list (list (car list))))
				 t)
			(iterate predicate
				 (cdr list)
				 (let ((last-group (first (last acc)))
				       (rest (butlast acc)))
				   (assert (or (consp last-group)
					       (null last-group)))
				   (assert (or (consp rest)
					       (null rest)))
				   (append rest
					   (list (append last-group
						   (list (car list))))))
				 t)))
		   (t (if last-val
			  (iterate predicate
				   (cdr list)
				   (append acc (list (list (car list))))
				   (funcall predicate (car list)))
			  (iterate predicate
				   (cdr list)
				   (let ((last-group (first (last acc)))
					 (rest (butlast acc)))
				     (assert (or (consp last-group)
						 (null last-group)))
				     (Assert  (or (consp rest)
						  (null rest)))
				     (append rest
					     (list (append last-group
						     (list (car list))))))
				   (funcall predicate (car list))))))))
    (let ((result (iterate predicate list '())))
      (assert (every #'(lambda(x)
			 x)
		     (mapcar #'(lambda(x)
				 (or (every #'(lambda(y)
						(funcall predicate y))
					    x)
				     (every #'(lambda(y)
						(not (funcall predicate y)))
					    x)))
			     result)))
      result)))

(defvar hr (htmlmacro hr t))

(defun create-TOC(list)
  (assert (consp list))
  (let ((title-counter 0)
	(subtitle-counter 0))
    (div '((id "TOCWrapper"))
	 (div '((id "tableofcontents"))
	      (div '((class "TOCHeader"))
		   "Table Of Contents"
		   (hr))
	      (div '((class "TOCContents"))
		   (mapcar #'(lambda(y)
			       (cond ((equal (car y)
					     'title)
				      (incf title-counter)
				      (setf subtitle-counter 0)
				      (funcall #'div
					       '((class "TOCTitle"))
					       (a (list (list 'href
							      (format nil "#title~a" title-counter)))

						  (format nil "~a. "
							  title-counter)
						  (cdr y))))

				     ((equal (car y)
					     'subtitle)
				      (incf subtitle-counter)
				      (funcall #'div
					       '((class "TOCSubtitle"))
					       (a (list (list 'href
							      (format nil "#subtitle~a~a" title-counter subtitle-counter)))

					       (format nil "~a.~a " title-counter subtitle-counter)
					       (cdr y))))))
			   list))))))

			   ;; '((title "Introduction")
			   ;;   (title "Computer Arithemetic")
			   ;;   (subtitle "Mathematical formalisms"))))))))


(defun title-p(x)
  "Predicate for identifying a title"
  (not (null (cl-ppcre:scan "^\\*\\* " x))))

(defun subtitle-p(x)
  "Predicate for indentfying a subtitle"
  (not (null (cl-ppcre:scan "^\\*\\*\\* " x))))

(defun page-title-p(x)
  "Predicate for identifying a page title"
  (not (null (cl-ppcre:scan "^\\* " x))))


(defun command-p(x)
  "Predicate to identify if a given line has a command"
  (not (null (cl-ppcre:scan "^\\# " x))))



(defun get-output-file-name(filename)
  (assert (stringp filename))
  (concatenate 'string (subseq filename 0 (- (length filename) 8))
	       ".html"))

(defun code-start-p(x)
  (string= "CODE_START" x))

(defun code-end-p(x)
  (string= "CODE_END" x))


(defun split-string-into-lines(string)
  (cl-ppcre:split (coerce '(#\Newline) 'string) ;; Split the contents of the file on newline
		  string))

(defun group-non-empty-lines(lines)
  (group-by #'(lambda(x) ;; Group lines based on blank lines
		(cl-ppcre:scan "^$" x))
	    lines))

(defun remove-empty-lines(grouped-lines)
  (remove-if #'(lambda(x) ;; Remove empty lines they have served their purpose
		 (and (listp x)
		      (every (lambda(y)
			       (cl-ppcre:scan "^$" y))
			     x)))
	     grouped-lines))

(defun concat-with-space( list )
  (format nil "~{~a~^ ~}" list))

(defun pre-process-input(string)
  (remove-empty-lines
   (group-non-empty-lines
    (split-string-into-lines string))))

(defun event-emitter()
  (let ((title-counter 0)
	(subtitle-counter 0))
    (lambda (grouped-lines)
      (let ((first-line (car grouped-lines)))
	(cond ((page-title-p first-line) (list 'page-title (subseq first-line 2)))
	      ((title-p first-line)
	       (setf subtitle-counter 0)
	       (incf title-counter)
	       (list 'title (list 'title-count title-counter) (subseq first-line 2)))
	       ((subtitle-p first-line)
		(incf subtitle-counter)
		(list 'subtitle (list 'title-count title-counter 'subtitle-count subtitle-counter) (subseq first-line 2)))
	      ((command-p first-line) (cons 'command grouped-lines))
	      (t (cons 'p grouped-lines)))))))

(defun is-close-code-tag(leaf)
  (and (stringp leaf)
       (string= "</CODE-TAG>" (string-trim " " leaf))))

(defun is-code-tag(leaf)
  (and (stringp leaf)
       (string= "<CODE-TAG>" (string-trim " " leaf))))

(defun merge-code-leaves-helper (tree accumulator code-accumulator is-in-code)
  (cond ((null tree)
	 (if (> (length code-accumulator) 1)
	     (append accumulator (list code-accumulator))
	     accumulator))
	(t (let ((leaf (car tree)))
	     (cond ((and (is-close-code-tag (second leaf))
			 is-in-code)
		    (merge-code-leaves-helper (cdr tree)
					      (append accumulator (list code-accumulator))
					      () nil))
		   ((and (is-code-tag (second leaf))
			 (not is-in-code))
		    (merge-code-leaves-helper (cdr tree)
					      accumulator
					      (list 'CODE-TAG)
					      t))
		   (is-in-code
		    (merge-code-leaves-helper (cdr tree)
					      accumulator
					      (append code-accumulator (cdr leaf))
					      t))
		   (t
		    (merge-code-leaves-helper (cdr tree)
					      (append accumulator (list leaf))
					      code-accumulator
					      is-in-code)))))))


(defun merge-code-leaves(parse-tree)
  "This function merges leaves in the parse tree if the leaves occur between <CODE-START> and <CODE-END>"
  (merge-code-leaves-helper parse-tree () () nil))

(defun build-basic-parse-tree(input-string)
  (let ((event-emitter (event-emitter)))
    (mapcar event-emitter
	    (pre-process-input input-string))))

(defun page-title(args)
  (h1 args))

(defun title(args)
  (h2 args))

(defun subtitle(args)
  (p (b args)))

(defun string-concatenator-with-helper-function (init-string separator string-list)
  (cond ((null string-list) init-string)
	(t
	 (string-concatenator-with-helper-function
	  (if (string= init-string "")
	      (concatenate 'string init-string (car string-list))
	      (concatenate 'string init-string separator (car string-list)))
	  separator
	  (cdr string-list)))))

(defun merge-strings-helper(parse-tree accumulator)
  (cond ((null parse-tree) accumulator)
	(t (let* ((leaf (car parse-tree))
		  (type (car leaf)))
	     (cond ((equal type 'code-tag)
		    (merge-strings-helper
		     (cdr parse-tree)
		     (append accumulator
			     (list (list  'CODE-TAG (string-concatenator-with-helper-function "" "~&" (cdr leaf)))))))
		   (t (merge-strings-helper
		       (cdr parse-tree)
		       (append accumulator
			       (list 
			        (if (consp (second leaf))
				    (list type (second leaf)
					  (string-concatenator-with-helper-function "" " " (cdr (cdr leaf))))
				    (list type
					  (string-concatenator-with-helper-function "" " " (cdr leaf)))))))))))))

(defun merge-strings(parse-tree)
  (merge-strings-helper parse-tree ()))

(defun parse-content(string)
  (merge-strings
   (merge-code-leaves
    (build-basic-parse-tree string))))

(defun command(str)
  "")

(defun page-title(str)
  "")

(defun generate-page-content(string)
  (let* ((title-counter 0)
	 (subtitle-counter 0)
	 (title-string "")
	 (title-list nil)
         (code-mode nil)
         (navalpha t)
         (navbeta t)
         (no-toc nil)
	 (page-content
	  (mapcar #'(lambda(x)
		      (cond ((title-p x)
			     (incf title-counter)
			     (setf subtitle-counter 0)
			     (setf title-list
				   (append title-list
					   (list (list 'title
						       (subseq x 2)))))
			     (h1 (a (list (list 'name (format nil "title~a" title-counter))
					  (list 'class "title"))
				    (subseq x 2))))
                            ((command-p x) (let ((command (string-trim " " (subseq x 2))))
                                             (cond ((string= command "no-navalpha") (setf navalpha nil))
                                                   ((string= command "no-navbeta") (setf navbeta nil))
                                                   ((string= command "no-toc") (setf no-toc t)))
                                             ""))
			    ((page-title-p x) (setf title-string (subseq x 2))
			     (list 'page-title (subseq x 2)))

                            ((code-start-p x) (progn
                                                (setf code-mode t)
                                                x))
                            ((code-end-p x) (progn
                                              (setf code-mode nil)
                                              x))
                            (code-mode x)

			    ((subtitle-p x)
			     (incf subtitle-counter)
			     (setf title-list
				   (append title-list
					   (list (list 'subtitle
						       (subseq x 3)))))
			     (h2 (a (list (list 'name (format nil "subtitle~a~a" title-counter subtitle-counter))
					  (list 'class "title"))
				    (subseq x 3))))

			    (t (p x))))
		  (remove-if #'(lambda(x) ;; Remove empty lines they have served their purpose
				 (cl-ppcre:scan "^$" x))
			     (mapcar #'(lambda(y) ;;Concatenate all groups into single strings
					 (apply #'concatenate (append (list 'string )
								      y)))
				     (group-by #'(lambda(x) ;; Group lines based on blank lines
						   (cl-ppcre:scan "^$" x))
					       (mapcar #'(lambda(str) ;; add a space to the end of every line as long as it is
							   (if (cl-ppcre:scan "^$" str) ;; not a blank line
							       str
							       (concatenate 'string str " ")))
						       (split-string-into-lines string)
						       )))))))
    (values title-string
            (if no-toc
                '()
                title-list)
            page-content navalpha navbeta)))

	      ;;       (let* ((title-list nil)
	      ;; 	     (title-counter 0)
	      ;; 	     (title-string "")
	      ;; 	     (subtitle-counter 0)
	      ;; 	     (page-content (generate-page-content filename)))

(defun compile-article(filename)
  (let ((output-file (get-output-file-name filename)))
    (with-open-file (out output-file
			 :direction :output
			 :if-exists :supersede)
      (format out
	      (multiple-value-bind ( title-string title-list page-content nav-alpha nav-beta) (generate-page-content filename)
		(page
		 (pagetitle title-string)
		 (content
		  (concatenate 'string
                               (if (not (zerop (length title-list))) ;; add a toc only if we find toc titles
                                   (create-TOC title-list)
                                   "")
			       (apply #'concatenate
				      (append (list 'string)
					      page-content))))
		 (if nav-alpha  (include-navalpha) "")
                 (if nav-beta (include-navbeta) "")
))))))

(defun main(command-line-args)
  (labels ((command-loop(args)
	     (cond ((null args)
		    t)
		   (t
		    (let ((arg (car args)))
					;Check if the extension is page...then we have a file
		      (if (string= "article"
				   (subseq arg
					   (- (length arg) 7)))
			  (compile-article arg)
			  (command-loop (cdr args))))))))
    (command-loop command-line-args)))

(defun main-func()
  (main sb-ext:*posix-argv*) ;extensions:*command-line-strings*)
  (quit))
