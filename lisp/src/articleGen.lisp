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

(defun generate-toc-entries(titles)
  (mapcar #'(lambda(title)
	      (let ((title-type (car title)))
		(cond ((equal title-type 'ARTICLE-TITLE)
		       (list 'div (list 'quote '((class "TOCTitle")))
			     (append
			      (list 'a (list 'quote (list (list 'href (format nil "#title~a" (second (second (second title))))))))
			      (cdr (cdr title)))))
		      ((equal title-type 'SUBTITLE)
		       (append
			(list 'div (list 'quote '((class "TOCSubtitle")))
			      (append 
			       (list 'a (list 'quote (list (list 'href (format nil "#subtitle~a~a" (second (second (second title)))
									       (fourth (second (Second title))))))) )
			       (cdr (cdr title))))))
		      (t nil))))
	  titles))

(defun create-new-TOC(titles)
  (list 'div  (list 'quote '((id "TOCWrapper")))
	(list 'div (list 'quote '((id "tableofcontents")))
	      (list 'div (list 'quote '((class "TOCHeader")))
		    "Table Of Contents"
		    (list 'hr))
	      (append
	       (list 'div (list 'quote '((class "TOCContents"))))
	       (generate-toc-entries titles)))))


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
	(cond ((page-title-p first-line) (list 'pagetitle (subseq first-line 2)))
	      ((title-p first-line)
	       (setf subtitle-counter 0)
	       (incf title-counter)
	       (list 'article-title (list 'quote (list 'title-count title-counter)) (subseq first-line 2)))
	       ((subtitle-p first-line)
		(incf subtitle-counter)
		(list 'subtitle (list 'quote (list 'title-count title-counter 'subtitle-count subtitle-counter)) (subseq first-line 2)))
	      ((command-p first-line) (cons 'command (list (string-upcase (string-trim " " (subseq first-line 2))))))
	      (t (cons 'p grouped-lines)))))))

(defun command(&rest arg))

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

(defun article-title(&rest args)
  (h1 (a (list (list 'name (format nil "title~a" (second (first args))))
	       (list 'class "title"))
	 (second args))))

(defun subtitle(&rest args)
    (h2 (a (list (list 'name (format nil "subtitle~a~a" (second (first args))  (fourth (first args))))
		 (list 'class "subtitle"))
	   (second args))))


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

(defun filter-titles(parse-tree)
  (remove-if-not #'(lambda(x)
		 (or (equal (car x) 'article-title)
		     (equal (car x) 'subtitle)))
		 parse-tree))

(defun filter-commands(parse-tree)
  (remove-if-not #'(lambda(x)
		     (equal (car x) 'command))
		 parse-tree))

(defun interpret-commands(parse-tree)
  (let* ((commands (mapcar #'(lambda(x) (intern (second x))) (filter-commands parse-tree)))
	 (supported-commands (list 'NO-TOC 'NO-NAVALPHA 'NO-NAVBETA)))
    (mapcar #'(lambda(command)
		(find command commands :test #'string=))
	    supported-commands)))

(defun add-toc(commands parse-tree)
  (if (find 'NO-TOC commands)
      parse-tree
      (append (list (first parse-tree))
	      (list (create-new-TOC (filter-titles parse-tree)))
	      (cdr parse-tree))))

(defun add-navbeta(commands parse-tree)
  (if (find 'NO-NAVBETA commands)
      parse-tree
      (let* ((local-navbeta-path (concatenate 'string (sb-ext:posix-getenv "PWD") "/" "navbeta.menu"))
	     (file (if (null (probe-file  local-navbeta-path))
				 *default-navbeta*
				 local-navbeta-path)))
	(with-open-file (in  file
			     :if-does-not-exist nil)
	  (if (null in)
	      "File not found"
	      (append
	       parse-tree
	       (list (read in))))))))
      

(defun add-navalpha(commands parse-tree)
    (if (find 'NO-NAVALPHA commands)
      parse-tree
      (with-open-file (in *default-navalpha*
			  :if-does-not-exist nil)
	(append
	 parse-tree
	 (list (read in))))))

(defun wrap-in-div(attrs to-be-wrapped)
  "attrs is a list of list of attrs"
  (cons 'div (cons (list 'quote attrs)
		   to-be-wrapped)))

(defun add-content-div(parse-tree)
  (let ((first-node (car parse-tree))
	(remaining (cdr parse-tree)))
    (list first-node
	  (wrap-in-div '((class "col-lg-5 order-3 offset-lg-1"))
		       (list (wrap-in-div '((class "content"))
					  remaining))))))


(defun add-row-div(parse-tree)
  (let ((first-node (car parse-tree))
	(remaining (cdr parse-tree)))
    (list
    (wrap-in-div '((class "container"))
		  (list first-node
			(wrap-in-div '(("class" "row"))
				     remaining))))))

(defun build-page (commands parse-tree)
  (add-row-div
   (add-navbeta commands
		(add-navalpha commands
			      (add-content-div
			       (add-toc commands parse-tree))))))

(defun compile-article(filename)
  (let ((output-file (get-output-file-name filename)))
    (with-open-file (out output-file
			 :direction :output
			 :if-exists :supersede)
      (let* ((parse-tree (parse-content (read-file filename)))
	     (commands (interpret-commands parse-tree)))
	(format out (apply #'page (build-page commands parse-tree)))))))
    
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
