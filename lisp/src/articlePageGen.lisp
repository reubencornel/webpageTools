(in-package :cl-user)
;(asdf:oos 'asdf:load-op 'cl-ppcre)
;; Load 

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
  (not (null (cl-ppcre:scan "^\\*\\* " x))))
  ;; (assert (stringp x))
  ;; (let ((first-3 (subseq x 0 3)))
  ;;   (string= "** " first-3)))

(defun subtitle-p(x)
  (not (null (cl-ppcre:scan "^\\*\\*\\* " x))))
  ;; (assert (stringp x))
  ;; (let ((first-4 (subseq x 0 4)))
  ;;   (string= "*** " first-4)))

(defun page-title-p(x)
  (not (null (cl-ppcre:scan "^\\* " x))))
  ;; (assert (stringp x))
  ;; (let ((first-2 (subseq x 0 2)))
  ;;   (string= "* " first-2)))

(defun get-output-file-name(filename)
  (assert (stringp filename))
  (concatenate 'string (subseq filename 0 (- (length filename) 8))
	       ".html"))

(defun code-start-p(x)
  (string= "CODE_START" x))

(defun code-end-p(x)
  (string= "CODE_END" x))

(defun generate-page-content(filename)
  (let* ((title-counter 0)
	 (subtitle-counter 0)
	 (title-string "")
	 (title-list nil)
         (code-mode nil)
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
			    ((page-title-p x) (setf title-string (subseq x 2))
			     "")
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
						       (cl-ppcre:split (coerce '(#\Newline) 'string) ;; Split the contents of the file on newline
								       (read-file filename)))))))))
    (values title-counter subtitle-counter title-string title-list page-content)))

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
	      (multiple-value-bind (title-counter subtitle-counter title-string title-list page-content) (generate-page-content filename)
		(page 
		 (pagetitle title-string)
		 (content
		  (concatenate 'string 
			       (create-TOC title-list)
			       (apply #'concatenate 
				      (append (list 'string)
					      page-content))))
;		 (include-navalpha)
;		 (include-navbeta)
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