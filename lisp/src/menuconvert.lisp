(defun reformat-navalpha-file(filename)
  (reformat-file 'navalpha filename "navalpha.menu"))

(defun reformat-navbeta-file(filename)
  (reformat-file 'navbeta filename "navbeta.menu"))

(defun reformat-file(file-type filename output-file)
  (with-open-file (output-file-handler output-file
				       :if-exists :supersede
				       :direction :output)
    
    (format output-file-handler "(~a~%" file-type)
    (with-open-file (file-handler filename)
      (do ((line (read-line file-handler nil nil)
		 (read-line file-handler nil nil)))
	  ((equal nil line))
	(if (and (not (null line)) 
		 (not (equal (position #\# line) 0)))
	    (format output-file-handler "'~s~&" (break-up-line #\| line)))))
    (format output-file-handler ")")))
  
  (defun break-up-line(char line)
  (if (equal 'standard-char (type-of char))
      (let ((char-position (position char line)))
	(if (null char-position)
	    (list line)
	    (cons (subseq line 0 char-position)
		  (break-up-line char 
				 (subseq line (+  char-position 1))))))))
  
;(break-up-line #\| "Home Sweet Home|index.html")   

(defun main(command-line-args)
  (defun command-loop(args)
    (cond ((null args) t)
	  (t
	   (cond ((string= (first args) "navalpha")
		  (progn
		    (reformat-navalpha-file (second args))
		    (command-loop (cddr args))))
		 ((string= (first args) "navbeta")
		  (progn
		    (reformat-navbeta-file (second args))
		    (command-loop (cddr args))))
		 (t
		  (command-loop (cdr args)))))))
    (command-loop command-line-args))

(defun main-func()
  (main sb-ext:*posix-argv*) ;extensions:*command-line-strings*)
  (quit))