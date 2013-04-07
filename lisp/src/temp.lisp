(defmacro testmacro(funcname)
  `(defun ,funcname()
    (print "Hello World")))


(testmacro name)


(defvar functionname (testmacro test))

(func)

(defmacro testmacro1()
  `(lambda()
    (print "Hello World 1")))

(defvar functionname1 (testmacro1))

(funcall functionname1)


(defun code (&rest code-listing)
  (return-string-from-list
   (let ((indent-char "&nbsp;")
	 (indent-level 0)
	 (indent-width 4)
	 (line-count 0))
     (mapcar #'(lambda(x)
		 (setf line-count (+ 1 line-count))
		 (if (position #\{ x)
		     (setf indent-level (+ 1 indent-level)))
		 (if (position #\{ x)
		     (setf indent-level (- indent-level 1)))
		 (concatenate 'string 
			      (format nil "~a" line-count)
			      indent-char
			      x
			      (br)
			      "~%"))
	     code-listing))))
			     
    

(format t (code "{" 
      "int main()"
      "{"
      "printf(\"Hello World\");"
      "}"))