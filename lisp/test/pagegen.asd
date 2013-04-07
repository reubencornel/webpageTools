(asdf:defsystem #:pagegen
  :components ((:file "pagegen")
	       (:file "photoalbum" 
		      :depends-on ("pagegen"))))