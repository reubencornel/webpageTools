(ql:quickload 'cl-ppcre)

(defvar *default-navalpha* "/home/reuben/webpage/navalpha.menu")
(defvar *default-navbeta* "/home/reuben/webpage/index/navbeta.menu")
(defun return-string-from-list(string-list)
  (string-concatenator-helper-function "" (flatten-list string-list)))

(defun string-concatenator-helper-function (init-string string-list)
  (cond ((null string-list) init-string)
	(t
	 (string-concatenator-helper-function
	  (concatenate 'string init-string (car string-list))
	  (cdr string-list)))))

(defun printattrs(parameters collector)
  (let ((current-args (car parameters)))
    (cond ((null parameters) collector)
	  (t
	   (printattrs
	    (cdr parameters)
	    (return-string-from-list
	     (list 
	      collector
	      (format nil " ~a" (car current-args))
	      " = " 
	      (format nil "'~a' " (car (cdr current-args))))))))))

(defun flatten-list(seq)
  (cond ((null seq)
	 '())
	((not (consp seq))
	 (list seq))
	(t
	 (append
	  (flatten-list (car seq))
	  (flatten-list (cdr seq))))))

(defmacro htmlmacro(name &optional single-tag)
  `(defun ,name(&rest parameters)
     (return-string-from-list
      (list
       "~&<"
       (format nil "~a" ,name)
       (if (consp (car parameters))
	   (printattrs (car parameters) ""))
       (if ,single-tag
	   "/")
       ">~&"
       (if (not (consp (car parameters)))
	   parameters
	(return-string-from-list (cdr parameters)))
       (if (not ,single-tag)
	   (list
	    "~&</"
	    (format nil "~a" ,name)
	    ">~&"))))))

(defmacro gen-html(output-file  html-list)
  `(with-open-file (output-stream ,output-file 
		    :direction :output
		    :if-exists :supersede)
    (format output-stream ,html-list)))

(defmacro gen-html1(output-file  html-list)
  ` (format t ,html-list))


;;;;;All macros defined now we do our tag specific stuff

(defvar html (htmlmacro html))
(defvar body (htmlmacro body))
(defvar head (htmlmacro head))
(defvar title (htmlmacro title))
(defvar br (htmlmacro br t))
(defvar ol (htmlmacro ol))
(defvar ul (htmlmacro ul))
(defvar li (htmlmacro li))
(defvar p (htmlmacro p))
(defvar a (htmlmacro a))
(defvar i (htmlmacro i))
(defvar b (htmlmacro b))
(defvar div (htmlmacro div))
(defvar h1 (htmlmacro h1))
(defvar h2 (htmlmacro h2))
(defvar h3 (htmlmacro h3))
(defvar code-tag (htmlmacro code-tag))
(defvar link (htmlmacro link t))
(defvar center (htmlmacro center))
(defvar img (htmlmacro img t))
(defvar script (htmlmacro script))
(defvar table (htmlmacro table))
(defvar tr (htmlmacro tr))
(defvar td (htmlmacro td))
(defvar input (htmlmacro input t))
(defun bold(text)
  (b text))

(defun insert-image(filename)
    (img '(("src" filename))))


(defun ordered-list(&rest stuff)
  (apply ol stuff))

(defun unordered-list(&rest stuff)
  (apply ul stuff))

(defun list-item(&rest stuff)
  (apply li stuff))
	   

(defun reference(link &optional text)
  (a (list (list 'href link)) 
     (if (null text)
	 link
	 text)))

(defun code(&rest code-listing)
  (concatenate 'string 
	       (br)
	       (code-tag
		(return-string-from-list
		 (flatten-list  
		  (mapcar #'(lambda(x) (list x "~&")) code-listing))))
	       (br)))
  
(defun content(&rest content-list)
  (div '(("class" "content"))
       content-list))
(defun glossary-entry(term definition)
  (with-open-file (glossary-handle "glossary" 
		   :if-exists :supersede
		   :direction :output)
    (format glossary-handle "~&~a|" term)
    (format glossary-handle "~&~a" definition)
    (format glossary-handle "~&~a" "%%")))

(defun glossary(glossary-text)
  (reference (concatenate 'string "glossary.html#" glossary-text)
	glossary-text))

(defun page(&rest page-contents)
  (html
   (head
    (title "Reuben's Webpage")
    (link '(("rel" "stylesheet") ("href" "sitestyle.css")
	    ("type" "text/css")))
    (link '(("rel" "stylesheet") ("href" "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
	    (integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
	    (crossorigin "anonymous"))))
   (body
    (if (listp page-contents)
	(return-string-from-list
	 (mapcar #'(lambda(x) (eval x)) page-contents))
	page-contents))))
  
(defun pagetitle(title-text)
  (div '(("id" "Header"))
       (center
	(h1 title-text))))

(defun generate-menu(menuitems)
  (cond ((null menuitems)
	 "")
	(t
	 (concatenate 'string
		      (reference (second (car menuitems))
				 (first (car menuitems)))
		      (br)(br)
		      (generate-menu (cdr menuitems))))))
				      
(defun navalpha(&rest menuitems)
  (div '(("class" "col-lg-2 order-1"))
  (div '(("id" "navAlpha"))
       (generate-menu menuitems))))

(defun navbeta (&rest menuitems)
  (div '(("class" "col-lg-3 offset-lg-1 order-3"))
  (div '(("id" "navBeta"))
       (generate-menu menuitems))))

(defun include-navbeta(&optional navbeta-file)
  (with-open-file (in 
		   (concatenate 'string 
				(sb-ext:posix-getenv "PWD")
				"/" "navbeta.menu"))
    (if (null in)
	"FILE NOT FOUND"
	(eval (read in)))))

(defun include-navalpha(&optional navalpha-file)
  (with-open-file (in (if (null navalpha-file)
			  *default-navalpha*
			  navalpha-file)
		      :if-does-not-exist nil)
    (if (equal in nil)
	"NAVALPHA file does not exist"
	(eval (read in)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Photo Album Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-first-elements(split-lines)
  (mapcar #'(lambda(split-line)
	      (car split-line))
	  split-lines))

(defun get-second-elements(split-lines)
  (mapcar #'(lambda(split-line)
	      (car (cdr split-line)))
	  split-lines))

(defun generate-div-names(file-name-list)
    (mapcar #'(lambda(file-name)
		(concatenate 'string "div" file-name))
     file-name-list))
  
(defun generate-div-element(div-file-name-list caption-list)
  (mapcar #'(lambda(div-file-name caption)
	      (concatenate 'string "<div id=\"" div-file-name "\" name = \"" div-file-name  "\" style='display:none'><p>" caption "</p></div>"))
	  div-file-name-list
	  caption-list))

(defun generate-javascript-list(file-name-list)
  (concatenate 'string "["
	       (reduce #'(lambda(x y)
			   (concatenate 'string x "," y))
		       file-name-list)
;		       (get-first-elements (split-lines *lines*)))
	     "];"))

(defun concatenate-double-quotes(list-of-strings)
  (mapcar #'(lambda(x)
	      (concatenate 'string "'" x "'"))
	  list-of-strings))


(defun photo-album( picture-file-list name-title)
  (html
   (head
    (link '(("rel" "stylesheet") ("href" "sitestyle.css")
	    ("type" "text/css")))

    (title name-title)
    (script "var imageList = " (generate-javascript-list (concatenate-double-quotes (get-first-elements picture-file-list)))
	    "indexValue = 0;
standardHeight = 300;
standardWidth = 500;

function changeImage()
{

	document.mainimage.src = imageList[indexValue]; 
//	alert(document.mainimage.clientHeight)
	
	var caption = 'div' + imageList[indexValue];  
	
	if (document.getElementById(caption) == null){
		document.getElementById('caption').innerHTML = '&nbsp;'
	}else{
		document.getElementById('caption').innerHTML =  document.getElementsByName(caption)[0].innerHTML;
	}
}

function prevImage()
{
	if (indexValue == 0) {
		indexValue = imageList.length - 1;
	}else{
		indexValue--;
	}
	changeImage();
}

function nextImage()
{
	var list = document.getElementById('optionlist');
	if (indexValue == imageList.length - 1 ){
		indexValue = 0;
	}else{
		indexValue++;
	}
	
	changeImage();
}"))
   (body '((onload "javascript:changeImage()"))
	 (pagetitle name-title)
	 (content
	  (reduce #'(lambda(x y)
		     (concatenate 'string x y)) 
		 (generate-div-element (generate-div-names (get-first-elements picture-file-list))
				       (get-second-elements picture-file-list)))
	 (table '((align "center") (border "0"))
		(tr (td '((colspan "3") (align "center"))
			(img '((name "mainimage") (border "1")))))
		(tr (td '((align "center") (colspan "3") (border "1"))
			(p (div '((id "caption"))))))
		(tr (td '((align "left"))
			(input '((type "button") (value "<- Back") 
				 (onclick "javascript:prevImage()"))))
		    (td)
		    (td '((align "right"))
			(input '((type "button") (value "Next ->") 
				 (onclick "javascript:nextImage()")))))))
	 (include-navalpha)
	 (include-navbeta))))

;(format t (photo-album '(("image1.jpg" "Flower 1") ("image2.jpg" "Flower 2"))
;		       "reuben"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Photo Album Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun photo-album(photo-album-file)
 ; (

;HTML elements with attributes are called as follows
;(a '((href "test")) "test")
;    +-------------+
;           |
;    This is how the argument should be passed

(defun main(command-line-args)
  (defun command-loop(args)
    (cond ((null args)
	   t)
	  (t
	   (let ((arg (car args)))
	     ;Check if the extension is page...then we have a file
	     (if (string= "page"
			  (subseq arg 
				  (- (length arg) 4)
				  (length arg)))
		 (let ((filename arg))
		   (with-open-file (in filename)
		     (eval (read in)))
		   (command-loop (cdr args)))
		 (command-loop (cdr args)))))))
  (command-loop command-line-args))

(defun main-func()
  (main sb-ext:*posix-argv*) ;extensions:*command-line-strings*)
  (quit))





