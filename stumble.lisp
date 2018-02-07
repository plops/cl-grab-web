#+nil
(with-open-file (in "/dev/urandom"
		    :if-does-not-exist nil
		    :external-format
		    '(#+nil :latin-1
		      #-nil :utf-8 :replacement #\?))
  (loop for line = (read-line in nil)
     while line
     for i from 0 below 19 do
       (format t
	       "~a~%" line)))
(ql:quickload
 '(:drakma :split-sequence :cl-ppcre-unicode :cl-ppcre :babel :montezuma))

(defparameter *url*
  "(\\w+://[-\\w\\./_?=%&]+)")

(defun http-p (url)
  (cl-ppcre:all-matches
   "\\bhttp://" url))

(defun known-binary-p (url)
  (let ((b
	 '(".png"
	   ".bmp"
	   ".jpg"
	   ".jpeg"
	   ".exe"
	   ".dmg"
	   ".package"
	   ".css"
	   ".ico"
	   ".gif"
	   ".dtd"
	   ".pdf"
	   ".xml"
	   ".tgz"
	   ".tar"
	   ".tar.gz"
	   ".tar.bz2"
	   ".tar.xz"
	   ".txz"
	   ".tbz"
	   ".mp4"
	   ".mp3"
	   ".webm"
	  
	   )))
    (dolist (e b NIL)
      (when (search e url)
	(return T)))))

(defun find-links (url)
  (when (and (http-p url)
	     (not (known-binary-p url)))
    (handler-case
	(progn
	  (format t "try to get ~a~%" url)
	 (let ((page (drakma:http-request url)))
	   (when page
	     (values (cl-ppcre:all-matches-as-strings
		      *url*
		      page)
		     page))))
      (sb-int:simple-stream-error (e)
	(format t
	 "stream error ~a when accessing ~a" e url))
      (drakma::drakma-simple-error (e)
	(format t
	 "drakma error ~a when accessing ~a" e url))
      (usocket:timeout-error (e)
	(format t
	 "timeout on socket error ~a when accessing ~a" e url))
      (usocket:ns-host-not-found-error (e)
	(format t
	 "host not found error ~a when accessing ~a" e url))
      (flexi-streams:external-format-encoding-error (e)
	(format t
	 "encoding error ~a when accessing ~a" e url)))))


(defparameter *idx* nil)


(defun walk-page (top-url action depth)
  (let ((seen))
    (labels ((walker (url depth)
	       (unless (zerop depth)
		 (push url seen)
		 (multiple-value-bind (links page) (find-links url)
		   (format t "size: ~a links: ~a commit to store~%" (length page) (length links))
		   (let ((doc (make-instance 'montezuma:document)))
		     (montezuma:add-field doc (montezuma:make-field
					       "content"
					       page
					       :stored t
					       :index :tokenized))
		     (montezuma:add-field doc (montezuma:make-field
					       "url"
					       url
					       :stored t
					       :index :untokenized))
		     (montezuma:add-document-to-index
		      *idx* doc)
		     (montezuma:flush *idx*))
		   (loop for link in links
		      for i from 0 do ;dolist (link links)
		    (unless (member link seen
				    :test #'string=)
		      #+nil(format t "   ~a/~a commit to seen~%" i (length links))
		      (push link seen)
		      
		      (funcall action link depth)
		      (walker link (1- depth))))))))
      (walker top-url depth))))


(defun run-trotter ()
  (let ((count 0))
   (walk-page
    "http://news.ycombinator.com/"
    #+nil "http://planet.lisp.org/"
    #+nil "http://news.baidu.com/"
    #'(lambda (url depth)
	#+nil(format t
		"   global-count=~6d depth=~2a: ~a~%" count depth url)
	(incf count))
    3)))

#+nil
(progn
  (format t
	  "let's start!~%")
  (setf *idx* (make-instance
		     'montezuma:index
		     :path "/home/martin/web.idx")
	)
  (time (run-trotter))
  (montezuma:flush *idx*)
  (time (montezuma:optimize *idx*))
  (montezuma:close *idx*))
#+nil
(setf *idx* (make-instance
		     'montezuma:index
		     :path "/home/martin/web.idx")
	)
#+nil
(montezuma:search-each *idx* "content:\"thread\""
		       #'(lambda (doc score)
			   (format T "~&Document ~S found with score of ~S." doc score)))
#+nil
(montezuma:document-value (montezuma:get-document *idx* 75) "content")
#+nil
(montezuma:document-value (montezuma:get-document *idx* 75) "url")
#+nil
(montezuma:optimize *idx*)
#+nil
(montezuma:close *idx*)
#+nil
(montezuma:flush *idx*)
