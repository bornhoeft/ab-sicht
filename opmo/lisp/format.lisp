;; see: http://www.gigamonkeys.com/book/a-few-format-recipes.html

(with-open-file 
    (stream 
     (merge-pathnames 
      (make-pathname 
       :directory      
       (list :relative 
             (format nil "Desktop/qlists/ch~a" ch-no))
       :name (format nil "cue~a-ch~a" cue-no ch-no) :type "txt")
      (user-homedir-pathname))
     :direction :output
     :if-exists :new-version)
  (format stream 
          (format nil "~{~a ~a~^; ~}~%" timelis)))


