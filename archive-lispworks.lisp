
(defun archive-article (client message group target)
  (let* ((target (parse-namestring target))
         (group-path (merge-pathnames (make-pathname :directory (list :relative group))
                                      target))
         (article-path (merge-pathnames (make-pathname :name (princ-to-string message)
                                                       :type "eml")
                                        group-path)))
    (ensure-directories-exist article-path)
    (unless (probe-file article-path)
      (with-open-file (s article-path
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
        (princ (cl-nntp:article client :article-number message)
               s)))))

(defun archiver (server group target)
  (let ((client  (cl-nntp::make-client)))
    (cl-nntp:connect server 119 client)
    (cl-nntp:group group client)
    (loop for message-num = (mp:process-wait-for-event)
          until (eql message-num :quit)
          do
          (archive-article client message-num group target))))

(defun start-workers (count &rest args)
  (coerce (loop repeat count
                for x from 0
                collect (apply 'mp:process-run-function
                               (format nil "archiver ~d" x)
                               ()
                               'archiver
                               args))
          'vector))
