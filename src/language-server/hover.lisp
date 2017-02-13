;;;; hover.lisp --- Hover contributors for different contexts.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

(defclass variable-hover-contributor () ())

(defmethod contrib:hover-contribution ((workspace   t)
                                       (document    t)
                                       (context     variable-value-context)
                                       (contributor variable-hover-contributor))
  (let ((variable-location (variable-location context))
        (variable-node     (variable-node context)))
    (values
     (list (format nil "Type: ~A" (var:variable-info-type variable-node))
           (or (var:variable-info-documentation variable-node)
               "«undocumented variable»"))
     (text.source-location:range variable-location) )))

;;;

(defclass project-version-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-version-hover-contributor))
  (let ((prefix (prefix context)))
    (log:error prefix)
    (map nil (lambda (project)
               (let ((name (model:name project)))
                 (when (starts-with-subseq prefix name)
                   (return-from contrib:hover-contribution
                     (values (describe-project project))))))
         (projects (workspace document)))))
