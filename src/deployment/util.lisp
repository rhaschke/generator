;;;; util.lisp --- Utilities provided by the target module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

(defun job-full-name (thing)
  (let* ((version (model:parent thing))
         (project (model:parent (model:specification version))))
    (format nil "~A@~A" (model:name project) (model:name version))))

(defun print-heading (stream title &key (width 80))
  (format stream "##~V,,,'#<~>##~@
                  # ~:*~V<~A~;~> #~@
                  ~2:*##~V,,,'#<~>##~2%"
          width title))
