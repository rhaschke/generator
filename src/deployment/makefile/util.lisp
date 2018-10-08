;;;; util.lisp --- Utilities used in the target.makefile module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

(defun escape-dollars (string)
  (with-output-to-string (stream)
    (loop :for char :across string
          :when (char= char #\$)
          :do (write-char #\$ stream)
          :do (write-char char stream))))
