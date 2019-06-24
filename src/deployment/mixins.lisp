;;;; mixins.lisp --- Mixins provided by the target module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

(defclass command-mixin ()
  ((%command :initarg  :command
             :reader   command))
  (:default-initargs
   :command (more-conditions:missing-required-initarg 'command-mixin :command)))

(defmethod print-items:print-items append ((object command-mixin))
  (let+ (((&values command shortened?)
          (util::maybe-truncate (command object))))
    `((:command ,(list command shortened?) "~{\"~A~:[~;…~]\"~}"))))
