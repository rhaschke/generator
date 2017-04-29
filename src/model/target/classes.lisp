;;;; classes.lisp --- .
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.target)

(defclass target (model:named-mixin
                  var:direct-variables-mixin)
  ()
  (:documentation
   "Instances of this class describe deployment targets.

    `target' instances can reference zero or more `template' instances
    from which variables, version specifications and job
    specifications are inherited."))
