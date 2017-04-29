;;;; package.lisp --- Package definition for the model.target module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.model.target
  (:use
   #:cl)

  (:local-nicknames
   (#:model #:build-generator.model)
   (#:var   #:build-generator.model.variables))

  (:export
   #:target))
