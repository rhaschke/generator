;;;; package.lisp --- Package definition for the deployment.travis module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.deployment.travis
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:shadow
   #:directory)

  (:local-nicknames
   (#:util    #:build-generator.util)

   (#:model   #:build-generator.model)
   (#:var     #:build-generator.model.variables)
   (#:project #:build-generator.model.project)
   (#:aspects #:build-generator.model.aspects)

   (#:deploy  #:build-generator.deployment)))
