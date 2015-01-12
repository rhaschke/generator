;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.api
   #:jenkins.dsl)

  (:shadow
   #:node #:as)

  (:shadowing-import-from #:jenkins.api
   #:parameters)

  (:shadowing-import-from #:jenkins.dsl
   #:job)

  (:import-from #:jenkins.version
   #:parse-version
   #:version>=
   #:version-matches)

  ;; Conditions
  (:export
   #:instantiation-condition
   #:instantiation-condition-specification

   #:instantiation-error

   #:deployment-condition
   #:deployment-condition-thing

   #:deployment-error)

  ;; Template protocol
  (:export
   #:find-template
   )

  ;; Project specification protocol
  (:export
   #:templates
   #:versions
   #:jobs)

  ;; Version specification protocol and class
  (:export
   #:version-spec)

  ;; Template specification protocol
  (:export
   #:inherit
   #:aspects
   #:jobs)

  ;; Aspect specification protocol
  (:export
   #:aspects)

  ;; Project protocol
  (:export
   #:find-project)

  ;; Instance protocol
  (:export
   #:find-instance)

  ;; Provider registry
  (:export
   #:find-provider
   #:find-provider/version
   #:providers/alist)

  ;; Platform requirements protocol
  (:export
   #:platform-requires)

  ;; Access protocol
  (:export
   #:access
   #:check-access)

  ;; Instantiation protocol
  (:export
   #:instantiate?
   #:instantiate
   #:add-dependencies!)

  ;; Deployment protocol
  (:export
   #:deploy
   #:deploy-dependencies)

  ;; Implementation protocol
  (:export
   #:specification)

  ;; Specification protocol
  (:export
   #:implementation
   #:implementations)

  ;; Name protocol
  (:export
   #:name)

  ;; Dependency protocol
  (:export
   #:direct-dependencies
   #:dependencies
   #:minimal-dependencies)

  ;; Variable protocol
  (:export
   #:*traced-variables*

   #:value-list
   #:value-list*
   #:value-cons
   #:value-acons
   #:value-parse

   #:direct-variables
   #:variables
   #:lookup
   #:value

   #:as)

  ;; JSON stuff
  (:export
   #:load-template/json
   #:load-project-spec/json
   #:load-distribution/json)

  (:documentation
   "TODO"))
