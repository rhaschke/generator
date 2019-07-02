;;;; model.lisp --- TODO.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

(defclass dockerfile ()
  (#+maybe (%filename :initarg :filename
              :reader  filename)
   (%stages   :initarg :stages
              :reader  stages)))

(defclass stage (model:named-mixin)
  ((%base-image   :initarg  :base-image
                  :reader   base-image)
   (%run-strategy :initarg  :run-strategy
                  :reader   run-strategy)
   ;; TODO use steps instead
   (%scripts      :initarg  :scripts
                  :type     (or null (cons (cons string (cons string null)) list))
                  :reader   scripts
                  :initform '())
   (%steps        :initarg  :steps
                  :reader   steps)))

(defclass script (model:named-mixin)
  ((%body :initarg :body
          :type    string
          :reader  body)))

(defclass copy ()
  ((%from-stage :initarg :from-stage
                :reader  from-stage)
   (%source     :initarg :source
                :type    string
                :reader  source)
   (%target     :initarg :target
                :type    string
                :reader  target))
  (:documentation
   "Copy results from previous stages."))
;;
;; COPY --from=0 /go/src/github.com/alexellis/href-counter/app .

(defclass dockerfile-job (model:named-mixin
                          model:implementation-mixin
                          aspects::aspect-builder-defining-mixin)
  ((%builders :accessor builders
              :initform '())))

(defclass shell-command (deploy:command-mixin
                         print-items:print-items-mixin)
  ((%aspect :initarg :aspect
            :reader  aspect)))

(defun shell-command (aspect format-control &rest format-arguments)
  (let ((command (if format-arguments
                     (apply #'format nil format-control format-arguments)
                     format-control)))
    (make-instance 'shell-command :aspect aspect :command command)))
