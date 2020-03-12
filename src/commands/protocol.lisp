;;;; protocol.lisp --- Protocol provided by the commands module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

;;; Command service

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass command-service (service-provider:standard-service
                             service-provider::change-hook-mixin)
    ()))

(service-provider:define-service command
  (:service-class command-service))

;;; Command protocol

(defgeneric make-command (provider &rest args)
  (:method ((provider t) &rest args)
    (apply #'service-provider:make-provider 'command provider args))
  (:documentation
   "Make and return a command according to PROVIDER and ARGS."))

(defgeneric command-execute (command)
  (:documentation
   "Execute the already-configured COMMAND."))

(defgeneric context-elements (command)
  (:method-combination append)
  (:method append ((command t))
    (log:debug "Computing context elements for ~A" command)
    '()))

;;; Command configuration

(defvar *command-schema*
  (configuration.options.service-provider:service-schema
   (service-provider:find-service 'command)))

;;; High-level interface: find, instantiate and execute a command

(defun configure-command (synchronizer name arguments)
  "Configure the command designate by NAME with ARGUMENTS.

   SYNCHRONIZER is responsible for putting the indicated option values
   into a configuration container."
  (let+ ((prefix (configuration.options:make-name "commands"))
         ((&flet notify (name event value &key (raw? t))
            (configuration.options:notify
             synchronizer name event value :source :commandline :raw? raw?)))
         ((&flet set-value (name value error-handler)
            (let ((name (configuration.options:merge-names
                         prefix name)))
              (handler-bind ((error error-handler))
                (notify :added     name nil)
                (notify :new-value name value
                        :raw? (not (typep value 'boolean))))))))

    ;; Select provider according to NAME.
    (set-value '("provider") name
               (lambda (condition)
                 (declare (ignore condition))
                 (error 'command-not-found-error :command name)))

    ;; Configure provider according to ARGUMENTS.
    (build-generator.commandline-options:map-commandline-options
     (lambda (option-path value)
       (set-value option-path value
                  (lambda (condition)
                    (error 'option-value-error
                           :context name
                           :option  (second option-path)
                           :command name
                           :value   value
                           :cause   condition))))
     name arguments)))

(defvar *configuration*)
(defvar *temp-directory*)
(defvar *cache-directory*)
(defvar *age-limit*)

(defun make-context-function (command)
  (let ((elements (context-elements command)))
    (lambda (worker-function)
      (funcall
       (reduce (lambda (function next)
                 (lambda ()
                   (funcall function next)))
               elements :initial-value worker-function :from-end t)))))

(defun execute-command (command
                        &key
                        configuration
                        (num-processes  1)
                        (error-policy   #'error)
                        (progress-style :cmake)
                        temp-directory
                        cache-directory
                        age-limit
                        trace-variables)
  ;; We modify the global value so that all threads pick up the value
  ;; without additional work.
  (setf var:*traced-variables* (map 'list (compose #'make-keyword
                                                   #'string-upcase)
                                    trace-variables))

  (let ((main-thread (bt:current-thread))
        (lock        (bt:make-lock)))
    (setf lparallel:*kernel* (lparallel:make-kernel
                              num-processes
                              :context (make-context-function command)))
    (unwind-protect-case ()
        (handler-bind ((error   error-policy)
                       (warning error-policy)
                       (more-conditions:progress-condition
                         (make-progress-handler progress-style)))
          (lparallel:task-handler-bind
              ((error   error-policy)
               (warning error-policy)
               (more-conditions:progress-condition
                (lambda (condition)
                  (unless (eq progress-style :none)
                    (bt:interrupt-thread
                     main-thread (lambda ()
                                   (sb-sys:without-interrupts
                                     (bt:with-lock-held (lock)
                                       (signal condition)))))))))
            (let ((*configuration*   configuration)
                  (*temp-directory*  temp-directory)
                  (*cache-directory* cache-directory)
                  (*age-limit*       age-limit))
              (command-execute command))))
      (:normal
       (lparallel:end-kernel :wait t))
      (:abort
       (lparallel:end-kernel :wait nil)
       (lparallel:kill-tasks :default)))))

(defun make-progress-handler (style &key (stream *error-output*))
  (lambda (condition)
    (case style
      (:none)
      (:cmake
       (princ condition stream)
       (fresh-line stream))
      (:one-line
       (let* ((progress      (progress-condition-progress condition))
              (progress/real (progress->real progress))
              (width    20))
         (format stream "~C[2K[~VA] ~A~C[G"
                 #\Escape
                 width
                 (make-string (floor progress/real (/ width))
                              :initial-element #\#)
                 condition
                 #\Escape)
         (if (eq progress t)
             (terpri stream)
             (force-output stream)))))))
