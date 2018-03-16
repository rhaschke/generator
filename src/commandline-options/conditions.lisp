;;;; conditions.lisp --- Conditions signaled by the commandline-options module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-options)

(define-condition option-context-condition (condition)
  ((context :initarg :context
            :type    string
            :reader  context
            :documentation
            "Stores the context for which the condition was
             signaled."))
  (:default-initargs
   :context (missing-required-initarg 'option-condition :context))
  (:documentation
   "Superclass for conditions involving an option context."))

(define-condition context-not-found-error (option-context-condition
                                           error)
  ()
  (:documentation
   "Signaled when a requested context does not exist."))

(define-condition option-condition (condition)
  ((option  :initarg :option
            :type    option-designator
            :reader  option
            :documentation
            "Stores a designator of the option for which the condition
             was signaled."))
  (:default-initargs
   :option (missing-required-initarg 'option-condition :option))
  (:documentation
   "Superclass for option-related conditions."))

(define-condition option-not-found-error (option-context-condition
                                          option-condition
                                          error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o context option) condition))
       (format stream "~@<~:[\"~A\" is not a known option of~;A ~
                       positional option at position ~D is not valid ~
                       for~] the \"~A\" context.~@:>"
               (integerp option) option context))))
  (:documentation
   "Signaled when a specified option does not exist."))

(define-condition mandatory-options-not-supplied-error
    (option-context-condition
     error)
  ((missing :initarg :missing
            :type    list
            :reader  missing
            :documentation
            "Stores the option info objects for the unsupplied
             options."))
  (:default-initargs
   :missing (missing-required-initarg 'mandatory-options-not-supplied-error :missing))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o context missing) condition))
       (format stream "~@<The following option~*~P ~
                       ~:*~[~;is~:;are~]~2:* mandatory for the \"~A\" ~
                       context but ~[~;has~:;have~] not been supplied: ~
                       ~{~A~^, ~}.~@:>"
               context (length missing) missing))))
  (:documentation
   "Signaled when at least one mandatory option is not supplied."))
