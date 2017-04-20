;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

;;; Dependencies protocol

(defgeneric direct-dependencies (thing)
  (:documentation
   "Return a list of direct (as opposed to transitive) dependencies of
    THING."))

(defgeneric dependencies (thing)
  (:documentation
   "Return a duplicate-free list of transitive dependencies of
    THING."))

(defgeneric minimal-dependencies (thing)
  (:documentation
   "Return a list of direct dependencies of THING that are not also
    transitive dependencies of the direct dependencies of THING."))

;; Default behavior

(defmethod dependencies ((thing t))
  (let+ ((result '())
         ((&labels one-thing (thing)
            (dolist (dependency (direct-dependencies thing))
              (unless (member dependency result :test #'eq)
                (push dependency result)
                (one-thing dependency))))))
    (one-thing thing)
    result))

(defmethod minimal-dependencies ((thing t))
  (let* ((direct-dependencies (direct-dependencies thing))
         (indirect            (reduce #'union direct-dependencies
                                      :key           #'dependencies
                                      :initial-value '())))
    (set-difference direct-dependencies indirect)))

;;; Access protocol

(defgeneric access (object)
  (:documentation
   "Return the access specification for OBJECT, either :private
    or :public."))

(defgeneric check-access (object lower-bound)
  (:documentation
   "Return true if OBJECT permits at least the level of access
    indicates by LOWER-BOUND. If OBJECT does not permit the access,
    return nil and a optionally a condition instance describing the
    reason as a second value."))

(defmethod access ((object t))
  (switch ((as (value object :access nil) '(or null string)) :test #'equal)
    (nil       :public)
    ("public"  :public)
    ("private" :private)))

(defmethod check-access ((object t) (lower-bound t))
  t)

(defmethod check-access ((object t) (lower-bound (eql :public)))
  (eq (access object) lower-bound))

;;; Instantiation protocol

(defgeneric instantiate? (spec parent)
  (:documentation
   "Return non-nil when SPEC should be instantiated."))

(defgeneric instantiate (spec &key parent specification-parent)
  (:documentation
   "Instantiate the specification SPEC and return the created
    object.

    Signal `instantiation-condition's such as `instantiation-error'
    when conditions such as errors are encountered."))

(defgeneric add-dependencies! (thing spec &key providers)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod instantiate? ((spec t) (parent t))
  t)

(defmethod instantiate :around ((spec t) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (with-simple-restart (continue "~@<Skip instantiation of ~A.~@:>" spec)
      (let ((implementation (call-next-method)))
        (setf (%specification implementation) spec)
        (push implementation (%implementations spec))
        (assert implementation)
        implementation))))

(defmethod add-dependencies! :around ((thing t) (spec t)
                                      &key providers)
  (declare (ignore providers))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (with-simple-restart (continue "~@<Skip adding dependencies to ~A ~
                                    according to ~A.~@:>"
                                   thing spec)
      (call-next-method))))

;;; Deployment protocol

(defgeneric deploy (thing)
  (:documentation
   "Deploy THING .

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

(defgeneric deploy-dependencies (thing)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod deploy :around ((thing t))
  (with-condition-translation
      (((error deployment-error)
        :thing thing))
    (with-simple-restart (continue "~@<Skip deployment of ~A.~@:>" thing)
      (call-next-method))))

(defmethod deploy-dependencies :around ((thing t))
  (with-condition-translation
      (((error deployment-error)
        :thing thing))
    (with-simple-restart (continue "~@<Skip deploying dependencies of ~
                                    ~A.~@:>"
                                   thing)
      (call-next-method))))