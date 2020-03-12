;;;; job.lisp --- Job model class.
;;;;
;;;; Copyright (C) 2012-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `job/project' class
;;;
;;; Aggregated classes:
;;;
;;; * `scm'
;;; * `properties'
;;; * `trigger'
;;; * `builder'
;;; * `publisher'

(define-model-class job/project ()
  ((description                :type     string
                               :initform nil)
   (disabled?                  :type     boolean
                               :xpath    "disabled/text()"
                               :initform nil)
   (block-on-downstream-build? :type     boolean
                               :xpath    "blockBuildWhenDownstreamBuilding/text()"
                               :initform nil)
   (block-on-upstream-build?   :type     boolean
                               :xpath    "blockBuildWhenUpstreamBuilding/text()"
                               :initform nil)
   (can-roam?                  :type     boolean
                               :xpath    "canRoam/text()"
                               :initform t)
   (restrict-to-slaves         :type     string
                               :xpath    "assignedNode/text()"
                               :initform nil
                               :optional? t)
   ;; Interface-based children
   (properties                 :type     property
                               :xpath    ("properties/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (triggers                   :type     trigger
                               :xpath    ("triggers/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (repository                 :type     scm
                               :xpath    ("scm")
                               :initform nil)
   (build-wrappers             :type     build-wrapper
                               :xpath    ("buildWrappers/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (builders                   :type     builder
                               :xpath    ("builders/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (publishers                 :type     publisher
                               :xpath    ("publishers/*"
                                          :if-multiple-matches :all)
                               :initform '())

   ;; TODO Not sure about these
   (slaves          :type     string/node ; TODO(jmoringe, 2012-07-10): not correct
                    :xpath    ("axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
                               :if-multiple-matches :all)
                    :optional? t
                    :initform '())
   (permissions     :type     access-control-rule
                    :xpath    ("properties/hudson.security.AuthorizationMatrixProperty/permission"
                               :if-multiple-matches :all)
                    :initform '())
   (jdk             :type     string
                    :xpath    "jdk/text()"
                    :initform nil))
  (:root? t))

(defun job-name-character? (character)
  (or (alphanumericp character) (member character '(#\- #\_ #\.))))

(defun job-name? (name)
  (every #'job-name-character? name))

(deftype job-name ()
  '(satisfies job-name?))

(defun check-job-name (name)
  (unless (job-name? name)
    (let ((offenders (map 'list (lambda (char)
                                  (list char (char-name char)))
                          (remove-duplicates
                           (remove-if #'job-name-character? name)))))
      (error 'simple-type-error
             :datum            name
             :expected-type    'job-name
             :format-control   "~@<Supplied job name ~S contains illegal ~
                                character~P: ~{~{~A (~@[~A~])~}~^, ~}.~@:>"
             :format-arguments (list name (length offenders) offenders)))))

(defmethod shared-initialize :around ((instance   job/project)
                                      (slot-names t)
                                      &rest args &key
                                      populate?)
  ;; Only initialize all slots if POPULATE? is true. Otherwise, the
  ;; uninitialized slots will be populated lazily from the remote
  ;; state.
  (if populate?
      (call-next-method)
      (apply #'call-next-method instance '(id get-func put-func) args)))

(defmethod initialize-instance :before ((instance job/project)
                                        &key
                                        id
                                        check-id?
                                        kind
                                        populate?)
  (when check-id? (check-job-name id))
  ;; Setting the kind requires the `%data' slot to be initialized
  ;; which is only the case when POPULATE? is true.
  (when (and kind (not populate?))
    (incompatible-initargs 'job/project :kind kind :populate? populate?)))

(defmethod initialize-instance :after ((instance job/project)
                                       &key
                                       kind
                                       populate?)
  ;; When POPULATE? is true, all slots are initialized to default
  ;; values. Put an empty document into the `%data' slot to match that
  ;; state.
  (when populate?
    (setf (%data instance) (stp:make-document (stp:make-element "project")))
    (when kind
      (setf (kind instance) kind))))

(defmethod kind ((object job/project))
  (let ((element-name (stp:local-name (stp:document-element (%data object)))))
    (make-keyword (string-upcase element-name))))

(defmethod (setf kind) ((new-value (eql :project))
                        (object    job/project))
  (setf (kind object) "project"))

(defmethod (setf kind) ((new-value (eql :matrix))
                        (object    job/project))
  (setf (kind object) '("matrix-project" "matrix-project@1.4")))

(defmethod (setf kind) ((new-value string)
                        (object    job/project))
  (setf (kind object) (list new-value))
  new-value)

(defmethod (setf kind) ((new-value cons)
                        (object    job/project))
  (let+ (((local-name &optional plugin) new-value)
         (root (stp:document-element (%data object))))
    (setf (stp:local-name root) local-name)
    (when plugin
      (setf (stp:attribute-value root "plugin") plugin))
    new-value))

(defmethod upstream ((object job/project))
  (when-let ((reverse (trigger-of-type 'trigger/reverse object)))
            (upstream-projects reverse)))

(defmethod (setf upstream) ((new-value list) (object job/project))
  (let ((reverse (or (trigger-of-type 'trigger/reverse object) ; TODO make a function or macro ensure-...
                     (let ((instance (make-instance 'trigger/reverse)))
                       (appendf (triggers object) (list instance))
                       instance))))
    (setf (upstream-projects reverse) new-value)))

;;; Permissions

(defmethod grant ((job job/project) (subject string) (action cons))
  (pushnew (list subject action) (permissions job) :test #'equal)
  (permissions job))

(defmethod revoke ((job job/project) (subject string) (action cons))
  (removef (permissions job) (list subject action) :test #'equal)
  (permissions job))

(macrolet
    ((define-permission-methods (name)
       `(progn
          (defmethod ,name ((job string) (subject t) (action t))
            (,name (job job) subject action))

          (defmethod ,name ((job job/project) (subject list) (action t))
            (mapc #'(lambda (subject) (,name job subject action)) subject)
            (permissions job)))))

  (define-permission-methods grant)
  (define-permission-methods revoke))
