;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchy is as follows:
;;;
;;; project
;;;   version
;;;     job
;;;       aspect

(cl:in-package #:jenkins.model.project)

;;; `distribution' class

(defclass distribution (named-mixin
                        implementation-mixin)
  ((versions :initarg :versions
             :type    list #|of version|#
             :reader  versions))
  (:documentation
   "Instances represent implementations of `distributions-spec's.

    Contained versions are `version' instance implementing
    `version-spec's."))

(defmethod lookup ((thing distribution) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (lookup (specification thing) name :if-undefined nil))

(defmethod persons-in-role ((role t) (container distribution))
  (persons-in-role role (specification container)))

;;; `version' class

(defclass version (named-mixin
                   implementation-mixin
                   parented-mixin
                   direct-variables-mixin)
  ((direct-dependencies :initarg  :direct-dependencies
                        :type     list #| of (version . reasons) |#
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "List of pairs of the form

                           (VERSION . REASONS)

                         where VERSION is either

                         + `nil' indicating unresolved requirements

                         + `:system' indicating requirements resolved
                           via a system dependency.

                         + or a `version' instance representing
                           project versions on which the project
                           version depends.

                         REASONS is a list of requirements of the form

                           (NATURE TARGET [VERSION])

                         .")
   (jobs                :initarg  :jobs
                        :type     list
                        :reader   jobs
                        :documentation
                        ""))
  (:documentation
   "Instances of this class represent versions of `project's."))

(defmethod print-items:print-items append ((object version))
  (let ((ancestor-names (list (name (parent object))
                              (name (parent (specification object)))
                              (name object))))
    `((:name ,ancestor-names "~{~A~^:~}"))))

(defmethod direct-variables ((thing version))
  (value-acons :version-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod lookup ((thing version) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (multiple-value-call #'merge-lookup-values
    (lookup (specification thing) name :if-undefined nil)
    (if (variable-inheritable? name)
        (call-next-method)
        (values nil nil nil))))

(defmethod variables append ((thing version))
  (variables (specification thing)))

(defmethod direct-dependencies/reasons ((thing version))
  (%direct-dependencies thing))

(defmethod direct-dependencies ((thing version))
  (mapcan (lambda+ ((target . &ign))
            (unless (member target '(nil :system) :test #'eq)
              (list target)))
          (%direct-dependencies thing)))

(defmethod add-dependencies! ((thing version) (spec version-spec)
                              &key
                              (providers (missing-required-argument :providers)))
  (let+ (platform-provides (platform-provides? nil)
         ((&flet platform-provides ()
            (if platform-provides?
                platform-provides
                (setf platform-provides? t
                      platform-provides  (platform-provides thing)))))
         ((&flet add-dependency (required provider)
            (let ((cell (or (assoc provider (%direct-dependencies thing))
                            (let ((new (cons provider '())))
                              (push new (%direct-dependencies thing))
                              new))))
              (pushnew required (cdr cell) :test #'equal)))))
    (iter (for requires in (requires spec))
          (log:trace "~@<Trying to satisfy requirement ~S for ~A.~@:>"
                     requires thing)
          (restart-case
              (cond ((when-let* ((match     (find-provider/version
                                             requires
                                             :if-does-not-exist nil
                                             :providers         providers))
                                 (candidate (implementation match)))
                       (log:trace "~@<Best candidate is ~S.~@:>" candidate)
                       (unless (eq candidate thing)
                         (add-dependency requires candidate))
                       t))
                    ((when (find-provider/version
                            requires :providers (platform-provides))
                       (add-dependency requires :system)
                       t)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip requirement ~S.~@:>" requires))
              (declare (ignore condition))
              ;; Record unresolved requirement.
              (add-dependency requires nil)))))

  (mapc #'add-dependencies! (jobs thing) (jobs spec)))

(defmethod deploy ((thing version))
  (mapcar #'deploy (jobs thing)))

;;; `job' class

(defclass job (named-mixin
               implementation-mixin
               specification-mixin ; TODO define another class for this
               parented-mixin)
  ((direct-dependencies :initarg  :direct-dependencies ; TODO(jmoringe, 2013-03-06): dependencies-mixin?
                        :type     list ; of job
                        :reader   direct-dependencies
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "List of other `job' instances.")
   (aspects             :initarg  :aspects
                        :type     list
                        :reader   aspects
                        :initform '()
                        :documentation
                        "List of aspects associated to the job."))
  (:documentation
   "Instances of this class represent build jobs which are associated
    to specific `version's of `project's."))

(defmethod lookup ((thing job) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (multiple-value-call #'merge-lookup-values
    (lookup (specification thing) name :if-undefined nil)
    (call-next-method)))

(defmethod add-dependencies! ((thing job) (spec job-spec)
                              &key providers)
  (declare (ignore providers))
  (let ((dependency-name (value/cast thing :dependency-job-name (name thing))))
    (iter (for dependency in (direct-dependencies (parent thing)))
          (log:trace "~@<Trying to add ~A to ~A~@:>" dependency thing)
          (with-simple-restart (continue "~@<Skip adding dependency ~A.~@:>"
                                         dependency)
            (let ((dependency
                    (or (find dependency-name (jobs dependency)
                              :test #'string= :key #'name)
                        (error "~@<Could not find ~S in the jobs of ~
                               ~A~@[ (~{~A~^, ~})~]~@:>"
                               dependency-name dependency (jobs dependency)))))
              (pushnew dependency (%direct-dependencies thing)))))))

(defmethod deploy ((thing job))
  (let+ ((id        (substitute-if-not
                     #\_ #'jenkins.api:job-name-character?
                     (value/cast thing :build-job-name)))
         (kind      (let+ (((kind &optional plugin)
                            (ensure-list (value thing :kind))))
                      (if plugin
                          (list kind plugin)
                          (make-keyword (string-upcase kind)))))
         (disabled? (value/cast thing :build-job.disabled? nil))
         ((&flet make-new-job (&optional existing-job)
            (let ((job (jenkins.dsl:job (kind id))))
              ;; Retain value of disabled slot unless
              ;; `:force-disabled' has been specified.
              (cond
                ((eq disabled? :force-disabled)
                 (setf (disabled? job) t))
                ((not existing-job)
                 (setf (disabled? job) disabled?))
                (t
                 (setf (disabled? job) (disabled? existing-job))))

              (push job (jenkins.model:implementations thing))

              ;; Apply aspects, respecting declared ordering, and sort
              ;; generated builders according to declared ordering.
              (jenkins.model.aspects:extend! job (aspects thing) thing)

              ;; TODO temp
              (xloc:->xml job (stp:root (jenkins.api::%data job)) 'jenkins.api:job)

              job))))

    ;; Create the actual Jenkins job.
    (let* ((existing-job  (when (jenkins.api:job? id)
                            (jenkins.api:job id)))
           (existing-kind (when existing-job
                            (jenkins.api:kind existing-job))))
      (cond
        ((not existing-job)
         (let ((new-job (make-new-job)))
           (log:info "~@<Creating new job ~A~@:>" new-job)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))

        ((or (equal kind existing-kind)
             (case kind
               (:project (string= existing-kind "project"))
               (:matrix  (string= existing-kind "matrix-project"))))
         (let ((new-job (make-new-job existing-job)))
           (log:info "~@<Updating existing job ~A~@:>" existing-job)
           (setf (jenkins.api:job-config id)
                 (jenkins.api::%data new-job))))

        (t
         (let ((new-job (make-new-job existing-job)))
           (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                     new-job (jenkins.api:kind existing-job) kind)
           (jenkins.api:delete-job id)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))))

    thing))

(defmethod deploy-dependencies ((thing job))
  (let ((relevant-dependencies
         (ecase (value/cast thing :dependencies.mode :direct)
           (:direct  (direct-dependencies thing))
           (:minimal (minimal-dependencies thing))
           (:none    '()))))
    (iter (for upstream-job in relevant-dependencies)
          (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                         upstream-job thing)
            (handler-bind
                ((error (lambda (condition)
                          (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                 upstream-job thing condition))))
              (jenkins.api:relate (implementation upstream-job) (implementation thing)
                                  :if-related nil))))))
