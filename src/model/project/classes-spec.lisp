;;;; spec-classes.lisp ---
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchies are as follows:
;;;
;;; distribution-spec
;;;   -> version-spec
;;;
;;; project-spec
;;;   -> template
;;;   version-spec
;;;   job-spec
;;;
;;; template
;;;   aspect-spec
;;;   job-spec

(cl:in-package #:jenkins.model.project)

;;; `distribution-spec' class

(defclass distribution-spec (named-mixin
                             direct-variables-mixin)
  ((versions :initarg  :versions
             :type     list ; of version-spec
             :reader   versions
             :documentation
             "Stores a list of project version specifications."))
  (:documentation
   "Instances represent specifications of distributions.

    Basically consists of variables and a set of project version
    specifications."))

(defmethod direct-variables ((thing distribution-spec))
  (value-acons :distribution-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod platform-requires ((object distribution-spec) (platform t))
  (remove-duplicates
   (append (call-next-method)
           (mappend (rcurry #'platform-requires platform)
                    (versions object)))
   :test #'string=))

(defmethod check-access ((object distribution-spec) (lower-bound t))
  (and (call-next-method)
       (if-let ((offenders (remove-if (rcurry #'check-access (access object))
                                      (versions object))))
         (let ((reasons (mapcar (lambda (version)
                                  (let+ (((&values access? reason)
                                          (check-access version (access object))))
                                    (unless access?
                                      (list version reason))))
                                offenders)))
           (values nil (make-condition
                        'simple-error
                        :format-control   "~@<~A declares ~A access but ~
                                           uses the following projects ~
                                           which do not: ~:@_~
                                           ~{~{* ~A~@[: ~A~]~}~^~:@_~}.~@:>"
                        :format-arguments (list object (access object)
                                                reasons))))
         t)))

;;; `project-spec' class

(defclass project-spec (named-mixin
                        specification-mixin
                        direct-variables-mixin
                        parented-mixin)
  ((templates :initarg  :templates
              :type     list ; of template
              :reader   templates
              :initform '()
              :documentation
              "")
   (versions  :initarg  :versions
              :type     list ; of version-spec
              :accessor versions
              :initform '()
              :documentation
              ""))
  (:documentation
   "Instances of this class describe projects.

    `project-spec' instances can reference zero or more `template'
    instances from which variables, version specifications and job
    specifications are inherited.

    In addition, `project-spec' instances directly contain version and
    job specifications."))

(defmethod direct-variables ((thing project-spec))
  (value-acons :project-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod variables :around ((thing project-spec))
  (append ;; TODO(jmoringe, 2013-02-22): this is a hack to add our
          ;; direct variables in front of variables from
          ;; templates. maybe variables should not have `append'
          ;; method combination?
          (direct-variables thing)
          (mappend #'variables (templates thing))
          ;; TODO this is a hack to not inherit the values of the
          ;; :access and :platform-requires variables from parents
          ;; like `distribution-spec' instances.
          (when-let ((parent (parent thing)))
            (remove-if (lambda (cell)
                         (member (car cell) '(:access :platform-requires)))
                       (variables parent)))))

(defmethod aspects ((thing project-spec))
  (remove-duplicates (mappend #'aspects (templates thing))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod jobs ((thing project-spec))
  (remove-duplicates (mappend #'jobs (templates thing))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod instantiate ((spec project-spec) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (let+ (((&flet make-version (spec parent)
            (when (instantiate? spec parent)
              (when-let ((version (instantiate spec :parent parent)))
                (list version)))))
         (name (name spec))
         (project (make-instance 'project
                                 :name      name
                                 :variables (variables spec) ; TODO(jmoringe, 2013-03-06): or direct-variables?
                                 )))
    (setf project
          (reinitialize-instance
           project
           :versions (mapcan (rcurry #'make-version project) (versions spec)))
          (find-instance name)
          project)))

;;; `version-spec' class

(defclass version-spec (named-mixin
                        specification-mixin
                        conditional-mixin
                        direct-variables-mixin
                        parented-mixin)
  ((requires :initarg  :requires
             :type     list
             :accessor %requires
             :initform '()
             :documentation
             "

              either a string naming the dependency or a list of the
              form

                (MECHANISM NAME VERSION)

             .")
   (provides :initarg  :provides
             :type     list
             :reader   %provides
             :initform '()
             :documentation
             "

              either a string naming the dependency or a list of the
              form

                (MECHANISM NAME VERSION)

              ."))
  (:documentation
   "Instances are project version specifications.

    Consists of variables, additional requirements and provided things
    and instantiation conditions."))

(defmethod shared-initialize :after ((instance   version-spec)
                                     (slot-names t)
                                     &key)
  ;; TODO moved from dump.lisp; but is not the right place, either
  (iter (for (mechanism name . version) in (provides instance))
        (push-provider
         instance
         (list* mechanism name
                (when (first version) (list (first version))))))

  (setf (%requires instance)
        (remove-if
         (lambda+ ((&ign name &optional version))
           (find (format nil "~(~A~@[-~A~]~)" (rs.f::normalize-name name) version)
                 (as (value instance :system-packages '()) 'list)
                 :test (lambda (a b) (ppcre:scan b a))))
         (%requires instance))))

(defmethod direct-variables ((thing version-spec))
  (value-acons :version-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod aspects ((thing version-spec))
  (aspects (parent thing)))

(defmethod jobs ((thing version-spec))
  (jobs (parent thing)))

(defmethod requires :around ((spec version-spec))
  (remove-duplicates
   (call-next-method) :test #'equal :key (rcurry #'subseq 0 2)))

(defmethod provides :around ((spec version-spec))
  (remove-duplicates
   (call-next-method) :test #'equal :key (rcurry #'subseq 0 2)))

(let+ (((&flet+ parse-spec ((mechanism name &optional version))
          (list* (make-keyword (string-upcase mechanism))
                 name
                 (etypecase version
                   (list   version)
                   (string (list (parse-version version))))))))

  (defmethod requires ((spec version-spec))
    (append (mapcar #'parse-spec (as (value spec :extra-requires '()) 'list))
            (%requires spec)))

  (defmethod provides ((spec version-spec))
    (append (mapcar #'parse-spec (as (value spec :extra-provides '()) 'list))
            (%provides spec))))

(defmethod requires-of-kind ((kind t) (spec version-spec))
  (remove kind (requires spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod provides-of-kind ((kind t) (spec version-spec))
  (remove kind (provides spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod check-access ((object version-spec) (lower-bound t))
  (let ((offender (or (when (as (value object :scm.credentials nil) '(or null string))
                        :scm.credentials)
                      (when (as (value object :scm.password nil) '(or null string))
                        :scm.password))))
    (cond
      ((not offender)
       (call-next-method))
      ((eq (access object) :public)
       (values nil (make-condition
                    'simple-error
                    :format-control   "~@<Project ~A has a ~S entry but ~
                                       ~A access.~@:>"
                    :format-arguments (list object offender
                                            (access object)))))
      (t
       (call-next-method)))))

(defmethod instantiate ((spec version-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (let+ (((&flet make-job (job-spec parent)
            (when (instantiate? job-spec parent)
              (when-let ((job (instantiate job-spec :parent parent :specification-parent spec)))
                (list job)))))
         (version (make-instance 'version
                                 :name      (name spec)
                                 :parent    parent
                                 :variables (direct-variables spec))))
    (reinitialize-instance
     version
     :jobs (mapcan (rcurry #'make-job version) (jobs spec)))))

;;; `job-spec' class

(defclass job-spec (named-mixin
                    specification-mixin
                    conditional-mixin
                    direct-variables-mixin
                    parented-mixin)
  ()
  (:documentation
   "Specification of a build job to be generated."))

(defmethod instantiate ((spec job-spec) &key parent specification-parent)
  (let+ (((&flet make-aspect (spec parent)
            (when (instantiate? spec parent)
              (when-let ((aspect (instantiate spec :parent parent)))
                (list aspect)))))
         (job (make-instance 'job
                             :name      (name spec)
                             :parent    parent
                             :variables (copy-list (direct-variables spec))))) ; TODO copy-list?
    (reinitialize-instance
     job :aspects (mapcan (rcurry #'make-aspect job)
                          (aspects specification-parent)))))

;;; `template' class

(defclass template (named-mixin
                    direct-variables-mixin)
  ((inherit :initarg  :inherit
            :type     list
            :reader   inherit
            :initform '()
            :documentation
            "")
   (aspects :initarg  :aspects
            :type     list ; of aspect
            :reader   %direct-aspects
            :initform '()
            :documentation
            "")
   (jobs    :initarg  :jobs
            :type     list ; of job
            :reader   %direct-jobs
            :initform '()
            :documentation
            ""))
  (:documentation
   "A collection of variables, aspects and jobs to use for a project version."))

;;; TODO(jmoringe, 2013-01-16): move to correct file

(defmethod variables :around ((thing template))
  (append (call-next-method) (mappend #'variables (inherit thing))))

(defmethod direct-aspects ((thing template))
  (copy-list (%direct-aspects thing)))

(defmethod aspects ((thing template))
  (remove-duplicates (append (direct-aspects thing)
                             (mapcan (compose #'copy-list #'aspects) (inherit thing))) ; TODO(jmoringe, 2013-03-11): copy?
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod direct-jobs ((thing template))
  (copy-list (%direct-jobs thing)))

(defmethod jobs ((thing template))
  (remove-duplicates (append (direct-jobs thing)
                             (mapcan (compose #'copy-list #'jobs) (inherit thing))) ; TODO(jmoringe, 2013-03-11): copy?
                     :test     #'string=
                     :key      #'name
                     :from-end t))

;;; `aspect-spec'

(defclass aspect-spec (named-mixin
                       specification-mixin
                       conditional-mixin
                       direct-variables-mixin
                       parented-mixin)
  ((aspect :initarg  :aspect
           :type     string
           :reader   aspect
           :documentation
           "Name of the aspect class that should be used to implement
            this specification."))
  (:default-initargs
   :aspect (missing-required-initarg 'aspect-spec :aspect))
  (:documentation
   "Specification of an aspect that should be applied to a build job."))

(defmethod instantiate ((spec aspect-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (make-aspect (aspect spec)
               :name      (name spec)
               :parent    parent
               :variables (direct-variables spec)))