;;;; job.lisp --- Deployment of Jenkins jobs.
;;;;
;;;; Copyright (C) 2012, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

;;; Deploy `distribution'

(defmethod deploy:deploy ((thing project:distribution) (target target))
  (let ((jobs (call-next-method)))
    (when-let ((dependency-jobs
                (remove :none jobs
                        :key (rcurry #'var:value/cast :dependencies.mode))))
      (with-sequence-progress (:deploy/dependencies jobs)
        (map nil (lambda (job)
                   (progress "~/print-items:format-print-items/"
                             (print-items:print-items job))
                   (deploy:deploy-dependencies job target))
             dependency-jobs)))
    jobs))

;;; Deploy `job'

(defmethod deploy:deploy ((thing project::job) (target target))
  (let+ ((id        (substitute-if-not
                     #\_ #'jenkins.api:job-name-character?
                     (var:value/cast thing :build-job-name)))
         (kind      (let+ (((kind &optional plugin)
                            (ensure-list (var:value thing :kind))))
                      (if plugin
                          (list kind plugin)
                          (make-keyword (string-upcase kind)))))
         (disabled? (var:value/cast thing :build-job.disabled? nil))
         ((&flet make-new-job (&optional existing-job)
            (let ((job (make-instance 'jenkins.api:job
                                      :id        id
                                      :check-id? t
                                      :kind      kind
                                      :populate? t)))
              ;; Retain value of disabled slot unless
              ;; `:force-disabled' has been specified.
              (setf (jenkins.api:disabled? job)
                    (cond ((eq disabled? :force-disabled)
                           t)
                          ((not existing-job)
                           disabled?)
                          (t
                           (jenkins.api:disabled? existing-job))))

              (push job (model:implementations thing))

              ;; Apply aspects, respecting declared ordering, and sort
              ;; generated builders according to declared ordering.
              (aspects:extend! job (aspects:aspects thing) thing)

              ;; TODO temp
              (xloc:->xml job (stp:root (jenkins.api::%data job)) 'jenkins.api:job)

              job)))
         (existing-job  (when (jenkins.api:job? id)
                          (jenkins.api:job id)))
         (existing-kind (when existing-job
                          (jenkins.api:kind existing-job)))
         (config        (jenkins.api::%data (make-new-job existing-job))))

    ;; Create the Jenkins job.
    (cond ((not existing-job)
           (log:info "~@<Creating new job ~A~@:>" id)
           (jenkins.api:make-job id config))

          ((or (equal kind existing-kind)
               (case kind
                 (:project (string= existing-kind "project"))
                 (:matrix  (string= existing-kind "matrix-project"))))
           (log:info "~@<Updating existing job ~A~@:>" existing-job)
           (setf (jenkins.api:job-config id) config))

          (t
           (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                     id (jenkins.api:kind existing-job) kind)
           (jenkins.api:delete-job id)
           (jenkins.api:make-job id config)))

    thing))

(defmethod deploy:deploy-dependencies ((thing project::job) (target target))
  (let ((relevant-dependencies
          (ecase (var:value/cast thing :dependencies.mode :direct)
            (:direct  (model:direct-dependencies thing))
            (:minimal (model:minimal-dependencies thing))
            (:none    '())))
        (required-upstream-result
          (ecase (var:value/cast thing :dependencies.required-upstream-result
                                 :success)
            (:success  :success)
            (:unstable :unstable)
            (:any      :failure)))
        (job (model:implementation thing)))
    ;; Look at required results in all upstream jobs, potentially
    ;; relaxing REQUIRED-UPSTREAM-RESULT to `:unstable' or `:failure'.
    (map nil (lambda (upstream-job)
               (ecase (var:value/cast upstream-job :dependencies.required-result
                                      :success)
                 (:success)
                 (:unstable
                  (case required-upstream-result
                    (:success
                     (setf required-upstream-result :unstable))))
                 (:any
                  (case required-upstream-result
                    ((:unstable :success)
                     (setf required-upstream-result :failure))))))
         relevant-dependencies)
    ;; Set threshold on "reverse" trigger based on required upstream
    ;; result.
    (aspects::with-interface (jenkins.api:triggers job)
        (trigger (jenkins.api::trigger/reverse))
      (setf (jenkins.api::threshold trigger) required-upstream-result))
    ;; Install relations between JOB and its upstream jobs.
    (map nil (lambda (upstream-job)
               (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                              upstream-job thing)
                 (handler-bind
                     ((error (lambda (condition)
                               (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                      upstream-job thing condition))))
                   (jenkins.api:relate (model:implementation upstream-job) job
                                       :if-related nil))))
         relevant-dependencies)))

;;; Deleting old jobs

(defun generated? (job)
  (search "automatically generated" (jenkins.api:description job)))

(defun make-delete-other-pattern (pattern distributions)
  (cond (pattern)
        ((length= 1 distributions)
         `(:sequence ,(model:name (first distributions)) :end-anchor))
        (t
         `(:sequence
           (:alternation ,@(map 'list #'model:name distributions))
           :end-anchor))))

(defun delete-other-jobs (all-jobs pattern)
  (log:info "Deleting other jobs using pattern ~S" pattern)
  (let* ((other-jobs     (set-difference
                          (jenkins.api:all-jobs pattern) all-jobs
                          :key #'jenkins.api:id :test #'string=))
         (generated-jobs (remove-if-not #'generated? other-jobs)))
    (with-sequence-progress (:delete-other generated-jobs)
      (mapc (progressing #'jenkins.api:delete-job :delete-other)
            generated-jobs))))

(defun maybe-delete-other-jobs (distributions all-jobs target)
  (let+ (((&accessors-r/o delete-other? delete-other-pattern) target))
    (when delete-other?
      (delete-other-jobs all-jobs (make-delete-other-pattern
                                   delete-other-pattern distributions)))))