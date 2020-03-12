;;;; target.lisp --- Target definition for generating a Travis.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.travis)

;;; `travis-target'

(defclass travis-target ()
  ((output-directory :initarg :output-directory
                     :type    pathname
                     :reader  output-directory
                     :documentation
                     #.(format nil "The directory into which the ~
                        Dockerfile and the associated scripts should ~
                        be written.")))
  (:documentation
   "Write a Travis that builds or install the specified projects."))

(service-provider:register-provider/class
 'deploy:target :travis :class 'travis-target)

;;; `project-rules'

(defclass project-rules (model:implementation-mixin
                         aspects::aspect-builder-defining-mixin)
  ((%directory :initarg  :directory
               :reader   directory)
   (%rules     :initarg  :rules
               :accessor rules
               :initform '()))
  (:documentation
   "A collection of `rule' instances for one project."))

(defun make-project-rules (specification directory)
  (make-instance 'project-rules
                 :directory     directory
                 :specification specification))

;;; `rule'

(defclass rule (deploy:command-mixin
                print-items:print-items-mixin)
  ((%name         :initarg  :name
                  :reader   name)
   (%dependencies :initarg  :dependencies
                  :accessor dependencies
                  :initform '())
   (%early?       :initarg  :early?
                  :type     boolean
                  :reader   early?
                  :initform nil
                  :documentation
                  "Controls whether the rule can be executed
                   \"early\", that is disregarding inter-project
                   dependencies.")
   ;; HACK
   (%builder-class :initarg :builder-class
                   :reader builder-class
                   :initform nil))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'rule :name)))

(defun make-rule (name command &key (dependencies '()) early? builder-class)
  (make-instance 'rule :name          name
                       :command       command
                       :dependencies  dependencies
                       :early?        early?
                       :builder-class builder-class))

(defmethod print-items:print-items append ((object rule))
  `((:name ,(name object) "~A " ((:before :command)))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   rule))
  (when-let ((builder-class (builder-class step)))
    (let* ((variable        (let ((*package* (find-package '#:keyword)))
                              (symbolicate  '#:aspect.builder-constraints.
                                            builder-class)))
           (constraints/raw (var:value aspect variable nil))
           (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
      (log:debug "~@<Constraints for ~A in ~A~:@_~
                  ~/aspects::format-constraints/~@:>"
                 step variable constraints)
      constraints)))

(defun make-ensure-directory-rule (directory)
  (make-rule "ensure-directory" (format nil "mkdir -p '~A'" directory)))

;;;

(defun write-rule (stream name &key dependencies directory command comment)
  (let* ((rule-name (util:safe-name name))
         (log-file  (format nil "~A.log" rule-name))
         (prefix    (string #\Tab)))
    ;; Write comment and rule head.
    (format stream "~@[# ~A~%~]~
                    ~A:~{ ~A~}~@
                    "
            comment rule-name (map 'list #'util:safe-name dependencies))
    ;; Write rule body (called "recipe" in the make documentation).
    (when command
      (pprint-logical-block (stream (list command) :per-line-prefix prefix)
        (format stream "@~
                        echo -en '\\e[1mExecuting ~A\\e[0m\\n'~@
                        +(~@
                          set -e~@
                          ~@[~
                            cd '~A'~@
                            export WORKSPACE=\"$$(pwd)\"~@
                          ~]~
                          ~@
                          ~A~@:_~
                        ) > '~A' 2>&1~@
                        if [ $$? -ne 0 ] ; then~@
                        ~2@Techo -en '\\e[35m'~@
                        ~2@Tcat '~:*~A'~@
                        ~2@Techo -en '\\e[0m'~@
                        ~2@Texit 1~@
                        fi~@
                        touch '~4:*~A'"
                rule-name directory (escape-dollars command) log-file))
      (terpri stream))
    (terpri stream)))

(defmethod deploy:deploy ((thing project::job) (target travis-target))
  (let* ((directory (deploy:job-full-name thing))
         (output    (make-project-rules thing directory)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :travis)

    output))

(defun write-project-rules (stream thing)
  (let+ ((specification        (model:specification thing))
         (name                 (deploy:job-full-name specification))
         (directory            (directory thing))
         (rules                (rules thing))
         (project-dependencies (map 'list #'deploy:job-full-name
                                    (model:direct-dependencies
                                     specification)))
         (ensure-directory     (make-ensure-directory-rule
                                directory))
         ((&flet rule-name (rule)
            (format nil "~A-~A" name (name rule)))))
    ;; Header/separator
    (deploy:print-heading stream name)

    ;; Preparation rule
    (write-rule stream (rule-name ensure-directory)
                :command (deploy:command ensure-directory))

    ;; Actual rules
    (map nil (lambda (rule)
               (with-simple-restart
                   (continue "~@<Skip ~A~@:>" rule)
                 (let ((dependencies (append
                                      (map 'list #'rule-name
                                           (list* ensure-directory
                                                  (dependencies rule)))
                                      (unless (early? rule)
                                        project-dependencies))))
                   (write-rule stream (rule-name rule)
                               :dependencies dependencies
                               :directory    directory
                               :command      (deploy:command rule)))))
         rules)

    ;; Interface rule
    (write-rule stream name :dependencies (map 'list #'rule-name rules))
    name))

(defmethod project::finish-deploy ((distributions   sequence)
                                   (deployed-things sequence)
                                   (target          travis-target))
  (let ((travis        (merge-pathnames "Travis" (output-directory target)))
        (project-rules   '())
        (interface-rules '()))
    ;; Generate rule text for all projects and collect the names of
    ;; interface rules.
    (map nil (lambda (thing)
               (with-simple-restart (continue "~@<Skip ~A~@:>" thing)
                 (let* ((stream         (make-string-output-stream))
                        (interface-rule (write-project-rules stream thing)))
                   (push (get-output-stream-string stream) project-rules)
                   (push interface-rule interface-rules))))
         deployed-things)

    ;; Write the Travis.
    (ensure-directories-exist travis)
    (with-output-to-file (stream travis :if-exists :supersede)
      (deploy:print-heading stream "TODO header")
      ;; TODO header

      ;; Execute the recipe lines of each rule as a single shell chunk
      ;; of shell instead of one shell invocation per line (or
      ;; multiple continuation lines using "\"). Note that we have to
      ;; pass -c to the shell so it doesn't attempt to execute the
      ;; shell code chunk as a command.
      (format stream ".ONESHELL:~@
                      SHELL = /bin/bash~@
                      .SHELLFLAGS = -c~@
                      ~2%")

      ;; Add an "all" rule for convenience.
      (write-rule stream "all" :dependencies interface-rules)

      ;; Write project rules.
      (format stream "~{~A~^~2%~}" project-rules))))
