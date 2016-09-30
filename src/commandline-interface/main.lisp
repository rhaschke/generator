;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(defvar *generator-version*
  (asdf:component-version (asdf:find-system :jenkins.project)))

;;; Input

(defun load-templates (files)
  (with-sequence-progress (:templates files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (with-simple-restart
                  (continue "~@<Skip template specification ~S.~@:>" file)
                (list (load-template/json
                       file :generator-version *generator-version*))))
            files)))

(defun locate-projects (distribution-pathnames distributions)
  (remove-duplicates
   (mappend
    (lambda (distribution-pathname distribution)
      (let ((projects-directory
              (merge-pathnames
               "../projects/"
               (make-pathname :name     nil
                              :type     "project"
                              :defaults distribution-pathname))))
        (mapcan
         (lambda+ ((name &rest versions))
           (when-let ((location
                       (first
                        (locate-specifications
                         :project
                         (list (make-pathname :name     name
                                              :defaults projects-directory))))))
             (list (list location versions distribution))))
         (jenkins.model.project::versions distribution))))
    distribution-pathnames distributions)
   :test #'equalp))

(defstruct project-spec-and-versions
  (spec     nil :type jenkins.model.project::project-spec :read-only t)
  (versions nil :type list                                :read-only t))

(defmethod print-items:print-items append ((object project-spec-and-versions))
  (let+ (((&structure-r/o project-spec-and-versions- spec versions) object)
         (versions (mapcar (rcurry #'getf :name) versions)))
    (append (print-items:print-items spec)
            `((:versions ,versions ":~{~A~^,~}" ((:after :name)))))))

(defun analyze-project (project &key cache-directory temp-directory non-interactive)
  (let+ (((&structure-r/o project-spec-and-versions- (project spec) versions) project)
         ((&labels+ do-version ((version-info . info))
            (let+ ((version-name      (getf version-info :name))
                   (version-variables (remove-from-plist version-info :name))
                   ((&plist-r/o (scm              :scm)
                                (branch-directory :branch-directory)
                                (requires         :requires)
                                (provides         :provides))
                    info)
                   (other-results (remove-from-plist info
                                                     :requires :provides
                                                     :properties))
                   (version (or (find version-name (versions project)
                                      :key #'name :test #'string=)
                                (let ((version (make-instance 'version-spec
                                                              :name   version-name
                                                              :parent project)))
                                  (push version (versions project))
                                  version))))
              (reinitialize-instance
               version
               :requires  requires
               :provides  provides
               :variables (append
                           (jenkins.model.variables:direct-variables version) ; TODO
                           (apply #'value-acons (append version-variables '(())))
                           (when scm
                             (list (value-cons :scm (string-downcase scm))))
                           (when branch-directory
                             (list (value-cons :branch-directory branch-directory)))
                           (iter (for (key value) :on other-results :by #'cddr)
                                 (let ((key (format-symbol '#:keyword "ANALYSIS.~A" key)))
                                   (collect (value-cons key (to-value value))))))))))
         ((&labels+ do-version1 ((&whole arg version-info . &ign))
            (with-simple-restart
                (continue "~@<Skip version ~A.~@:>" version-info)
              (do-version arg)))))

    (handler-bind
        ((error (lambda (condition)
                  (error 'jenkins.analysis:analysis-error
                         :specification project
                         :cause         condition))))
      (mapc #'do-version1
            (macrolet ((var (name &optional default)
                         `(value project ,name ,default)))
              (apply #'jenkins.analysis:analyze
                     (when-let ((value (var :repository)))
                       (puri:uri value))
                     :auto
                     :scm              (var :scm)
                     :username         (var :scm.username)
                     :password         (var :scm.password)
                     :versions         versions
                     :sub-directory    (when-let ((value (var :sub-directory)))
                                         (parse-namestring (concatenate 'string value "/")))
                     :history-limit    (var :scm.history-limit)
                     :non-interactive  non-interactive
                     (append
                      (let ((natures (var :natures :none)))
                        (unless (eq natures :none)
                          (list :natures (mapcar (compose #'make-keyword #'string-upcase) natures))))
                      (when cache-directory
                        (list :cache-directory cache-directory))
                      (when temp-directory
                        (list :temp-directory temp-directory)))))))
    project))

(defun load-projects/versioned (files-and-versions)
  (with-sequence-progress (:load/project files-and-versions)
    (lparallel:pmapcan
     (lambda+ ((file versions distribution))
       (progress "~A" file)
       (with-simple-restart
           (continue "~@<Skip project specification ~S.~@:>" file)
         (let+ ((version-names (mapcar #'first versions))
                (project       (reinitialize-instance
                                (load-project-spec/json
                                 file
                                 :version-test      (lambda (version)
                                                      (find version version-names
                                                            :test #'string=))
                                 :generator-version *generator-version*)
                                :parent distribution))
                (branches      (as (value project :branches '()) 'list))
                (branches      (intersection version-names branches :test #'string=))
                (tags          (as (value project :tags '()) 'list))
                (tags          (intersection version-names tags :test #'string=))
                (tags+branches (union branches tags))
                (versions1     (set-difference version-names tags+branches
                                               :test #'string=))
                ((&flet process-version (name &key version-required? branch? tag? directory?)
                   (let+ ((version (or (find name (versions project)
                                             :test #'string= :key #'name)
                                       (when version-required?
                                         (error "~@<No version section ~
                                                 for version ~S in ~
                                                 project ~A.~@:>"
                                                name project))))
                          ((&flet version-var (name &optional default)
                             (if version
                                 (value version name default)
                                 default)))
                          (parameters (alist-plist (second (find name versions
                                                                 :test #'string=
                                                                 :key  #'first))))
                          (branch     (when branch?    (version-var :branch (when (eq branch? t) name))))
                          (tag        (when tag?       (version-var :tag (when (eq tag? t) name))))
                          (directory  (when directory? (version-var :directory)))
                          (commit     (version-var :commit)))
                     `(:name   ,name
                       ,@parameters
                       ,@(when branch    `(:branch    ,branch))
                       ,@(when tag       `(:tag       ,tag))
                       ,@(when directory `(:directory ,directory))
                       ,@(when commit    `(:commit    ,commit)))))))
           (list (make-project-spec-and-versions
                  :spec     project
                  :versions (append (mapcar (rcurry #'process-version :branch? t) branches)
                                    (mapcar (rcurry #'process-version :tag?    t) tags)
                                    (mapcar (rcurry #'process-version
                                                    :version-required? t
                                                    :branch?           :maybe
                                                    :tag?              :maybe
                                                    :directory?        :mabye)
                                            versions1)))))))
     :parts most-positive-fixnum files-and-versions)))

(defun analyze-projects (projects &key cache-directory temp-directory non-interactive)
  (jenkins.analysis::with-git-cache ()
    (let ((cache jenkins.analysis::*git-cache*))
      (with-sequence-progress (:analyze/project projects)
        (lparallel:pmapcan
         (lambda (project)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items project))
           (more-conditions::without-progress
             (let ((jenkins.analysis::*git-cache* cache))
               (with-simple-restart
                   (continue "~@<Skip analyzing project ~A.~@:>" project)
                 (when-let ((project (apply #'analyze-project project
                                            :non-interactive non-interactive
                                            (append
                                             (when cache-directory
                                               (list :cache-directory cache-directory))
                                             (when temp-directory
                                               (list :temp-directory temp-directory))))))
                   (list (setf (find-project (name project)) project)))))))
         :parts most-positive-fixnum projects)))))

(defun resolve-project-version (project version)
  (let ((project (find-project project)))
    (or (find version (versions project) :test #'string= :key #'name)
        (error "~@<Could not find version ~S in project ~A.~@:>"
               version project))))

(defun resolve-project-versions (versions)
  (mapcan (lambda+ ((project &rest versions))
            (mapcan (lambda+ ((version &optional &ign))
                      (with-simple-restart
                          (continue "~@<Skip version ~A of project ~A.~@:>"
                                    version project)
                        (list (resolve-project-version project version))))
                    versions))
          versions))

(defun load-distributions (files &optional (overwrites '()))
  (with-sequence-progress (:load/distribution files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (with-simple-restart
                  (continue "~@<Skip distribution specification ~S.~@:>" file)
                (let ((distribution (load-distribution/json
                                     file :generator-version *generator-version*)))
                  (iter (for (name . value) in overwrites)
                        (log:info "~@<In ~A, setting ~S to ~S.~@:>"
                                  distribution name value)
                        (setf (lookup distribution name) value))
                  (list distribution))))
            files)))

(defun check-platform-requirements
    (distributions
     &key
     (platform (multiple-value-list (jenkins.analysis:current-platform))))
  (let ((installed-packages (jenkins.analysis:installed-packages))
        (requirements       (platform-requires distributions platform)))
    (log:info "~@<Found ~:D installed package~:P~@:>"
              (length installed-packages))
    (log:debug "~@<Found ~:D platform requirement~:P: ~{~A~^ ~}~@:>"
               (length requirements) requirements)
    (when (and platform installed-packages)
      (dolist (requirement requirements)
        (with-simple-restart
            (continue "~@<Ignore the requirement ~A.~@:>" requirement)
          (or (find requirement installed-packages
                    :test #'string= :key #'first)
              (error 'jenkins.analysis:unfulfilled-platform-dependency-error
                     :dependency requirement))))))
  distributions)

(defun check-distribution-access (distributions)
  (mapcan (lambda (distribution)
            (with-simple-restart
                (continue "~@<Skip distribution ~A.~@:>" distribution)
              (let+ (((&values access? problem)
                      (check-access distribution t)))
                (cond
                  (access?
                   (list distribution))
                  (problem
                   (error problem))
                  (t
                   (error "~@<Unsuitable access declaration in ~
                           distribution ~A.~@:>"
                          distribution))))))
          distributions))

;; Deployment

(defun instantiate-projects (specs
                             &optional
                             (distributions nil distributions-supplied?))
  (assert (length= 1 distributions))
  (let+ ((projects (mapcar (rcurry #'instantiate :parent (first distributions))
                           specs))
         ((&flet find-version (version)
            (when-let ((dist (find version distributions
                                   :test #'member
                                   :key  #'versions)))
              (remove-if-not (rcurry #'member (versions dist))
                             (providers/alist)
                             :key #'cdr)))))
    (iter (for project in projects)
          (for spec    in specs)
          (when project
            (apply #'add-dependencies! project spec
                   (when distributions-supplied?
                     (list :providers #'find-version)))
            (collect project)))))

(defun deploy-projects (projects)
  (with-sequence-progress (:deploy/project projects)
    (iter (for project in projects)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items project))
          (more-conditions::without-progress
            (with-simple-restart
                (continue "~@<Skip deploying project ~S.~@:>" project)
              (appending (flatten (deploy project))))))))

(defun deploy-job-dependencies (jobs)
  (with-sequence-progress (:deploy/dependencies jobs)
    (iter (for job in jobs)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items job))
          (deploy-dependencies job))))

(defun generated? (job)
  (search "automatically generated" (jenkins.api:description job)))

;;; Toolkit specific stuff

(defun configure-orchestration (distribution)
  (with-trivial-progress (:orchestration "Configuring orchestration jobs")
    (let* ((templates (list (find-template "orchestration")))
           (spec      (make-instance 'jenkins.model.project::project-spec
                                     :name      "orchestration"
                                     :parent    distribution
                                     :templates templates))
           (version   (make-instance 'jenkins.model.project::version-spec
                                     :name   "orchestration"
                                     :parent spec)))
      (reinitialize-instance spec :versions (list version))
      (flatten (deploy (instantiate spec))))))

(defun configure-view (name jobs)
  (with-trivial-progress (:view "~A" name)
    (let ((view (make-instance 'jenkins.api:view
                               :id   name
                               :jobs '())))
      (if (jenkins.api::view? name)
          (jenkins.api::update! view)
          (jenkins.api:make-view name (jenkins.api::%data view)))
      (setf (jenkins.api:jobs view) (mapcar #'jenkins.api:id jobs))
      (jenkins.api:commit! view)
      view)))

(defun configure-distribution (distribution)
  (let* ((jobs               (mappend (compose #'jobs #'implementation)
                                      (versions distribution)))
         (orchestration-jobs (with-simple-restart
                                 (continue "~@<Continue without configuring orchestration jobs~@:>")
                               (configure-orchestration distribution)))
         (all-jobs           (mapcar #'implementation
                                     (append jobs orchestration-jobs))))
    (log:trace "~@<Jobs in ~A: ~A~@:>" distribution jobs)
    (when-let* ((create? (as (value distribution :view.create? nil) 'boolean))
                (name    (value distribution :view.name)))
      (with-simple-restart (continue "~@<Continue without creating a view~@:>")
        (configure-view name all-jobs)))
    (values jobs orchestration-jobs all-jobs)))

(defun configure-distributions (distributions)
  (values-list
   (reduce (lambda+ ((jobs orchestration-jobs all-jobs) distribution)
             (let+ (((&values jobs1 orchestration-jobs1 all-jobs1)
                     (configure-distribution distribution)))
               (list (append jobs1               jobs)
                     (append orchestration-jobs1 orchestration-jobs)
                     (append all-jobs1           all-jobs))))
           distributions
           :initial-value '(() () ()))))

(defun list-credentials (jobs)
  (let+ ((all-credentials (make-hash-table :test #'equal))
         ((&flet+ job-credentials (job)
            (when-let* ((repository  (jenkins.api:repository job))
                        (credentials (jenkins.api:credentials repository)))
              (push job (gethash credentials all-credentials))))))
    (mapc #'job-credentials jobs)
    (when (plusp (hash-table-count all-credentials))
      (format t "~@<The following credentials have been referenced and ~
                 have to be configured in Jenkins' credential store:~@:_~
                 ~{~{* ~S for job~P ~<~{~A~^, ~}~:@>~}~^~@:_~}~
                 ~%~:>"
              (mapcar (lambda+ ((credentials . jobs))
                        (list credentials (length jobs)
                              (list (mapcar #'jenkins.api:id jobs))))
                      (hash-table-alist all-credentials))))))

;;; Commandline options

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (clon:make-synopsis
   ;; Basic usage and specific options.
   :item    (clon:defgroup (:header "General Options")
              (flag    :long-name     "version"
                       :description
                       "Print version information and exit.")
              (flag    :long-name     "help"
                       :short-name    "h"
                       :description
                       "Print this help and exit.")
              (flag    :long-name     "swank"
                       :description
                       "Start a swank server.")
              (flag    :long-name     "debug"
                       :description
                       "Enable debug mode.")
              (enum    :long-name     "progress-style"
                       :enum          '(:none :cmake :one-line)
                       :description
                       "Progress display style.")
              (flag    :long-name    "non-interactive"
                       :description
                       "Avoid any user interaction.")
              (lispobj :long-name    "num-processes"
                       :short-name   "j"
                       :typespec     'positive-integer
                       :argument-name "NUMBER-OF-PROCESSES"
                       :description
                       "Number of processes to execute in parallel when checking out from repositories and analyzing working copies.")
              (enum    :long-name     "on-error"
                       :enum          '(:abort :continue)
                       :argument-name "POLICY"
                       :description
                       "Abort when encountering errors? Either \"abort\" or \"continue\".")
              (path    :long-name    "cache-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which repository mirrors should be written.")
              (path    :long-name    "temp-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which temporary files should be written during analysis step.")
              (path    :long-name    "report-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Write information about distributions and projects into one or more report files. The written information includes most of the content of the respective underlying recipe but also expanded variable values, inferred variable values and analysis results.")
              (flag    :long-name    "dry-run"
                       :description
                       "Read recipes and perform the usual analysis but do not create or delete Jenkins jobs.")
              (stropt  :long-name    "trace-variable"
                       :argument-name "VARIABLE-NAME"
                       :description
                       "Trace all accesses to the specified variable.")
              (flag    :long-name     "info-variables"
                       :description
                       "Show information about variables.")
              (flag    :long-name     "info-aspects"
                       :description
                       "Show information about available aspects."))

   :item    (clon:defgroup (:header "Processing Options")
              (path   :long-name     "template-directory"
                      :type          :directory
                      :argument-name "DIRECTORY"
                      :description
                      "Directory containing sub-directories in turn containing template files. Must be used in combination with the mode option to select one of the sub-directories.")
              (stropt :long-name     "template"
                      :short-name    "t"
                      :argument-name "TEMPLATE"
                      :description
                      "Load one or more templates. This option can be supplied multiple times. Mutually exclusive with the mode option.")
              (stropt :long-name     "distribution"
                      :short-name    "d"
                      :argument-name "DISTRIBUTION"
                      :description
                      "Load one or more distributions. This option can be supplied multiple times.")
              (stropt :long-name     "mode"
                      :short-name    "m"
                      :argument-name "MODE"
                      :description
                      "The mode according to which jobs should be generated. Selects a sub-directory of the directory specified using the template-directory option and thus a set of templates. Mutually exclusive with the template option.")
              (stropt :long-name     "set"
                      :short-name    "D"
                      :argument-name "VARIABLE-NAME=VALUE"
                      :description
                      "Overwrite a variable after loading the distribution. Arguments to this option have to be of the form VARIABLE-NAME=VALUE. This option can be supplied multiple times."))

   :item    (clon:defgroup (:header "Jenkins Options")
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
                      :description
                      "Jenkins base URI.")
              (stropt :long-name     "username"
                      :short-name    "u"
                      :description
                      "Username for Jenkins authentication.")
              (stropt :long-name     "password"
                      :short-name    "p"
                      :description
                      "Password for Jenkins authentication.")
              (stropt :long-name     "api-token"
                      :short-name    "a"
                      :description
                      "API token for Jenkins authentication.")
              (flag   :long-name     "delete-other"
                      :description
                      "Delete previously automatically generated jobs when they are not re-created in this generation run.")
              (stropt :long-name     "delete-other-pattern"
                      :argument-name "REGEX"
                      :description
                      "When deleting previously automatically generated jobs, only consider jobs whose name matches the regular expression REGEX.

A common case, deleting only jobs belonging to the distribution being generated, can be achieved using the regular expression DISTRIBUTION-NAME$."))))

(defun collect-inputs (spec)
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (list spec))
    (t
     (error "~@<Invalid input specification: ~S.~@:>" spec))))

;;; Error handling stuff

(defun call-with-delayed-error-reporting (thunk
                                          &key
                                          debug?
                                          (report-function #'report-error))
  (let+ ((errors      '())
         (errors-lock (bt:make-lock))
         ((&flet errors ()
            (bt:with-lock-held (errors-lock)
              (copy-list errors))))
         ((&flet (setf errors) (new-value)
            (bt:with-lock-held (errors-lock)
              (setf errors new-value))))
         ((&flet collect-error (condition)
            (when debug?
              (bt:with-lock-held (errors-lock)
                (terpri)
                (princ condition)
                (terpri)
                (sb-debug:print-backtrace)))
            (bt:with-lock-held (errors-lock)
              (appendf errors (list condition)))))
         ((&flet report ()
            (mapc (lambda (condition)
                    (ignore-errors
                     (funcall report-function *error-output* condition))
                    (format *error-output* "~2%"))
                  errors)))
         ((&flet call-with-deferrable-conditions (thunk)
            (restart-bind ((defer (lambda (condition)
                                    (collect-error condition)
                                    (continue)
                                    (abort))
                             :test-function (lambda (condition)
                                              (find-restart 'continue condition))))
              (funcall thunk)))))
    (unwind-protect ; TODO probably not a good idea
         ;; Execute THUNK collecting errors.
         (lparallel:task-handler-bind ((error (lambda (condition)
                                                (call-with-deferrable-conditions
                                                 (lambda () (error condition))))))
           (call-with-deferrable-conditions
            (lambda () (funcall thunk #'errors #'(setf errors) #'report))))

      ;; Report collected errors.
      (report))))

(defmacro with-delayed-error-reporting ((&key debug? report-function)
                                        &body body)
  (with-unique-names (errors set-errors report)
    `(call-with-delayed-error-reporting
      (lambda (,errors ,set-errors ,report)
        (flet ((errors () (funcall ,errors))
               ((setf errors) (new-value) (funcall ,set-errors new-value))
               (report () (funcall ,report)))
          ,@body))
      ,@(when debug? `(:debug? ,debug?))
      ,@(when report-function `(:report-function ,report-function)))))

;;; Main

(defun locate-specifications (kind namestrings)
  (with-simple-restart (continue "~@<Do not load ~A specifications.~@:>" kind)
    (or (iter (for namestring in namestrings)
              (if-let ((matches (collect-inputs (parse-namestring namestring))))
                (appending matches)
                (warn "~@<~A pattern ~S did not match anything.~@:>"
                      kind namestring)))
        (error "~@<None of the ~A patterns ~{~S~^, ~} matched ~
                anything.~@:>"
               kind namestrings))))

(defun parse-overwrite (spec)
  (let+ ((position  (or (position #\= spec)
                        (error "~@<Variable assignment ~S is not of the ~
                                form NAME=VALUE.~@:>"
                               spec)))
         (name/raw  (subseq spec 0 position))
         (name      (make-keyword (string-upcase name/raw)))
         (value/raw (subseq spec (1+ position)))
         (value     (if (and (not (emptyp value/raw))
                             (member (aref value/raw 0) '(#\" #\{ #\[)))
                        (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
                          (json:decode-json-from-string value/raw))
                        value/raw)))
    (cons name value)))

(defun call-with-phase-error-check (phase errors set-errors report continuable?
                                    thunk)
  (let ((start (get-internal-real-time)))
    (format t "START ~A~%" phase)
    (unwind-protect
         (multiple-value-prog1
             (funcall thunk)
           (when-let* ((errors       (funcall errors))
                       (phase-errors (remove-if (of-type 'phase-condition) errors)))
             (restart-case
                 (error 'simple-phase-error
                        :phase            phase
                        :format-control   "~@<~D error~:P during ~A phase.~@[ ~
                                           This error is fatal.~]~@:>"
                        :format-arguments (list (length phase-errors) phase
                                                (not continuable?)))
               (continue (&optional condition)
                 :report (lambda (stream)
                           (format stream "~@<Ignore the error:P in phase ~A ~
                                           and continue.~@:>"
                                   (length phase-errors) phase))
                 :test   (lambda (condition)
                           (declare (ignore condition))
                           continuable?)
                 (declare (ignore condition))
                 (funcall set-errors
                          (append (set-difference errors phase-errors)
                                  (list (make-condition 'deferred-phase-error
                                                        :phase      phase
                                                        :conditions phase-errors))))
                 (terpri)
                 (funcall report)
                 (funcall set-errors '())))))
      (let ((end (get-internal-real-time)))
        (format t "~&END   ~A, ~,3F second~:P~2%"
                phase
                (/ (- end start)
                   internal-time-units-per-second))))))

(defmacro with-phase-error-check ((phase errors set-errors report
                                   &key
                                   (continuable? 't))
                                  &body body)
  `(call-with-phase-error-check
    ',phase ,errors ,set-errors ,report ,continuable? (lambda () ,@body)))

(defun configure ()
  (let+ ((*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS"))) ; TODO
                                   (parse-integer value)
                                   100))
         (schema        *schema*)
         (configuration (configuration.options:make-configuration schema))
         (source        (configuration.options.sources:make-source
                         :common-cascade ; TODO environment variables
                         :basename "build-generator"
                         :syntax   :ini))
         (synchronizer  (make-instance 'configuration.options:standard-synchronizer
                                       :target configuration))
         ((&flet option-value (section name)
            (let+ ((option (configuration.options:find-option
                            (list section name) configuration))
                   ((&values value source)
                    (if (typep (configuration.options:option-type option)
                               '(cons (eql list)))
                        (iter (for spec next (clon:getopt :long-name name))
                              (while spec)
                              (collect spec :into values)
                              (finally (when values (return (values values t)))))
                        (clon:getopt :long-name name))))
              (if source
                  (values value :commandline)
                  (configuration.options:option-value
                   option :if-does-not-exist nil))))))
    ;; Process configuration options.
    (handler-case
        (progn
          (configuration.options.sources:initialize source schema)
          (configuration.options.sources:process source synchronizer))
      (error (condition)
        (format t "Configuration error:~%~A~%" condition)
        (uiop:quit 3)))

    ;; Process commandline options.
    (update-synopsis)
    (clon:make-context)

    (let ((debug? (option-value "general" "debug")))

      (when debug?
        (describe configuration)
        (fresh-line))

      (when (or (emptyp (uiop:command-line-arguments))
                (option-value "general" "help"))
        (clon:help)
        (uiop:quit))

      (when (option-value "general" "version")
        (format *standard-output* "~A version ~A~&"
                "build-generator" *generator-version*)
        (uiop:quit))

      (when (option-value "general" "info-variables")
        (print-variable-info *standard-output*)
        (uiop:quit))

      (when (option-value "general" "info-aspects")
        (print-aspect-info *standard-output*)
        (uiop:quit))

      (values #'option-value configuration debug?))))

(defun print-variable-info (stream)
  (let ((sorted (sort (copy-list (all-variables)) #'string<
                      :key #'variable-info-name)))
    (format stream "~@<~{~{~
                      \"~(~A~)\"~@[: ~(~A~)~]~
                      ~@[~@:_~2@T~<~A~:>~]~
                    ~}~^~@:_~@:_~}~:>"
            (mapcar (lambda (variable)
                      (let+ (((&structure-r/o variable-info- name type documentation)
                              variable))
                        (list name
                              (unless (eq type t) type)
                              (when documentation (list documentation)))))
                    sorted))))

(defun print-aspect-info (stream)
  (let* ((providers (service-provider:service-providers 'jenkins.model.aspects::aspect))
         (providers (sort (copy-list providers) #'string<
                          :key (compose #'string #'service-provider:provider-name))))
    (format stream "~{~<~
                      ~(~A~)~
                      ~@[~@:_~4@T~<~{~{~
                        \"~(~A~)\"~@[: ~(~A~)~]~@[ = ~A~]~
                        ~@[~@:_~A~]~
                      ~}~^~@:_~}~:>~]~
                      ~@[~@:_~2@T~A~]~
                    ~:>~^~2%~}"
            (mapcar (lambda (provider)
                      (list (service-provider:provider-name provider)
                            (when-let ((stuff (mapcar (lambda (parameter)
                                                        (let ((variable (jenkins.model.aspects:aspect-parameter-variable parameter)))
                                                          (list (variable-info-name variable)
                                                                (unless (eq (variable-info-type variable) t)
                                                                  (variable-info-type variable))
                                                                (json:encode-json-to-string
                                                                 (jenkins.model.aspects:aspect-parameter-default-value parameter))
                                                                (variable-info-documentation variable))))
                                                      (jenkins.model.aspects:aspect-parameters
                                                       (service-provider:provider-class provider)))))
                              (list stuff))
                            (documentation provider t)))
                    providers))))

(defun main ()
  (log:config :thread :warn)
  (let+ (((&values option-value &ign debug?) (configure))
         ((&flet option-value (&rest args)
            (apply option-value args)))
         (progress-style         (option-value "general" "progress-style"))
         (*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS")))
                                   (parse-integer value)
                                   200))
         (non-interactive        (option-value "general" "non-interactive"))
         (num-processes          (option-value "general" "num-processes"))
         ((&flet restart/condition (name)
            (lambda (condition)
              (when-let ((restart (find-restart name condition)))
                (invoke-restart restart condition)))))
         (non-dependency-errors? nil)
         (error-policy           (case (option-value "general" "on-error")
                                   (:continue #'continue)
                                   (t         (restart/condition 'abort))))
         (effective-error-policy (lambda (condition)
                                   (when (typep condition
                                                '(and error
                                                      (not unfulfilled-project-dependency-error)))
                                     (setf non-dependency-errors? t))
                                   (cond
                                     ((and (typep condition 'simple-phase-error)
                                           (funcall error-policy condition)
                                           nil))
                                     ((funcall (restart/condition 'defer) condition))
                                     ((funcall (restart/condition 'abort) condition)))))
         (cache-directory        (option-value "general" "cache-directory"))
         (temp-directory         (option-value "general" "temp-directory"))
         (report-directory       (option-value "general" "report-directory"))
         (dry-run?               (option-value "general" "dry-run"))

         (main (bt:current-thread))
         (lock (bt:make-lock)))
    (when debug? (log:config :trace))

    (restart-case

        (handler-bind ((error effective-error-policy)
                       (more-conditions:progress-condition
                         (lambda (condition)
                           (sb-sys:without-interrupts
                             (bt:with-lock-held (lock)
                               (case progress-style
                                 (:none)
                                 (:cmake
                                  (princ condition)
                                  (fresh-line))
                                 (:one-line
                                  (let* ((progress      (progress-condition-progress condition))
                                         (progress/real (progress->real progress))
                                         (width    20))
                                    (format t "~C[2K[~VA] ~A~C[G"
                                            #\Escape
                                            width
                                            (make-string (floor progress/real (/ width))
                                                         :initial-element #\#)
                                            condition
                                            #\Escape)
                                    (if (eq progress t)
                                        (terpri)
                                        (force-output))))))))))
          (lparallel:task-handler-bind ((error effective-error-policy)
                                        (more-conditions:progress-condition
                                         (lambda (condition)
                                           (bt:interrupt-thread
                                            main (lambda () (signal condition))))))
            (with-delayed-error-reporting (:debug? debug?)

              (let+ ((jenkins.api:*base-url*       (option-value "jenkins" "base-uri"))
                     (jenkins.api:*username*       (option-value "jenkins" "username"))
                     (jenkins.api:*password*       (or (option-value "jenkins" "password")
                                                       (option-value "jenkins" "api-token")))
                     (delete-other?                (option-value "generation" "delete-other"))
                     (delete-other-pattern         (option-value "generation" "delete-other-pattern"))
                     (template-directory           (option-value "generation" "template-directory"))
                     (template                     (option-value "generation" "template"))
                     ((&values mode mode?)         (option-value "generation" "mode"))
                     (distribution                 (option-value "generation" "distribution"))
                     (template-pattern             (cond
                                                     ((and template mode (not (eq mode? :default)))
                                                      (error "~@<The options template and mode are mutually exclusive.~@:>"))
                                                     (template)
                                                     (mode
                                                      (let ((template-directory (or template-directory
                                                                                    (make-pathname
                                                                                     :name     nil
                                                                                     :type     nil
                                                                                     :defaults (merge-pathnames
                                                                                                #P"../templates/"
                                                                                                (first distribution))))))
                                                        (list (merge-pathnames
                                                               (make-pathname
                                                                :name      :wild
                                                                :type      "template"
                                                                :directory `(:relative ,mode))
                                                               template-directory))))
                                                     (t
                                                      (error "~@<At least one of the options template and mode must be supplied.~@:>"))))

                     (templates     (with-phase-error-check
                                        (:locate/template #'errors #'(setf errors) #'report)
                                      (sort (locate-specifications :template template-pattern)
                                            #'string< :key #'pathname-name)))
                     (distributions (with-phase-error-check
                                        (:locate/distribution #'errors #'(setf errors) #'report)
                                      (locate-specifications :distribution distribution)))
                     (overwrites    (mapcar #'parse-overwrite (option-value "generation" "set"))))
                (setf *traced-variables* (mapcar (compose #'make-keyword #'string-upcase)
                                                 (option-value "general" "trace-variable")))
                (setf lparallel:*kernel* (lparallel:make-kernel num-processes))

                (with-trivial-progress (:jobs)

                  (let* ((repository        (make-instance 'rs.m.d::base-repository))
                         (resolver          (make-instance 'rs.f:search-path-resolver
                                                           :search-path (list (make-pathname :name     nil
                                                                                             :type     nil
                                                                                             :defaults (first distributions))
                                                                              (merge-pathnames
                                                                               (make-pathname :name      nil
                                                                                              :type      nil
                                                                                              :directory '(:relative :back "projects"))
                                                                               (first distributions)))))
                         (locations         (make-instance 'rs.f:location-repository))
                         (builder           (service-provider:make-provider 'rs.m.d::builder
                                                                            :model
                                                                            :repository repository
                                                                            :resolver   resolver
                                                                            :locations  locations))

                         (templates         (with-phase-error-check
                                                (:load/template #'errors #'(setf errors) #'report
                                                 :continuable? nil)
                                              (load-templates templates)))
                         (distributions/raw (with-phase-error-check
                                                (:load/distribution #'errors #'(setf errors) #'report)
                                              (rs.f:process :distribution-recipe distributions builder)
                                              #+later overwrites))
                         #+no (projects          (with-phase-error-check
                                                (:locate/project #'errors #'(setf errors) #'report)
                                              (locate-projects distributions distributions/raw)))
                         #+no (projects/raw      (with-phase-error-check
                                                (:load/project #'errors #'(setf errors) #'report)
                                              (load-projects/versioned projects)))
                         #+no (projects/specs    (with-phase-error-check
                                                (:analyze/project #'errors #'(setf errors) #'report)
                                              (apply #'analyze-projects projects/raw
                                                     :non-interactive non-interactive
                                                     (append
                                                      (when cache-directory
                                                        (list :cache-directory cache-directory))
                                                      (when temp-directory
                                                        (list :temp-directory temp-directory))))))
                         (distributions     (with-phase-error-check
                                                (:resolve/distribution #'errors #'(setf errors) #'report)
                                              (mapcar (curry #'project-automation.model.project.stage2::transform-distribution
                                                             builder)
                                                      distributions/raw)))
                         #+no (distributions     (with-phase-error-check
                                                (:check-platform-requirements #'errors #'(setf errors) #'report)
                                              (check-platform-requirements distributions)))
                         #+no (distributions     (with-phase-error-check
                                                (:check-access #'errors #'(setf errors) #'report
                                                 :continuable? nil)
                                                   (check-distribution-access distributions)))

                         (foo (labels ((project-version (project)
                                         (with-simple-restart (continue "Skip project version ~A" project)
                                           (let+ (((&values url kind (&optional revision-kind revision-designator))
                                                   (project-automation.model.project.stage2::source-information project))
                                                  ((&values kind url branches)
                                                   (project-automation.access:probe-source url kind)) ; TODO should not be necessary when probe-sources step has been performed
                                                  (directory (project-automation.access:access-source
                                                              url kind revision-kind revision-designator)))
                                             (list (rs.f:process :guess directory builder)))))
                                       (dist-version (distribution)
                                         (append (mapcan #'dist-version (rs.m.d:contents distribution :include))
                                                 (mapcan #'project-version (rs.m.d:contents distribution :project))))
                                       (dist (distribution)
                                         (with-simple-restart (continue "Skip distribution ~A" distribution)
                                           (mapcan #'dist-version (rs.m.d:contents distribution :version)))))
                                (with-phase-error-check
                                    (:analyze/project #'errors #'(setf errors) #'report)
                                  (mapcan #'dist distributions))))

                         (projects          (with-phase-error-check
                                                (:instantiate/project #'errors #'(setf errors) #'report)
                                              (mapcar (curry #'project-automation.model.project.stage3::transform-distribution
                                                             builder)
                                                      distributions)))
                         #+no (jobs/spec         (unless dry-run?
                                              (with-phase-error-check
                                                  (:deploy/project #'errors #'(setf errors) #'report)
                                                (flatten (deploy-projects projects)))))
                         #+no (jobs              (unless dry-run?
                                              (mappend #'implementations jobs/spec))))
                    (declare (ignore templates))

                    (utilities.print-tree:print-tree
                     *standard-output* (first distributions)
                     (utilities.print-tree:make-node-printer
                      (lambda (stream depth object)
                        (declare (ignore depth))
                        (princ object stream)
                        '() #+no (project-automation.model.variable:direct-variables object))
                      #'project-automation.commands::print-node
                      (lambda (object)
                        (rs.m.d:contents object t))))

                    (terpri) (terpri)

                    (mapcar (lambda (x)
                             (utilities.print-tree:print-tree
                              *standard-output* x
                              (utilities.print-tree:make-node-printer
                               (lambda (stream depth object)
                                 (declare (ignore depth))
                                 (princ object stream)
                                 '() #+no (project-automation.model.variable:direct-variables object))
                               #'project-automation.commands::print-node
                               (lambda (object)
                                 (rs.m.d:contents object t))))
                             (terpri))
                            foo)

                    (terpri)

                    (utilities.print-tree:print-tree
                     *standard-output* (first projects)
                     (utilities.print-tree:make-node-printer
                      (lambda (stream depth object)
                        (declare (ignore depth))
                        (princ object stream)
                        '() #+no (project-automation.model.variable:direct-variables object))
                      #'project-automation.commands::print-node
                      (lambda (object)
                        (rs.m.d:contents object t))))

                    #+no (unless dry-run?
                           ;; Delete automatically generated jobs
                           ;; found on the server for which no
                           ;; counterpart exists among the newly
                           ;; generated jobs. This is necessary to get
                           ;; rid of leftover jobs when projects (or
                           ;; project versions) are deleted or
                           ;; renamed.
                           (when delete-other?
                             (with-phase-error-check
                                 (:delete-other-jobs #'errors #'(setf errors) #'report)
                               (let* ((other-jobs (set-difference
                                                   (jenkins.api:all-jobs delete-other-pattern)
                                                   all-jobs
                                                   :key #'jenkins.api:id :test #'string=))
                                      (generated-jobs (remove-if-not #'generated? other-jobs)))
                                 (with-sequence-progress (:delete-other generated-jobs)
                                   (mapc (progressing #'jenkins.api:delete-job :delete-other)
                                         generated-jobs)))))

                           (with-phase-error-check
                               (:list-credentials #'errors #'(setf errors) #'report)
                             (list-credentials jobs)))

                    #+no (when report-directory
                      (with-phase-error-check
                          (:report #'errors #'(setf errors) #'report)
                        (flet ((maybe-first (thing)
                                 (if (consp thing) (first thing) thing)))
                          (jenkins.report:report
                           (maybe-first distributions) :json report-directory)
                          (with-simple-restart (continue "Skip graph report")
                            (if (setf cl-dot:*dot-path* (cl-dot::find-dot))
                                (jenkins.report:report
                                 (maybe-first distributions) :graph report-directory)
                                (error "~@<Could not find dot program.~@:>"))))))))))))

      (abort (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Abort execution.~@:>"))
        (when condition
          (report-error *error-output* condition))
        (uiop:quit 2)))
    (uiop:quit (if non-dependency-errors? 1 0))))

(eval-when (:load-toplevel)
  (check-variable-liveness))
