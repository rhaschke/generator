;;;; target.lisp --- Target definition for generating a Dockerfiles.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

(defclass dockerfile-target ()
  (;; Configuration
   (output-directory :initarg  :output-directory ; TODO mixin to share with makefile-target
                     :type     pathname
                     :reader   output-directory
                     :documentation
                     #.(format nil "The directory into which the ~
                        Dockerfile and the associated scripts should ~
                        be written."))
   (base-image       :initarg  :base-image
                     :type     string
                     :reader   base-image
                     :documentation
                     #.(format nil "The Docker image on which the ~
                        resulting image should be based."))
   (platform         :initarg  :platform
                     :type     (or null (cons string))
                     :reader   platform
                     :initform '("ubuntu" "xenial")
                     :documentation
                     #.(format nil "The target platform assumed in ~
                        platform dependency computations."))
   (run-strategy     :initarg  :run-strategy ; TODO call these "debug" and "release" instead?
                     :type     (member :one-file-for-all-builders
                                       :one-file-per-builder)
                     :reader   run-strategy
                     :initform :one-file-per-builder
                     :documentation
                     #.(format nil "Controls whether the commands for ~
                        one project are executed as a single RUN ~
                        command or multiple RUN COMMANDS.~@
                        ~@
                        A single RUN command is more efficient in ~
                        terms of image build time and of the resulting ~
                        storage efficiency of the resulting image.~@
                        ~@
                        Multiple RUN commands are better debugging and ~
                        interactive development since build steps can ~
                        be cached on a finer granularity.")))
  (:default-initargs
   :output-directory (more-conditions:missing-required-initarg 'dockerfile-target :output-directory)
   :base-image       (more-conditions:missing-required-initarg 'dockerfile-target :base-image))
  (:documentation
   "Generate a Dockerfile that builds projects into an image."))

(service-provider:register-provider/class
 'deploy:target :dockerfile :class 'dockerfile-target)

(defun write-header-comment (stream target)
  (declare (ignore target))
  (deploy:print-heading stream "TODO steal from distribution release command"))

(defun write-package-installation-commands (stream distribution target)
  (let* ((platform     (platform target))
         (requirements (project:platform-requires distribution platform))
         (apt-command  "DEBIAN_FRONTEND=noninteractive apt-get -qq"))
    (deploy:print-heading stream (format nil "Dependencies for platform ~{~A~^ ~}"
                                             platform))
    (if requirements
        (format stream "RUN ~A update \\~@
                        ~4T&& ~:*~A --assume-yes upgrade \\~@
                        ~4T&& ~:*~A --assume-yes install \\~@
                          ~6@T~{~<~T\\~%~6@T~1,:;~A~>~^ ~}~
                          ~2%"
                apt-command requirements)
        (format stream "# no dependencies~2%"))))

(defun write-cleanup-commands (stream)
  (deploy:print-heading stream "Cleanup")
  #+later (format stream "RUN ~%"))

(defmethod deploy:deploy ((thing sequence) (target dockerfile-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let* ((deployed-things (call-next-method))
         (deployed-things (util:sort-with-partial-order
                           deployed-things (lambda (left right)
                                             (find (model:specification left)
                                                   (model:dependencies
                                                    (model:specification right))))))
         (run-strategy    (run-strategy target))
         (dockerfile      (merge-pathnames "Dockerfile" (output-directory target))))
    (ensure-directories-exist dockerfile)
    (with-output-to-file (stream dockerfile :if-exists :supersede)
      ;; Header comment and base image.
      (write-header-comment stream target)
      (format stream "FROM ~A~2%" (base-image target))

      ;; Install build and runtime dependencies.
      (write-package-installation-commands
       stream (first-elt thing) target)

      ;; Write scripts and RUN directives for prepare hook(s).
      (map nil (lambda (distribution) ; TODO could we get this into DEPLOYED-THINGS?
                 (with-simple-restart
                     (continue "~@<Skip writing RUN commands for ~A ~
                                prepare hook~@:>"
                               distribution)
                   (when-let* ((name         (model:name distribution))
                               (prepare-hook (var:value distribution :prepare-hook/unix nil)))
                     (deploy:print-heading stream (format nil "~A Prepare Hook" name))
                     (write-scripts-and-run-commands*
                      stream target "distribution-prepare"
                      `((,(format nil "~A-prepare-hook" name)
                         "Prepare Hook"
                         ,prepare-hook)))
                     (format stream "~2%"))))
           thing)

      ;; Write scripts and RUN directives for project builders.
      (map nil (lambda (thing)
                 (with-simple-restart
                     (continue "~@<Skip writing RUN commands for ~A~@:>" thing)
                   (deploy:print-heading stream (model:name thing))
                   (write-scripts-and-run-commands
                    target stream thing run-strategy)
                   (format stream "~2%")))
           deployed-things)

      ;; TODO Optionally delete workspaces
      ;; TODO Uninstall build dependencies
      (write-cleanup-commands stream))))

(defclass dockerfile-job (model:named-mixin
                          model:implementation-mixin
                          aspects::aspect-builder-defining-mixin)
  ((%builders :accessor builders
              :initform '())))

(defclass shell-command (deploy:command-mixin
                         print-items:print-items-mixin)
  ((%aspect  :initarg :aspect
             :reader  aspect)))

(defun shell-command (aspect format-control &rest format-arguments)
  (let ((command (if format-arguments
                     (apply #'format nil format-control format-arguments)
                     format-control)))
    (make-instance 'shell-command :aspect aspect :command command)))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   shell-command))
  (let* ((variable        :aspect.builder-constraints.shell)
         (constraints/raw (var:value aspect variable nil))
         (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
    (log:trace "~@<Constraints for ~A in ~A~:@_~
                ~/aspects::format-constraints/~@:>"
               step variable constraints)
    constraints))

(defun write-copy-and-run-commands (stream script-directory scripts)
  (format stream "COPY ~{~A~^ ~} /tmp/~A~@
                    ~@
                    ~{~{~
                      # ~A~@
                      RUN mkdir -p \"~A\" \\~@
                      ~4@T&& cd \"~:*~A\" \\~@
                      ~4@T&& sh \"/tmp/~A\"~@
                    ~}~^~2%~}"
          (map 'list #'third scripts) script-directory scripts))

(defun make-script-directory (sub-directory)
  (make-pathname :directory `(:relative "scripts" ,sub-directory)))

(defun make-script-name (name script-directory output-directory)
  (let* ((script/relative (make-pathname :name      (util:safe-name name)
                                         :type      "sh"
                                         :defaults  script-directory))
         (script/absolute (merge-pathnames script/relative output-directory)))
    (values script/relative script/absolute)))

(defmacro with-output-to-script ((stream-var name script-directory output-directory)
                                 &body body)
  (with-gensyms (relative-var absolute-var)
    `(let+ (((&values ,relative-var ,absolute-var)
             (make-script-name ,name ,script-directory ,output-directory)))
       (ensure-directories-exist ,absolute-var)
       (with-output-to-file (,stream-var ,absolute-var :if-exists :supersede)
         (format ,stream-var "set -e~2%")
         ,@body)
       (values ,relative-var ,absolute-var))))

(defun write-scripts-and-run-commands* (stream target sub-directory steps)
  (let ((output-directory (output-directory target))
        (script-directory (make-script-directory sub-directory))
        (runs             '()))
    (map nil (lambda+ ((name title command))
               (let ((script/relative
                       (with-output-to-script
                           (stream name script-directory output-directory)
                         (write-string command stream))))
                 (appendf runs (list (list title name script/relative)))))
         steps)

    (write-copy-and-run-commands stream script-directory runs)))

(defmethod write-scripts-and-run-commands ((target   t)
                                           (stream   t)
                                           (job      t)
                                           (strategy (eql :one-file-per-builder)))
  (let* ((output-directory  (output-directory target))
         (project-directory (deploy:job-full-name (model:specification job)))
         (script-directory  (make-script-directory project-directory))
         (runs              '()))
    (map nil (lambda (builder)
               (let* ((name    (model:name (aspect builder)))
                      (command (trim-command (deploy:command builder)))
                      (script/relative
                        (with-output-to-script
                            (stream name script-directory output-directory)
                          (write-string command stream))))
                 (appendf runs (list (list name
                                           project-directory
                                           script/relative)))))
         (builders job))

    (write-copy-and-run-commands stream script-directory runs)))

(defmethod write-scripts-and-run-commands ((target   t)
                                           (stream   t)
                                           (job      t)
                                           (strategy (eql :one-file-for-all-builders)))
  (let* ((output-directory  (output-directory target))
         (project-directory (deploy:job-full-name (model:specification job)))
         (script-directory  (make-script-directory project-directory))
         (script/relative
           (with-output-to-script (stream "builders" script-directory output-directory)
             (map nil (lambda (builder)
                        (let ((name    (model:name (aspect builder)))
                              (command (trim-command (deploy:command builder))))
                          (deploy:print-heading stream (format nil "Aspect ~A" name)) ; TODO should take format-control &rest format-arguments
                          ;; Execute COMMAND in a sub-shell so that
                          ;; e.g. changing the current directory or
                          ;; the environment does not affect the next
                          ;; step.
                          ;;
                          ;; Note that we must not indent the
                          ;; sub-shell command string as that could
                          ;; break HERE documents and maybe other
                          ;; things.
                          (format stream "(~@
                                           ~@<~@;~A~:>~@
                                          )~2%" command)))
                  (builders job)))))

    (write-copy-and-run-commands
     stream script-directory
     `(("Builders" ,project-directory ,script/relative)))))

(defmethod deploy:deploy ((thing project::job) (target dockerfile-target))
  (let ((output (make-instance 'dockerfile-job
                               :name          (deploy:job-full-name thing)
                               :specification thing)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :dockerfile)

    output))
