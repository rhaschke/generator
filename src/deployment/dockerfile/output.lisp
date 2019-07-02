;;;; output.lisp --- TODO.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

;;; Output for `dockerfile'

(defun write-header-comment (stream dockerfile)
  (declare (ignore dockerfile))
  (deploy:print-heading stream "TODO steal from distribution release command"))

(defmethod output ((object dockerfile) (target pathname))
  (let ((filename target #+no (filename object)))
    (ensure-directories-exist filename)
    (with-output-to-file (stream filename :if-exists :supersede)
      (output object stream))))

(defmethod output ((object dockerfile) (target stream))
  ;; Header comment and base image.
  (write-header-comment target object)

  (map nil (rcurry #'output target) (stages object)))

;;; Output for `stage'

(defmethod output ((object stage) (target stream))
  (deploy:print-heading target (format nil "Stage ~A" (model:name object)))
  (format target "FROM ~A~@[ as ~A~]~2%"
          (base-image object) (model:name object))

  ;; Install build and runtime dependencies.
  #+no (write-package-installation-commands
   target (first distributions) target)

  ;; Write scripts and RUN directives for prepare hook(s).
  (loop :for (name body) :in (scripts object)
        :do (with-simple-restart
                (continue "~@<Skip writing RUN commands for ~A ~
                            prepare hook~@:>"
                          name)
              (deploy:print-heading target (format nil "~A Prepare Hook" name))
              (write-scripts-and-run-commands*
               target target "distribution-prepare"
               `((,(format nil "~A-prepare-hook" name)
                  "Prepare Hook"
                  ,body)))
              (format target "~2%")))

  ;; Write scripts and RUN directives for project builders.
  (map nil (lambda (step)
             (with-simple-restart
                 (continue "~@<Skip writing commands for ~A~@:>" step)
               (output step target)
               (format target "~2%")))
       (steps object))

  ;; TODO Optionally delete workspaces
  ;; TODO Uninstall build dependencies
  #+later (write-cleanup-commands stream))

;;; `script'

(defmethod output ((object script) (target stream))
  )

;;; `copy'

(defmethod output ((object copy) (target* stream))
  (let+ (((&accessors-r/o from-stage source target) object))
    (deploy:print-heading target* (format nil "Copy results from stage ~A"
                                         (model:name from-stage)))
    (format target* "COPY --from=~A \"~A\" \"~A\"~%"
            (model:name from-stage) source target)))

;;; `dockerfile-job'

(defmethod output ((object dockerfile-job) (target stream))
  (deploy:print-heading target (model:name object))
  ; (write-scripts-and-run-commands target stream object run-strategy)
  )

;;;

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

(defmethod write-scripts-and-run-commands ((target   dockerfile-target)
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

(defmethod write-scripts-and-run-commands ((target   dockerfile-target)
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
