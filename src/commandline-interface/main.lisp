;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(defun make-error-policy (policy &key debug? fail)
  (let+ (((&flet flame (condition &key (debug? debug?))
            (format *error-output* "~@<~A~@:>~2%" condition)
            (when debug?
              #+sbcl (sb-debug:print-backtrace))))
         ;; Specific actions.
         ((&flet do-continue (condition)
            (when-let ((restart (find-restart 'jenkins.project.commands::defer condition))) ; TODO should just call defer
              (invoke-restart restart condition :debug? debug?))
            (flame condition :debug? debug?)
            (when (typep condition 'jenkins.util:continuable-error)
              (when-let ((restart (jenkins.util:find-continue-restart condition)))
                (invoke-restart restart)))))
         ((&flet do-fail (condition)
            (when fail
              (funcall fail condition))
            (do-continue condition)))
         ((&flet do-abort (condition)
            (flame condition :debug? debug?)
            (when-let ((restart (find-restart 'abort condition)))
              (invoke-restart restart)))))
    (lambda (condition)
      (when (typep condition 'jenkins.project.commands::deferred-phase-error)
        (format t "~A~2%" condition)
        (continue))
      (log:info "Handling ~A: ~A" (type-of condition) condition)
      (loop :for (condition-type . action) :in policy
            :do (log:debug "Considering rule: ~S → ~S" condition-type action)
            :when (typep condition condition-type)
              :do (log:info "Applying rule: ~S → ~S" condition-type action)
                  (ecase action
                    (:continue (do-continue condition))
                    (:fail     (do-fail     condition))
                    (:abort    (do-abort    condition)))
                  (return)))))

(defun main ()
  (log:config :thread :warn)
  (choose-default-progress-style)

  (let+ ((arguments (uiop:command-line-arguments))
         ((&flet execute-command-and-quit (code command &rest args)
            (jenkins.project.commands:command-execute
             (apply #'jenkins.project.commands:make-command
                    command args))
            (uiop:quit code)))
         (debugging? nil)
         ((&flet die (condition &optional usage? context)
            (format *error-output* "~@<~A~@:>~2%" condition)
            (when debugging?
              #+sbcl (sb-debug:print-backtrace))
            (if usage?
                (apply #'execute-command-and-quit
                       1 :help :brief? t
                       (when (and context (not (equal context "global")))
                         (list :command context)))
                (uiop:quit 1))))
         ((&values option-value &ign configuration &ign
                   (&plist-r/o
                    (version? :version?) (help? :help?) (debug? :debug?)))
          (handler-bind (((and error jenkins.project.commandline-options:option-context-condition)
                          (lambda (condition)
                            (die condition t (jenkins.project.commandline-options:context condition))))
                         (error (rcurry #'die t "global")))
            (process-configuration-and-commandline-arguments arguments))) ; TODO this calls configure-command but reported conditions are not right. e.g. report -D foo.distribution produces "The "-D" option requires a VARIABLE-NAME=VALUE argument." and the generic help. does not mention the command and does not print the command-specific help
         ((&flet option-value (&rest args)
            (apply option-value args))))
    (cond (version? (execute-command-and-quit 0 :version))
          (help?    (execute-command-and-quit 0 :help))
          (debug?   (setf debugging? t)))
    (handler-bind
        ((jenkins.project.commands:command-not-found-error
          (rcurry #'die t "global"))
         (jenkins.project.commands:command-configuration-problem
          (lambda (condition)
            (die condition t (jenkins.project.commands:command condition))))
         (error #'die)
         #+sbcl (sb-sys:interactive-interrupt #'die))
      (let* ((configuration (configuration.options:sub-configuration
                             "commands.**" configuration))
             (command       (jenkins.project.commands:make-command
                             configuration))
             (fail?         nil))
        (jenkins.project.commands:execute-command
         command
         :num-processes   (option-value "global" "num-processes")
         :error-policy    (make-error-policy
                           (option-value "global" "on-error")
                           :debug? debugging?
                           :fail   (lambda (condition)
                                     (declare (ignore condition))
                                     (setf fail? t)))
         :progress-style  (option-value "global" "progress-style")
         :cache-directory (option-value "global" "cache-directory")
         :temp-directory  (option-value "global" "temp-directory")
         :trace-variables (option-value "global" "trace-variable"))
        (uiop:quit (if fail? 1 0))))))

(eval-when (:load-toplevel)
  (check-variable-liveness))
