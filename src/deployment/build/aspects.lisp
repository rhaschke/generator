;;;; aspects.lisp --- Aspect extensions used in the deployment.build module.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.build)

(defun step-name (aspect)
  (format nil "~{~A~^.~}" (reverse (model:ancestor-names aspect))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   step))
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

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  output)

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  ;; Apply aspects, respecting declared ordering. Translate builder
  ;; ordering constraints into step dependencies.
  (let+ ((aspects::*step-constraints* '())
         (aspects (util:sort-with-partial-order
                   (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; call (add-step STEP OUTPUT).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (steps       (steps output)))
      (when steps
        (log:debug "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/build-generator.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build (hash-table-count constraints)
                   (hash-table-alist constraints))

        (map nil (lambda (step)
                   (setf (dependencies step)
                         (remove-if-not (rcurry #'aspects::step< step constraints)
                                        steps)))
             steps))))

  output)

;;;

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (step    (make-step (step-name aspect) command :early? t)))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::archive '((:before t)))
    (add-step step output))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (catch 'aspects::%bail
    (apply
     (lambda (url username password credentials branches local-branch
              clone-timeout wipe-out-workspace? clean-before-checkout?
              checkout-submodules? shallow? sub-directory)
       (let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (when sub-directory
                           (aspects:extend! aspect spec stream :sub-directory-command))))
              (step    (make-step (step-name aspect) command :early? t
                                  :builder-class 'aspects::git)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::git '())
         (add-step step output)))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-subversion)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (catch 'aspects::%bail
    (apply
     (lambda (url revision credentials local-dir checkout-strategy)
       (let* ((command (aspects:extend! aspect spec 'string :command))
              (step    (make-step (step-name aspect) command
                                  :early?        t
                                  :builder-class 'aspects::subversion)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::subversion '())
         (add-step step output)))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-shell)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (catch 'aspects::%bail
    (apply
     (lambda (command)
       (let ((step (make-step (step-name aspect) command
                              :builder-class 'aspects::shell)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::shell '())
         (add-step step output)))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-cmake/unix)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (catch 'aspects::%bail
    (let ((step (make-step (step-name aspect)
                           (aspects:extend! aspect spec 'string :command)
                           :builder-class 'aspects::cmake/unix)))
      (aspects::register-constraints aspect 'aspects::build step 'aspects::cmake/unix '())
      (add-step step output)))
  output)
