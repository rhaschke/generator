;;;; execution.lisp --- Execution of planned tasks.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.build)

;;; Debug

(defvar *output-mutex* (bt:make-lock "Output lock"))

(defun out (format-control &rest format-arguments)
  #+no (bt:with-lock-held (*output-mutex*)
    (apply #'format t format-control format-arguments)
    (fresh-line)))

;;; Execution

(defclass execution-state ()
  ((%pending-tasks   :initarg  :pending-tasks
                     :type     list
                     :accessor pending-tasks)
   (%completed-tasks :reader   completed-tasks
                     :initform (make-hash-table :test #'eq))))

(defmethod runnable? ((task t) (state execution-state))
  (let ((completed (completed-tasks state)))
    (every (lambda (dependency)
             (let+ (((&values status status?) (gethash dependency completed)))
               (eq status t)))
           (dependencies task))))

(defmethod cancelable? ((task t) (state execution-state))
  (let ((completed (completed-tasks state)))
    (some (lambda (dependency)
            (let+ (((&values status status?) (gethash dependency completed)))
              (and status? (not (eq status t)))))
          (dependencies task))))

(defmethod done! ((task t) (status t) (state execution-state))
  (setf (gethash task (completed-tasks state)) status))

(defmethod pop-actionable-tasks ((state execution-state))
  (loop :for task :in (pending-tasks state)
        :if (runnable? task state)
        :collect (list task :start) :into actionable
        :else :if (cancelable? task state)
        :collect (list task :cancel) :into actionable
        :else
        :collect task :into remaining
        :finally (out "Remaining tasks: ~A" remaining)
                 (setf (pending-tasks state) remaining)
                 (return actionable)))

(defmethod execute* ((state execution-state))
  (more-conditions:with-trivial-progress (:execute-steps)
    (let* ((task-count      (length (pending-tasks state)))
           (remaining-count task-count)
           (results         (lparallel:make-channel)))
      (flet ((submit-runnable-tasks ()
               (loop :for (task action) :in (pop-actionable-tasks state)
                     :do (decf remaining-count)
                         (more-conditions:progress
                          :execute-steps (- 1 (/ remaining-count task-count))
                          "~A ~A" action (name task))
                         (let ((task task)) ; rebind since LOOP may mutate the binding
                           (ecase action
                             (:start
                              (lparallel:submit-task
                               results (lambda ()
                                         (list task (execute task state)))))
                             (:cancel
                              (lparallel:submit-task
                               results (constantly (list task nil)))))))))
        (submit-runnable-tasks)
        (loop :for result = (lparallel:receive-result results)
              :until (eq result :done)
              :do (destructuring-bind (task status) result
                    (done! task status state))
                  (if (null (pending-tasks state))
                      (lparallel:submit-task results (constantly :done))
                      (submit-runnable-tasks)))))))

(defmethod execute ((task step) (state execution-state))
  (let ((start-time (get-internal-real-time)))
    (out "Starting  ~A" task)
    (let ((result
            (with-simple-restart (continue "~@<Skip execution of task ~A~:@>" task)
              (let ((directory (directory task)))
                (out "~A in ~A" (name task) directory)
                (build-generator.analysis::run
                 (list "sh" "-c" (deploy:command task)) directory))
              t)))
      (out "Done with ~A -> ~A in ~,2F s"
           task
           result
           (/ (- (get-internal-real-time) start-time)
              internal-time-units-per-second))
      result)))
