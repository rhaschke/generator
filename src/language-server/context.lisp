(cl:in-package #:jenkins.language-server)

;;; Utilities

(defun structure-path (position document)
  (when-let* ((locations (lookup:lookup position (index document))))
    #+NO (lsp::debug1 (list* position (sloc:index position) (subseq (lsp:text document) (- (sloc:index position) 2) (+ (sloc:index position) 2))
                        (loop :for location :in locations
                              :collect (cons location (gethash location (location->object document))))))
    (let ((path (mappend
                 (lambda (node)
                   (when (typep node '(cons keyword))
                     (list (car node))))
                 (map 'list (lambda (location)
                              (gethash location (location->object document)))
                      locations))))
      (values path locations))))

;;; `structure-context'

(defclass structure-context (print-items:print-items-mixin)
  ((%path :initarg :path
          :reader  path)))

(defmethod print-items:print-items append ((object structure-context))
  `((:path ,(reverse (path object)) "~{~A~^ » ~}")))

(defclass structure-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor structure-context-contributor))
  (when-let* ((path (structure-path position document)))
    (list (make-instance 'structure-context :path path))))

;;;

(defclass template-name-context () ())

(defclass template-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    project-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :templates path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context)))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    template-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :inherit path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context)))))

;;;

(defclass variable-name-context () ())

(defclass variable-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-name-context-contributor))
  (when-let* ((locations (lookup:lookup position (index document)))
              (leaf      (gethash (first locations) (location->object document)))
              (leaf      (when (stringp leaf)
                           leaf))
              (path      (structure-path position document))
              (position* (position :variables path)))
    (log:error locations leaf path position*)
    (cond ((and (= position* 1)
                (string-equal leaf (first path))) ;; TODO could be deeper within dictionary value
           (list (make-instance 'variable-name-context)))

          ((stringp leaf)
           (let ((relative (- (sloc:index position) (sloc:index (sloc:start (first locations))))))
             (log:error leaf (sloc:start (first locations)) relative)
             (when (search "${" leaf :end2 (1+ relative) :from-end t)
               (list (make-instance 'variable-name-context))))))))

;;;

(defclass variable-value-context ()
  ((%variable-location :initarg :variable-location
                       :reader  variable-location)
   (%variable-node     :initarg :variable-node
                       :reader  variable-node)))

(defclass variable-value-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-value-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document)))
    (when-let ((position (position :variables path)))
      (when (= position 1)
        (when-let* ((name     (first path))
                    (variable (jenkins.model.variables:find-variable name :if-does-not-exist nil)))
          (log:error path position name variable)
          (list (make-instance 'variable-value-context
                               :variable-location location ; TODO
                               :variable-node     variable)))))))

;;; project version reference context provider

(defclass project-name-context (print-items:print-items-mixin)
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defmethod print-items:print-items append ((object project-name-context))
  `((:prefix ,(prefix object) "~S")))

(defclass project-version-context () ())

(defclass project-version-reference-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (gethash location (location->object document))))
    (log:error path location thing)
    (when-let ((position (position :versions path)))
      (cond ((and (= position 0) (stringp thing) (not (find #\@ thing)))
             (list (make-instance 'project-name-context :prefix thing)))))))

;;; Aspect name context

(defclass aspect-class-context ()
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defclass aspect-class-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspcae   t)
     (document    t)
     (position    t)
     (contributor aspect-class-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (gethash location (location->object document))))
    (log:error path location thing)
    (when (ends-with-subseq '(:aspect :aspects) path)
      (list (make-instance 'aspect-class-context :prefix thing)))))
