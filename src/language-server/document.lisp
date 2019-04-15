;;;; document.lisp --- Document classes for different recipe types.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

(defclass build-generator-document (lsp:document
                                    contrib:diagnostics-contributors-mixin
                                    contrib:context-contributors-mixin
                                    contrib:completion-contributors-mixin
                                    contrib:hover-contributors-mixin
                                    contrib:definition-contributors-mixin)
  ((lsp::%workspace :initarg  :workspace
               :reader   workspace)
   (%object    :accessor object
               :initform nil)
   (%locations :accessor locations
               :initform nil)
   (%source    :accessor source
               :initform nil)
   (%index     :accessor index
               :initform nil)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib:diagnostics)))
  (list (make-instance 'tabs-diagnostics-contributor)
        (make-instance 'variable-diagnostics-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib:context)))
  (list (make-instance 'structure-context-contributor)
        (make-instance 'variable-name-context-contributor)
        (make-instance 'variable-value-context-contributor)
        (make-instance 'system-package-name-context-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib:hover)))
  (list (make-instance 'variable-hover-contributor)
        (make-instance 'effective-value-hover-contributor)
        (make-instance 'system-package-name-hover-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib:completion)))
  (list (make-instance 'structure-completion-contributor)
        (make-instance 'variable-name-completion-contributor)
        (make-instance 'variable-value-completion-contributor)
        (make-instance 'system-package-name-completion-contributor)))

(defmethod (setf lsp:text) :after ((new-value string)
                                   (document  build-generator-document))
  (unless (starts-with-subseq "file:///" *uri*)
    (error "unsupported URI: ~S" *uri*))

  (let* ((pathname (pathname (subseq *uri* (length "file://"))))
         (source   (setf (source document)
                         (sloc:make-source pathname :content (lsp:text document))))

         (errors    '())
         (result    nil)
         (index     (make-instance 'lookup::range-index))

         (project::*locations* (make-instance 'project::locations
                                              :hook (lambda (object location)
                                                      (declare (ignore object))
                                                      ;; TODO the sources should be eq
                                                      (when (equalp (sloc:name (sloc:source location))
                                                                    (sloc:name source))
                                                        (lookup:add! location index))))))
    (handler-bind (((and error build-generator.util:continuable-error)
                     (lambda (condition)
                       (log:warn "~@<Error during parsing:~:@_~A~@:>" condition)
                       (push condition errors)
                       (when-let ((restart (build-generator.util:find-continue-restart condition))) ; TODO doesn't work with simple-object-error since it doesn't have a cause
                         (invoke-restart restart))
                       (log:error "~@<Unrecoverable error during parsing:~:@_~A~@:>" condition))))
      (with-simple-restart (continue "Abort parse")

        (setf result (parse document (lsp:text document) pathname))))
    (setf (object document)    result
          (locations document) project::*locations*
          (index document)     index)
    (let ((diagnostics (contrib:diagnostics (workspace document) document))
          (messages    '()))
      (flet ((do-condition (condition)
               (if (compute-applicable-methods #'project::annotations (list condition))
                   (push (make-instance 'proto:diagnostic
                                        :annotation (first (project::annotations condition))
                                        :message    condition)
                         diagnostics)
                   (push (proto:make-message :error (princ-to-string condition))
                         messages))))
        (map nil #'do-condition errors)
        (methods:publish-diagnostics document diagnostics)
        (map nil #'methods::log-message messages)))))

(defmethod contrib:contexts :around ((workspace t) ; HACK))))
                                     (document  build-generator-document)
                                     (position  t))
  (reinitialize-instance position :column (max 0 (1- (sloc:column position))))
  (call-next-method))

(defmethod methods:highlight-in-document ((workspace t)
                                          (document  build-generator-document)
                                          (version   t)
                                          (position  t))
  (or (when-let ((locations (lookup:lookup position (index document))))
        (vector (proto:make-highlight :text (sloc:range (first locations)))))
      #()))

(defmethod methods:code-actions ((workspace t)
                                 (document  build-generator-document)
                                 (range     t)
                                 (context   t))
  nil)

(defmethod methods:signature-help ((workspace t)
                                   (document  t)
                                   (position  t))
  nil)

;;; Document classes

;;; TODO use this for build-generator-document as well
(defmacro define-document-class (name slots &body aspect-contributors)
  (let+ (((&flet+ make-aspect-method ((aspect &rest contributors))
            (let+ (((&flet make-contributor-form (contributor)
                      `(make-instance ',contributor))))
              `(defmethod contrib:make-contributors ((document ,name)
                                                     (aspect   (eql ',aspect)))
                 (list* ,@(map 'list #'make-contributor-form contributors)
                        (when (next-method-p)
                          (call-next-method)))))) ))
    `(progn
       (defclass ,name (build-generator-document)
         ,slots)
       ,@(map 'list #'make-aspect-method aspect-contributors))))

;;; `project-document'

(define-document-class project-document
  ()
  (contrib:context    template-name-context-contributor)
  (contrib:completion template-name-completion-contributor)
  (contrib:definition template-definition-contributor))

(defmethod parse ((document project-document) (text string) (pathname t))
  (let* ((workspace            (workspace document))
         (project::*templates* (lparallel:force (ensure-templates workspace))))
    (project::load-project-spec/yaml
     text
     :pathname          pathname
     :repository        (repository workspace)
     :generator-version "0.26.0"
     :version-test      (lambda (name pattern)
                          (cond (name
                                 t)
                                (pattern
                                 (remove pattern '("foo")
                                         :test-not #'ppcre:scan)))))))

;;; `distribution-document'

(define-document-class distribution-document
  ()
  (contrib:context    project-version-reference-context-contributor)
  (contrib:hover      project-version-hover-contributor)
  (contrib:completion project-name-completion-contributor)
  (contrib:definition project-definition-contributor))

(defmethod parse ((document distribution-document) (text string) (pathname t))
  (let* ((workspace    (workspace document))
         (repository   (repository workspace))
         (name         (pathname-name pathname)) ; TODO project:recipe-name
         (project::*distributions* (make-hash-table :test #'equal))
         (distribution (project::loading-recipe (project::*distribution-load-stack* name)
                         (project::load-one-distribution/yaml
                          text
                          :pathname          pathname
                          :repository        (repository workspace)
                          :generator-version "0.26.0")))
         (projects-files+versions (uiop:symbol-call '#:build-generator.commands '#:locate-projects
                                                    (list distribution) repository))
         (project::*templates*        (lparallel:force (ensure-templates (workspace document))))
         (project::*projects*         (make-hash-table :test #'equal))
         (project::*locations*        (make-instance 'project::locations
                                                                   :hook nil))
         (projects (uiop:symbol-call '#:build-generator.commands '#:load-projects/versioned
                                     projects-files+versions repository
                                     :generator-version "0.26.0" ; generator-version
                                     )))
    distribution))

;;; `template-document'

(define-document-class template-document
  ()
  (contrib:context    template-name-context-contributor
                      aspect-class-context-contributor)
  (contrib:completion template-name-completion-contributor
                      aspect-class-completion-contributor)
  (contrib:definition template-definition-contributor))

(defmethod parse ((document template-document) (text string) (pathname t))
  (let ((workspace (workspace document))
        (name      (pathname-name pathname))
        (project::*templates* (make-hash-table :test #'equal)))
    (project::loading-recipe (project::*template-load-stack* name)
      (project::load-one-template/yaml
       text
       :pathname          pathname
       :repository        (repository workspace)
       :generator-version "0.26.0"))))
