;;;; completion.lisp --- Completion contributors for different recipe types.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; Structure completion for different document kinds

(defclass structure-completion-contributor ()
  ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    project-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ (((&accessors-r/o location path)  context)
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)
                                        :range (sloc:range location)))))
    (cond ((and (<= 1 (length path) 2) (ends-with :versions path))
           (map 'list #'make-item '(:name :pattern :variables)))
          ((length= 1 path)
           (map 'list #'make-item '(:minimum-generator-version
                                    :templates :catalog :variables :versions))))))

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    distribution-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ (((&accessors-r/o location path) context)
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)
                                        :range (sloc:range location)))))
    (cond ((length= 1 path)
           (map 'list #'make-item '(:minimum-generator-version
                                    :catalog :variables :include :versions)))
          ((and (length= 2 path) (ends-with :versions path))
           (map 'list #'make-item '(:name :versions :variables))))))

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    template-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ (((&accessors-r/o location path) context)
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)
                                        :range (sloc:range location)))))
    (cond ((length= 1 path)
           (map 'list #'make-item '(:inherit :variables :aspects :jobs)))
          ((and (length= 2 path) (ends-with :aspects path))
           (map 'list #'make-item '(:name :aspect :conditions :variables)))
          ((and (length= 2 path) (ends-with :jobs path))
           (map 'list #'make-item '(:minimum-generator-version
                                    :name :conditions :variables))))))

;;; Template name contributor

(defclass template-name-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     template-name-context)
     (contributor template-name-completion-contributor))
  (let+ (((&accessors-r/o location word) context)
         (templates (templates workspace))
         ((&flet make-item (template)
            (proto:make-completion-item (model:name template)
                                        :range (sloc:range location)))))
    (when (lparallel:fulfilledp templates)
      (loop :for template :in (lparallel:force templates)
            :when (starts-with-subseq word (model:name template ))
            :collect (make-item template)))))

;;; Variable name completion

(defclass variable-name-completion-contributor ()
  ())

(labels ((make-new-text (kind variable-name)
           (case kind
             ((nil)                       variable-name)
             (:scalar (format nil "${~A}" variable-name))
             (:list   (format nil "@{~A}" variable-name))))
         (make-completions (context kind) ; make-known-variable-items
           (flet ((make-item (variable)
                    (let* ((name          (var:variable-info-name variable))
                           (type          (var:variable-info-type variable))
                           (documentation (var:variable-info-documentation variable))
                           (title         (string-downcase name)))
                      (proto:make-completion-item
                       title
                       :kind          :variable
                       :detail        (format nil "Type: ~A " type)
                       :documentation documentation
                       :range         (prefix-range context)
                       :new-text      (make-new-text kind title)))))
             (loop :for variable :in (var:all-variables)
                      #+todo :when #+todo (starts-with-subseq
                                           prefix (string-downcase (var:variable-info-name variable)))
                   :collect (make-item variable))))
         (make-local-variable-items (context object)
           (map 'list (lambda+ ((name . &ign))
                        (let ((title (string-downcase name)))
                          (proto:make-completion-item
                           title
                           :kind   :variable
                                        ; :detail
                           :range    (prefix-range context)
                           :new-text (make-new-text (kind context) title))))
                (var:variables object)))
         (make-next-value-item (context default?)
           (let* ((kind          (kind context))
                  (documentation "The next value of this variable")
                  (title         (format nil "next-value~@[~A~]"
                                         (cond ((not default?)    nil)
                                               ((eq kind :scalar) "|")
                                               ((eq kind :list)   "|[]"))))
                  (detail        (format nil "~:[without~;with~] default" default?)))
             (proto:make-completion-item
              title
              :kind          :variable
              :detail        detail
              :documentation documentation
              :range         (prefix-range context)
              :new-text      (make-new-text kind title)))))

  (defmethod contrib:completion-contributions
      ((workspace   t)
       (document    t)
       (context     variable-name-context)
       (contributor variable-name-completion-contributor))
    (make-completions context nil))

  (defmethod contrib:completion-contributions
      ((workspace   t)
       (document    t)
       (context     variable-reference-context)
       (contributor variable-name-completion-contributor))
    (let ((kind (kind context)))
      (list* (make-next-value-item context nil)
             (make-next-value-item context t)
             (nconc (when-let ((object (object document)))
                      (make-local-variable-items context object))
                    (make-completions context kind))))))

;;; Variable value completion

(defclass variable-value-completion-contributor ()
  ())

(defmethod possible-values ((type t))
  '())

(defmethod possible-values ((type (eql 'boolean)))
  (list "false" "true"))

(defmethod possible-values ((type cons))
  (possible-values-using-head type (first type)))

(defmethod possible-values-using-head ((type cons) (head t))
  '())

(defmethod possible-values-using-head ((type cons) (head (eql 'or)))
  (mappend #'possible-values (rest type)))

(defmethod possible-values-using-head ((type cons) (head (eql 'eql)))
  (list (format nil "~(~A~)" (second type))))

(defun complete-person (context workspace)
  (when-let ((persons (elements (%persons workspace)))) ; TODO
    (mappend (lambda (person)
               (when (search (word context) (rosetta.model:name person ))
                 (list (proto:make-completion-item
                        (format nil "~A <~A>"
                                (rosetta.model:name person )
                                (first (rosetta-project.model.resource:identities person)))
                        :kind   :constant
                        :detail "Person"
                        :range  (sloc:range (location context))))))
             persons)))

(defun remote-refs (project kind)
  (when-let ((repository (ignore-errors (var:value/cast project :repository nil)))) ; TODO
    (mappend (lambda (line)
               (let+ (((&values match? groups)
                       (ppcre:scan-to-strings
                        (ecase kind
                          (:branch "refs/heads/(.*)$")
                          (:tag    "refs/tags/(.*)$"))
                        line)))
                 (when match?
                   (list (proto:make-completion-item
                          (aref groups 0)
                          :kind   :constant
                          :detail (format nil "Remote ~(~A~)" kind))))))
             (inferior-shell:run/lines `("git" "ls-remote" ,repository)))))

(defmethod contrib:completion-contributions
    ((workspace    t)
     (document     t)
     (context      known-variable-value-context)
     (contriubutor variable-value-completion-contributor))
  (let ((variable (variable-node context)))
    (cond ((member (var:variable-info-name variable) '(:branches :branch))
           (remote-refs (object document) :branch))
          ((member (var:variable-info-name variable) '(:tags :tag))
           (remote-refs (object document) :tag))
          ((eq (var:variable-info-name variable) :recipe.maintainer)
           (complete-person context workspace))
          (t
           (map 'list (lambda (value)
                        (proto:make-completion-item value
                                                    :kind  :constant
                                                    :range (sloc:range (location context))))
                (possible-values (var:variable-info-type variable)))))))

;;; Project name and version completion

(defclass project-name-completion-contributor ()
  ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-name-completion-contributor))
  (when-let ((prefix   (prefix context))
             (projects (projects workspace)))
    (mapcan (lambda (project)
              (let ((name (model:name project)))
                (when (starts-with-subseq prefix name)
                  (list (proto:make-completion-item
                         name
                         :kind          :file
                         :detail        "project"
                         :documentation (proto:make-markup-content (describe-project project) :markdown) ; TODO directly describe as markup-content
                         :range         (sloc:range (location context)))))))
            projects)))

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     project-version-context)
     (contributor project-name-completion-contributor))
  (when-let* ((project-name (project-name context))
              (prefix       (prefix context))
              (project      (find-project project-name workspace)))
    (let+ ((result '())
           ((&flet consider (name detail &key documentation)
              (when (starts-with-subseq prefix name)
                (push (apply #'proto:make-completion-item
                             name
                             :kind   :file
                             :detail detail
                             :range  (sloc:range (location context))
                             (when documentation
                               `(:documentation ,(funcall documentation))))
                      result)))))

      (ignore-errors
       (map nil (rcurry #'consider "branch version")
            (var:value/cast project :branches)))
      (map nil (lambda (version)
                 (consider
                  (model:name version)
                  "project version"
                  :documentation
                  (lambda ()
                    ;; TODO directly describe as markup-content))))
                    (proto:make-markup-content (describe-project project) :markdown))))
           (project:versions project))
      result)))

;;; Aspect class completion

(defclass aspect-class-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     aspect-class-context)
     (contributor aspect-class-completion-contributor))
  (protocol.language-server::debug1 context)
  (let ((prefix (prefix context)))
    (mappend (lambda (provider)
               (let ((name (string-downcase (service-provider:provider-name provider))))
                 (when (starts-with-subseq prefix name)
                   (list (proto:make-completion-item
                          name
                          :kind   :class
                          :detail "aspect"
                          :range  (sloc:range (location context)))))))
             (service-provider:service-providers 'build-generator.model.aspects::aspect))))

;;; System package name completion

(defclass system-package-name-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     system-package-name-context)
     (contributor system-package-name-completion-contributor))
  (let ((prefix (word context))
        (range  (sloc:range (location context))))
   (loop :for package :in (lparallel:force (ensure-platform-packages workspace))
         :for name = (first package)
         :when (starts-with-subseq prefix name)
           :collect (proto:make-completion-item name
                                                :kind   :module
                                                :detail "package"
                                                :range  range))))

;;;

#+no (defclass document ()
  ((index :initarg :index
          :reader  index)))

#+no (let* ((source   (text.source-location:make-source "<string>"))
            (index    (text.source-location.lookup:make-index))
            (document (make-instance 'document :index index)))
       (values (language.yaml:load "variables:
  foo: bar"
                                   :builder (project::make-builder
                                             source index))
               index
               (lookup:lookup (text.source-location:make-location source 13 13) index)
               (contrib::context-contributions
                t
                document
                (text.source-location:make-location source 13 13)
                (make-instance 'variable-name-context-contributor))))
