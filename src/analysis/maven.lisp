;;;; maven.lisp --- Analysis of maven projects.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Constants

(define-constant +pom-namespace+
  "http://maven.apache.org/POM/4.0.0"
  :test #'string=)

;;;

(defmethod analyze ((directory pathname)
                    (kind      (eql :maven))
                    &key)
  (let* ((maven-file (merge-pathnames "pom.xml" directory))
         (document   (cxml:parse maven-file (stp:make-builder))))
    (xloc:with-locations-r/o
        (((:val id :type 'list/dependency)           "project")
         ((:val id/parent :type 'list/dependency)    "project/parent"
                                                     :if-no-match :do-nothing)
         (description                                "project/description/text()"
                                                     :if-no-match :do-nothing)
         (url                                        "project/url/text()"
                                                     :if-no-match :do-nothing)
         (license                                    "licenses/license/name/text()"
                                                     :if-no-match :do-nothing)
         ((:val properties :type 'cons/property)     "project/properties/*"
                                                     :if-multiple-matches :all)
         ((:val dependencies :type 'list/dependency) "project/dependencies/dependency"
                                                     :if-multiple-matches :all)
         (modules                                    "project/modules/module/text()"
                                                     :if-multiple-matches :all)
         :namespaces `((nil . ,+pom-namespace+)))
        document
      (let+ ((id           (if id/parent (merge-ids id id/parent) id))
             (name+version (id->name+version id))
             (license      (or license (analyze directory :license)))
             (sub-provides '())
             (sub-requires '())
             ((&flet process-sub-project (name)
                (let+ ((sub-directory (merge-pathnames (concatenate 'string name "/") directory))
                       ((&plist-r/o (provides :provides) (requires :requires))
                        (analyze sub-directory :maven)))
                  (appendf sub-provides provides)
                  (appendf sub-requires requires))))
             ((&flet+ process-dependency ((name version1))
                (list :maven name
                      (when version1
                        (parse-version
                         (%resolve-maven-version
                          version1 (acons "project.version" (fourth id)
                                          properties))))))))
        (mapc #'process-sub-project modules)
        (append
         (list :versions `((:main ,name+version)) ; TODO remove
               :provides `(,(process-dependency name+version)
                           ,@sub-provides)
               :requires `(,@(mapcar (compose #'process-dependency #'id->name+version)
                                     dependencies)
                           ,@sub-requires))
         (when description `(:description ,description))
         (when url         `(:url         ,url))
         (when license     `(:license     ,license))
         (when properties  `(:properties  ,properties)))))))

;;; Utility functions

(defun %resolve-maven-value (spec properties)
  (let+ (((&flet lookup (name)
            (cdr (find name properties :key #'car :test #'string=))))
         ((&labels replace1 (value &optional (depth 10))
            (when (zerop depth)
              (error "~@<Failed to expand property reference ~S~@:>"
                     spec))
            (let+ (((&values result match?)
                    (ppcre:regex-replace-all
                     "\\${([^${}]+)}" value
                     (lambda (expression name)
                       (if-let ((value (lookup name)))
                         (replace1 value (1- depth))
                         expression))
                     :simple-calls t)))
              (if match? (replace1 result (1- depth)) result)))))
    (replace1 spec)))

(defun %parse-maven-version-spec (string)
  (or (ppcre:register-groups-bind (open version close) ("(\\[|\\()?([^])]*)(\\]|\\))?" string)
        (when (or (and open close) (not (or open close)))
          version))
      (error "~@<Invalid version specification: ~S.~@:>"
             string)))

(defun %resolve-maven-version (spec properties)
  (%parse-maven-version-spec (%resolve-maven-value spec properties)))

;;; Conversion helpers

(deftype cons/property ()
  '(cons string string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'cons/property))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:name name) ".")
                            (value        "text()")) value
    (cons name value)))

(deftype list/dependency ()
  '(cons (or null string)
             (cons (or null string)
                   (cons (or null string)
                         (cons (or null string) null)))))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'list/dependency))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o
      ((group   "groupId/text()"    :if-no-match :do-nothing) ; TODO which of these are actually optional?
       (name    "artifactId/text()" :if-no-match :do-nothing)
       (version "version/text()"    :if-no-match :do-nothing)
       #+not-used (type    "type/text()"       :if-no-match :do-nothing)
       (scope   "scope/text()"      :if-no-match :do-nothing)
       :namespaces `((nil . ,+pom-namespace+)))
      value
    (list group name scope version)))

(declaim (ftype (function (list/dependency) (values (cons string (cons (or null string) null)) &optional))
                id->name+version))
(defun+ id->name+version ((group name scope version))
  (list (format nil "~A/~A~@[/~A~]" group name scope) version))

(declaim (ftype (function (list/dependency list/dependency)
                          (values list/dependency &optional))
                merge-ids))
(defun+ merge-ids ((&whole id1 group1 name1 scope1 version1)
                   (&whole id2 group2 name2 scope2 version2))
  (let+ (((&flet fail (component)
            (error "~@<None of the ids, ~A and ~A supplied for ~
                    merging, contain a ~S component.~@:>"
                   id1 id2 component))))
    (list (or group1   group2   (fail :group))
          (or name1    name2    (fail :name))
          (or scope1   scope2)
          (or version1 version2))))
