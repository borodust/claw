(cl:in-package :claw.wrapper)

(declaim (special *always-generate*))

(defvar *wrapper-registry* (make-hash-table :test 'equal))


(defun sanitize-wrapper-name (name)
  (make-keyword (uiop:standard-case-symbol-name name)))


(defun register-wrapper (name configuration)
  (setf (gethash (sanitize-wrapper-name name) *wrapper-registry*) configuration))


(defun generate-default-header-name (symbol)
  (format nil "~A.h" (substitute #\_ #\- (string-downcase (symbol-name symbol)))))


(defstruct persistent-options
  asd-path
  bindings-system
  bindings-path
  system-depends-on)


(defstruct parse-options
  headers
  includes
  framework-includes
  defines
  intrinsics
  system-includes)


(defun parse-parse-options (opts)
  (destructuring-bind (&key
                         headers
                         includes
                         framework-includes
                         defines
                         intrinsics
                         system-includes
                       &allow-other-keys)
      (alist-plist opts)
    (with-evaluated-lists (headers
                           includes
                           framework-includes
                           system-includes
                           defines
                           intrinsics)
      (let* ((includes (mapcar #'map-path
                               (append
                                (list nil)
                                includes)))
             (system-includes (mapcar #'map-path
                                      (append
                                       (list nil)
                                       system-includes
                                       (list-all-known-include-paths))))
             (framework-includes (mapcar #'map-path
                                         (append
                                          (list nil)
                                          framework-includes
                                          (list-all-known-framework-paths)))))
        (make-parse-options :headers headers
                            :includes includes
                            :framework-includes framework-includes
                            :system-includes system-includes
                            :defines defines
                            :intrinsics intrinsics)))))


(defstruct target-options
  features
  triple

  parse)


(defun parse-persistent-options (name config)
  (when (first config)
    (destructuring-bind (bindings-system &key asd-path bindings-path depends-on) config
      (let* ((bindings-system (if (eq t bindings-system)
                                  (make-keyword (substitute
                                                 #\- #\/
                                                 (format nil "~A-~A"
                                                         name
                                                         :bindings)))
                                  bindings-system))
             (bindings-path (map-path (or bindings-path "bindings/")))
             (asd-path (map-path (or asd-path
                                     (substitute
                                      #\- #\/
                                      (format nil "~(~A~).asd"
                                              bindings-system))))))
        (make-persistent-options :asd-path asd-path
                                 :bindings-path bindings-path
                                 :bindings-system bindings-system
                                 :system-depends-on depends-on)))))


(defstruct wrapper-options
  system
  base-path
  persistent

  language
  standard
  parser
  generator

  targets
  selected-target
  parse

  instantiation-filter

  include-sources
  include-definitions
  exclude-sources
  exclude-definitions)


(defmacro selected-parse-option (opts name)
  (let ((accessor-name (symbolicate 'parse-options- name)))
    (once-only (opts)
      (with-gensyms (target)
        `(let ((,target (wrapper-options-selected-target ,opts)))
           (append
            (when ,target
              (,accessor-name (target-options-parse ,target)))
            (,accessor-name (wrapper-options-parse ,opts))))))))


(defun wrapper-options-headers (opts)
  (selected-parse-option opts headers))

(defun wrapper-options-includes (opts)
  (selected-parse-option opts includes))

(defun wrapper-options-system-includes (opts)
  (selected-parse-option opts system-includes))

(defun wrapper-options-framework-includes (opts)
  (selected-parse-option opts framework-includes))

(defun wrapper-options-defines (opts)
  (selected-parse-option opts defines))

(defun wrapper-options-intrinsics (opts)
  (selected-parse-option opts intrinsics))


(defstruct wrapper
  name
  options
  configuration
  target
  entities
  (always-generate nil))


(defun merge-wrapper-pathname (pathname wrapper)
  (map-path pathname))


(defun predefined-targets (&key (linux "gnu") (windows "msvc") (darwin "gnu"))
  `(((:and :x86-64 :linux) . ,(string+ "x86_64-pc-linux-" linux))
    ((:and :x86 :linux) . ,(string+ "i686-pc-linux-" linux))
    ((:and :ppc64 :linux :big-endian) . ,(string+ "powerpc64-pc-linux-" linux))
    ((:and :ppc64 :linux :little-endian) . ,(string+ "powerpc64le-pc-linux-" linux))
    ((:and :x86-64 :windows) . ,(string+ "x86_64-pc-windows-" windows))
    ((:and :x86-64 :windows) . ,(string+ "i686-pc-windows-" windows))
    ((:and :x86-64 :darwin) . ,(string+ "x86_64-apple-darwin-" darwin))
    ((:and :x86-64 :darwin) . ,(string+ "i686-apple-darwin-" darwin))))


(defun eval-targets (targets)
  (loop for (features target . parse-opts) in targets
        collect (make-target-options :features features
                                     :triple target
                                     :parse (parse-parse-options parse-opts))))


(defun eval-opts (name opts)
  (destructuring-bind (&key
                         system
                         base-path
                         (persistent '(t))

                         language
                         standard
                         parser
                         generator

                         (targets '(:native))
                         instantiate

                         include-sources include-definitions
                         exclude-sources exclude-definitions
                       &allow-other-keys)
      (alist-plist opts)
    (with-evaluated-variables (base-path
                               language
                               standard
                               parser
                               generator
                               instantiate)
      (with-evaluated-lists (include-sources
                             include-definitions
                             exclude-sources
                             exclude-definitions)
        (let* ((system (or (first system) (when (asdf:find-system name nil) name)))
               (base-path (when base-path
                            (find-path base-path :system system)))
               (*path-mapper* (lambda (path)
                                (find-path path :system system :path base-path)))
               (parser (or parser :claw/resect))
               (targets (case (first targets)
                          (:local `((t . ,(local-platform))))
                          (:gnu (predefined-targets :linux "gnu"
                                                    :windows "gnu"
                                                    :darwin "gnu"))
                          (:native (predefined-targets))
                          (t (eval-targets targets)))))
          (make-wrapper-options :system system
                                :base-path base-path
                                :persistent (parse-persistent-options name persistent)

                                :language language
                                :standard standard
                                :parser parser
                                :generator generator

                                :targets targets
                                :parse (parse-parse-options opts)

                                :include-sources include-sources
                                :include-definitions include-definitions
                                :exclude-sources exclude-sources
                                :exclude-definitions exclude-definitions
                                :instantiation-filter instantiate))))))


(defun make-wrapper-options-for-target (opts target)
  (let ((copy (copy-wrapper-options opts)))
    (setf (wrapper-options-targets copy) (list target)
          (wrapper-options-selected-target copy) target)
    copy))


(defun make-bindings-table (name opts configuration)
  (loop with table = (make-hash-table :test 'equal)
        for target in (wrapper-options-targets opts)
        for triple = (target-options-triple target)
        for selected-opts = (make-wrapper-options-for-target opts target)
        for library = (describe-foreign-library
                       (wrapper-options-parser selected-opts)
                       (wrapper-options-headers selected-opts)
                       :language (wrapper-options-language selected-opts)
                       :standard (wrapper-options-standard selected-opts)
                       :includes (append (wrapper-options-includes selected-opts)
                                         (wrapper-options-system-includes selected-opts))
                       :framework-includes (wrapper-options-framework-includes selected-opts)
                       :target triple
                       :defines (wrapper-options-defines selected-opts)
                       :intrinsics (wrapper-options-intrinsics selected-opts)
                       :instantiation-filter (wrapper-options-instantiation-filter selected-opts)
                       :include-sources (wrapper-options-include-sources selected-opts)
                       :include-definitions (wrapper-options-include-definitions selected-opts)
                       :exclude-sources (wrapper-options-exclude-sources selected-opts)
                       :exclude-definitions (wrapper-options-exclude-definitions selected-opts))
        for selected-language = (or (wrapper-options-language selected-opts)
                                    (foreign-library-language library)
                                    :c)
        for selected-generator = (or (wrapper-options-generator selected-opts)
                                     (ecase selected-language
                                       (:c :claw/cffi)
                                       (:c++ :claw/iffi)))
        for entities = (foreign-library-entities library)
        do (setf (gethash triple table) (generate-bindings
                                         selected-generator
                                         selected-language
                                         (make-wrapper :name name
                                                       :options selected-opts
                                                       :configuration configuration
                                                       :entities entities
                                                       :target triple
                                                       :always-generate *always-generate*)
                                         configuration))
        finally (return table)))


(defun unexport-package-symbols (packages)
  (loop for package-name in packages
        for package = (find-package package-name)
        when package
          append (let (exported)
                   (do-external-symbols (symbol package)
                     (handler-case
                         (progn
                           (unexport symbol package)
                           (push exported symbol))
                       (package-error ())))
                   exported)))


(defun reexport-package-symbols (symbols)
  (loop for symbol in symbols
        do (export symbol (symbol-package symbol))))


(defun persist-bindings (opts bindings-table)
  (let* ((persistent-opts (wrapper-options-persistent opts))
         (bindings-path (persistent-options-bindings-path persistent-opts))
         (generated-package-name (format-symbol :keyword "~A~A"
                                                (persistent-options-bindings-system persistent-opts)
                                                '~pristine))
         selected-target
         feature-targets
         required-systems
         (timestamp-comment (format-claw-timestamp-text)))
    (flet ((%bindings-file (target)
             (merge-pathnames (format nil "~A.lisp" target) bindings-path)))
      (loop for target-opts in (wrapper-options-targets opts)
            for features = (target-options-features target-opts)
            for triple = (target-options-triple target-opts)
            for bindings = (gethash triple bindings-table)
            for bindings-file = (%bindings-file triple)
            when (or (null selected-target)
                     (eq t features)
                     (uiop:featurep features))
              do (setf selected-target (%bindings-file triple))
            when (or *always-generate*
                     (not (probe-file bindings-file)))
              do (push (cons features triple) feature-targets)
                 (push (bindings-required-systems bindings) required-systems)
                 (with-open-file (out bindings-file
                                      :direction :output
                                      :external-format :utf-8
                                      :if-exists :supersede)

                   (format out "~A~%" timestamp-comment)
                   (let ((*print-pretty* t)
                         (*print-case* :downcase)
                         (*print-circle* nil)
                         (*package* (find-package :%claw.wrapper.pristine)))
                     (flet ((print-define-package (package &rest use)
                              (format out "(uiop:define-package ")
                              (prin1 package out)
                              (format out " ")
                              (prin1 `(:use ,@use) out)
                              (format out ")")))
                       (loop for package in (bindings-required-packages bindings)
                             do (print-define-package package)
                                (terpri out))
                       (print-define-package generated-package-name :cl)
                       (terpri out)
                       (prin1 `(cl:in-package ,generated-package-name) out)
                       (fresh-line out)
                       (terpri out))
                     (let ((*package* (find-package :%claw.wrapper.cl))
                           (symbols (unexport-package-symbols (bindings-required-packages bindings))))
                       (unwind-protect
                            (progn
                              (unexport-bindings bindings)
                              (loop for binding in (bindings-definition bindings)
                                    do (prin1 binding out)
                                       (fresh-line out)
                                       (terpri out)))
                         (reexport-package-symbols symbols)
                         (reexport-bindings bindings)))))))
    (values selected-target feature-targets required-systems)))


(defun persist-bindings-asd (name persistent-opts feature-targets required-systems)
  (let* ((bindings-system (persistent-options-bindings-system persistent-opts))
         (bindings-path (persistent-options-bindings-path persistent-opts))
         (asd-path (persistent-options-asd-path persistent-opts))
         (asd-dir (uiop:pathname-directory-pathname asd-path))
         (enough-bindings-path (uiop:enough-pathname bindings-path asd-dir)))
    (when (string= (namestring asd-path)
                   (namestring enough-bindings-path))
      (error "Bindings path must be a subpath of .asd directory"))
    (when (or *always-generate*
              (not (probe-file asd-path)))
      (ensure-directories-exist bindings-path)
      (ensure-directories-exist asd-dir)
      (with-open-file (out asd-path
                           :direction :output
                           :external-format :utf-8
                           :if-exists :supersede)
        (let ((*print-pretty* t)
              (*print-case* :downcase)
              (*print-circle* nil)
              (*package* (find-package :cl-user)))
          (format-claw-timestamp-text out)
          (format out "~&(asdf:defsystem #:~A" bindings-system)
          (format out "~&  :description \"Bindings generated by ~A\"" name)
          (format out "~&  :author \"CLAW\"")
          (format out "~&  :license \"Public domain\"")
          (format out "~&  :defsystem-depends-on (:trivial-features)")
          (when required-systems
            (format out "~&  :depends-on ")
            (prin1 (remove-duplicates
                    (append (list :uiop)
                            (flatten required-systems)
                            (persistent-options-system-depends-on persistent-opts))
                    :test #'equal
                    :key (lambda (name) (string-downcase (string name))))
                   out))
          (format out "~&  :components~&  ")
          (prin1 (loop for (features . target) in (reverse feature-targets)
                       collect `(:file ,(namestring (merge-pathnames target enough-bindings-path))
                                 :if-feature ,features))
                 out)
          (format out ")")
          ;; Add warning for unsupported platforms
          (let ((*print-pretty* nil))
            (format out "~%#-(:or ")
            (loop for features in (mapcar #'car feature-targets)
                  do (prin1 features out))
            (format out ")")
            (format out "~%")
            (prin1 `(warn ,(format nil "Current platform unrecognized or unsupported by ~A system"
                                   bindings-system))
                   out)))))))



(defun persist-bindings-and-asd (name opts bindings-table)
  (let ((persistent-opts (wrapper-options-persistent opts)))
    (multiple-value-bind (selected-target
                          feature-targets
                          required-systems)
        (persist-bindings opts bindings-table)
      (unless (zerop (hash-table-count bindings-table))
        (persist-bindings-asd name persistent-opts feature-targets required-systems))
      selected-target)))


(defun persist-and-load-bindings (name opts bindings-table)
  (when-let (selected-target (persist-bindings-and-asd name opts bindings-table))
    (load selected-target)))


(defun expand-bindings (opts bindings-table)
  `(progn
     ,@(loop for (features . target) in (wrapper-options-targets opts)
             when (or (eq t features)
                      (uiop:featurep features))
               return (bindings-definition (gethash target bindings-table)))))


(defun call-with-wrapper-opts (name wrapper-handler &key always-generate)
  (let ((name (sanitize-wrapper-name name)))
    (destructuring-bind (opts . configuration)
        (if-let (wrapper-def (gethash name *wrapper-registry*))
          wrapper-def
          (error "Wrapper ~A not found" name))
      (let* ((*always-generate* always-generate)
             (opts (eval-opts name opts))
             (*path-mapper* (lambda (path)
                              (find-path path :system (wrapper-options-system opts)
                                              :path (wrapper-options-base-path opts))))
             (bindings-table (make-bindings-table name opts configuration)))
        (funcall wrapper-handler name opts bindings-table)))))


(defmacro with-wrapper-opts ((name opts bindings-table &key always-generate) wrapper-name &body body)
  `(call-with-wrapper-opts ,wrapper-name
                           (lambda (,name ,opts ,bindings-table)
                             ,@body)
                           :always-generate ,always-generate))


(defun generate-wrapper (name)
  (call-with-wrapper-opts name #'persist-bindings-and-asd :always-generate t))


(defun load-wrapper (name)
  (with-wrapper-opts (name opts bindings-table) name
    (if (wrapper-options-persistent opts)
        (persist-and-load-bindings name opts bindings-table)
        (eval (expand-bindings opts bindings-table)))))


(defmacro defwrapper (name-and-opts &body configuration)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    `(register-wrapper ',name (cons ',opts ',configuration))))


(defmacro include (path-or-paths &key in-package)
  (with-gensyms (name)
    `(progn
       (defwrapper (,name
                    (:headers ,@(ensure-list path-or-paths))
                    (:targets :local)
                    (:persistent nil))
         :in-package ,in-package)
       (load-wrapper ,name))))
