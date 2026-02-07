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
  language
  standard

  include-sources
  include-definitions
  exclude-sources
  exclude-definitions
  enforce-sources
  enforce-definitions
  ignore-sources
  ignore-definitions

  headers
  includes
  framework-includes
  defines
  intrinsics
  system-includes)


(defun parse-parse-options (opts)
  (destructuring-bind (&key
                         language
                         standard

                         include-sources
                         include-definitions
                         exclude-sources
                         exclude-definitions
                         enforce-sources
                         enforce-definitions
                         ignore-sources
                         ignore-definitions

                         headers
                         includes
                         framework-includes
                         defines
                         intrinsics
                         system-includes
                       &allow-other-keys)
      (alist-plist opts)
    (with-evaluated-variables (language
                               standard)
      (with-evaluated-lists (headers
                             includes
                             framework-includes
                             system-includes
                             defines
                             intrinsics

                             include-sources
                             include-definitions
                             exclude-sources
                             exclude-definitions
                             enforce-sources
                             enforce-definitions
                             ignore-sources
                             ignore-definitions)
        (let* ((includes (mapcar #'map-path includes))
               (system-includes (mapcar #'map-path system-includes))
               (framework-includes (mapcar #'map-path framework-includes)))
          (make-parse-options :language language
                              :standard standard
                              :headers headers
                              :includes includes
                              :framework-includes framework-includes
                              :system-includes system-includes
                              :defines defines
                              :intrinsics intrinsics
                              :include-sources include-sources
                              :include-definitions include-definitions
                              :exclude-sources exclude-sources
                              :exclude-definitions exclude-definitions
                              :enforce-sources enforce-sources
                              :enforce-definitions enforce-definitions
                              :ignore-sources ignore-sources
                              :ignore-definitions ignore-definitions))))))


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

  parser
  generator

  targets
  parse

  instantiation-filter)


(defstruct wrapper
  name
  options
  configuration
  target
  entities
  (always-generate nil))


(defun merge-wrapper-pathname (pathname wrapper)
  (declare (ignore wrapper))
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
  (loop for (features triple . parse-opts) in targets
        collect (make-target-options :features features
                                     :triple triple
                                     :parse (parse-parse-options parse-opts))))


(defun wrapper-options-standard (opts)
  (parse-options-standard (wrapper-options-parse opts)))

(defun wrapper-options-headers (opts)
  (parse-options-headers (wrapper-options-parse opts)))

(defun wrapper-options-includes (opts)
  (parse-options-includes (wrapper-options-parse opts)))

(defun wrapper-options-defines (opts)
  (parse-options-defines (wrapper-options-parse opts)))

(defun wrapper-options-intrinsics (opts)
  (parse-options-intrinsics (wrapper-options-parse opts)))


(defun eval-opts (name opts)
  (destructuring-bind (&key
                         system
                         base-path
                         (persistent '(t))

                         parser
                         generator

                         (targets '(:local))
                         instantiate
                       &allow-other-keys)
      (alist-plist opts)
    (with-evaluated-variables (base-path
                               parser
                               generator
                               instantiate)
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
                              :parser parser
                              :generator generator
                              :targets targets
                              :parse (parse-parse-options opts)
                              :instantiation-filter instantiate)))))


(defun make-wrapper-options-for-target (opts features triple parse-opts)
  (let ((copy (copy-wrapper-options opts)))
    (setf (wrapper-options-targets copy) (list (make-target-options
                                                  :features features
                                                  :triple triple))
          (wrapper-options-parse copy) parse-opts)
    copy))


(defun combine-parse-options (wrapper-opts target-opts)
  (macrolet ((%parse-opt (opt-name opts)
               (let ((opt-name (second opt-name)))
                 (once-only (opts)
                   `(when ,opts
                      (,(format-symbol *package*
                                       "~A~A" 'parse-options- opt-name)
                       ,opts))))))
    (let* ((common-parse-opts (wrapper-options-parse wrapper-opts))
           (target-parse-opts (target-options-parse target-opts))
           (language (or
                      (%parse-opt 'language target-parse-opts)
                      (%parse-opt 'language common-parse-opts)
                      :c))
           (standard (or
                      (%parse-opt 'standard target-parse-opts)
                      (%parse-opt 'standard common-parse-opts)))
           (features (target-options-features target-opts))
           (triple (target-options-triple target-opts)))
      (make-parse-options :include-sources
                          (append
                           (%parse-opt 'include-sources common-parse-opts)
                           (%parse-opt 'include-sources target-parse-opts))
                          :include-definitions
                          (append
                           (%parse-opt 'include-definitions common-parse-opts)
                           (%parse-opt 'include-definitions target-parse-opts))
                          :exclude-sources
                          (append
                           (%parse-opt 'exclude-sources common-parse-opts)
                           (%parse-opt 'exclude-sources target-parse-opts))
                          :exclude-definitions
                          (append
                           (%parse-opt 'exclude-definitions common-parse-opts)
                           (%parse-opt 'exclude-definitions target-parse-opts))
                          :enforce-sources
                          (append
                           (%parse-opt 'enforce-sources common-parse-opts)
                           (%parse-opt 'enforce-sources target-parse-opts))
                          :enforce-definitions
                          (append
                           (%parse-opt 'enforce-definitions common-parse-opts)
                           (%parse-opt 'enforce-definitions target-parse-opts))
                          :ignore-sources
                          (append
                           (%parse-opt 'ignore-sources common-parse-opts)
                           (%parse-opt 'ignore-sources target-parse-opts))
                          :ignore-definitions
                          (append
                           (%parse-opt 'ignore-definitions common-parse-opts)
                           (%parse-opt 'ignore-definitions target-parse-opts))

                          :language language
                          :standard standard

                          :headers
                          (append
                           (%parse-opt 'headers common-parse-opts)
                           (%parse-opt 'headers target-parse-opts))
                          :includes
                          (append (list (map-path nil))
                                  (%parse-opt 'includes target-parse-opts)
                                  (%parse-opt 'includes common-parse-opts))
                          :system-includes
                          (or (append
                               (%parse-opt 'system-includes target-parse-opts)
                               (%parse-opt 'system-includes common-parse-opts))
                              (list-system-include-paths language triple features))
                          :framework-includes
                          (or (append
                               (%parse-opt 'framework-includes target-parse-opts)
                               (%parse-opt 'framework-includes common-parse-opts))
                              (list-framework-paths language triple features))
                          :defines
                          (append
                           (%parse-opt 'defines common-parse-opts)
                           (%parse-opt 'defines target-parse-opts))
                          :intrinsics
                          (or
                           (%parse-opt 'intrinsics target-parse-opts)
                           (%parse-opt 'intrinsics common-parse-opts))))))


(defun make-bindings-table (name opts configuration)
  (loop with table = (make-hash-table :test 'equal)
        for target in (wrapper-options-targets opts)
        for triple = (target-options-triple target)
        for parse-opts = (combine-parse-options opts target)
        for library = (describe-foreign-library
                       (wrapper-options-parser opts)
                       (parse-options-headers parse-opts)
                       :language (parse-options-language parse-opts)
                       :standard (parse-options-standard parse-opts)
                       :includes (append
                                  (parse-options-includes parse-opts)
                                  (parse-options-system-includes parse-opts))
                       :framework-includes (parse-options-framework-includes parse-opts)
                       :target triple
                       :defines (parse-options-defines parse-opts)
                       :intrinsics (parse-options-intrinsics parse-opts)
                       :instantiation-filter (wrapper-options-instantiation-filter opts)
                       :include-sources (parse-options-include-sources parse-opts)
                       :include-definitions (parse-options-include-definitions parse-opts)
                       :exclude-sources (parse-options-exclude-sources parse-opts)
                       :exclude-definitions (parse-options-exclude-definitions parse-opts)
                       :enforce-sources (parse-options-enforce-sources parse-opts)
                       :enforce-definitions (parse-options-enforce-definitions parse-opts)
                       :ignore-sources (parse-options-ignore-sources parse-opts)
                       :ignore-definitions (parse-options-ignore-definitions parse-opts))
        for selected-language = (or (parse-options-language parse-opts)
                                    (foreign-library-language library)
                                    :c)
        for selected-generator = (or (wrapper-options-generator opts)
                                     (ecase selected-language
                                       (:c :claw/cffi)
                                       (:c++ :claw/iffi)))
        for entities = (foreign-library-entities library)
        do (setf (gethash triple table) (generate-bindings
                                         selected-generator
                                         selected-language
                                         (make-wrapper :name name
                                                       :options (make-wrapper-options-for-target
                                                                 opts
                                                                 (target-options-features target)
                                                                 triple
                                                                 parse-opts)
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
