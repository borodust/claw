(asdf:defsystem :claw/util
  :description "Various utilities used across CLAW subsystems"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :local-time :named-readtables :claw-utils)
  :pathname "src/util/"
  :serial t
  :components ((:file "sha1")
               (:file "infix")
               (:file "util")))


(asdf:defsystem :claw/spec
  :description "C/C++ spec API"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw/util)
  :pathname "src/spec/"
  :serial t
  :components ((:file "entity")))


(asdf:defsystem :claw/wrapper
  :description "Wrapper definition interface for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :claw/util)
  :pathname "src/wrap/"
  :serial t
  :components ((:file "packages")
               (:file "library")
               (:file "wrapper")))


(asdf:defsystem :claw/resect
  :description "Spec generation support using libresect"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-resect :claw/util :claw/spec
                     :claw/wrapper :parse-number)
  :pathname "src/resect/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "template")
               (:file "inspect")
               (:file "macro")
               (:file "prepare")
               (:file "resect")))


(asdf:defsystem :claw/generator/common
  :description "Common code for included CLAW generators"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cffi :cl-ppcre
               :trivial-features :claw/util
               :claw/spec :float-features)
  :pathname "src/gen/common/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module "generator"
                :serial t
                :components ((:file "type")
                             (:file "primitive")
                             (:file "constant")
                             (:file "enum")
                             (:file "function")))
               (:module "adapter"
                :serial t
                :components ((:file "adapter")
                             (:static-file "template/dynamic.c")
                             (:file "dynamic")
                             (:static-file "template/static.c")
                             (:file "static")))))


(asdf:defsystem :claw/generator/cffi
  :description "CFFI generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/wrapper :claw/generator/common)
  :pathname "src/gen/cffi/c/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module generator
                :serial t
                :components ((:file "type")
                             (:file "typedef")
                             (:file "record")
                             (:file "function")))))


(asdf:defsystem :claw/generator/iffi
  :description "Intricate foreign function interface generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/wrapper :claw/generator/common :iffi)
  :pathname "src/gen/iffi/cxx/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module "generator"
                :serial t
                :components ((:file "type")
                             (:file "function")
                             (:file "class")
                             (:file "template")
                             (:file "alias")
                             (:file "variable")))))


(asdf:defsystem :claw
  :description "Generate clean & lean bindings to foreign libraries easily"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:cffi :claw/wrapper :claw/resect :claw/generator/cffi :claw/generator/iffi)
  :pathname "src/"
  :serial t
  :components ((:file "packages")))
