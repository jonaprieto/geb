(asdf:defsystem :geb
  :depends-on (:trivia :alexandria :serapeum :fset :fare-quasiquote-extras
                       ;; wed are only importing this for now until I
                       ;; give good instructions to update the asdf of sbcl
                       :cl-reexport
                       :mgl-pax
                       :command-line-arguments)
  :version "0.3.3"
  :description "Gödel, Escher, Bach, a categorical view of computation"
  :build-pathname "../build/geb"
  :entry-point "geb.entry::entry"

  :build-operation "program-op"

  :maintainer "Compilers Team at Heliax AG <hello@heliax.dev>"
  :author "Mariari, Artem Gureev, Terence Rokop, and GitHub Contributors"

  :license "GPL-3.0"

  :pathname "src/"
  :components

  ((:module util
            :serial t
            :description "Internal utility functions"
            :components ((:file package)
                         (:file utils)))
   (:module mixins
            :serial t
            :description "Mixin utility functions"
            :depends-on (util)
            :components ((:file package)
                         (:file meta)
                         (:file mixins)
                         (:file cat)))
   (:module generics
            :serial t
            :description "Generics utility functions"
            :depends-on (util)
            :components ((:file package)
                         (:file generics)))
   (:module extensions
            :serial t
            :depends-on (specs util vampir)
            :description "The extensions module"
            :components ((:file package)
                         (:file sub-expressions)))

   (:module vampir
            :serial t
            :description "The VampIR extraction module"
            :depends-on (specs)
            :components ((:file package)
                         (:file spec)
                         (:file print)
                         (:file vampir)))

   (:module geb
            :serial t
            :description "The main GEB module"
            :depends-on (util specs)
            :components ((:file package)
                         (:file geb)
                         (:file bool)
                         (:file list)
                         (:file decision)
                         (:file trans)))

   (:module poly
            :serial t
            :description "Polynomial model"
            :depends-on (util geb vampir specs extensions)
            :components ((:file package)
                         (:file poly)
                         (:file trans)))

   (:module bitc
            :serial t
            :description "BITC (Boolean Circuits)"
            :depends-on (util vampir mixins specs)
            :components ((:file package)
                         (:file bitc)
                         (:file trans)))

   (:module lambda
            :serial t
            :depends-on (geb specs)
            :description "STLC model"
            :components ((:file package)
                         (:module experimental
                                  :serial t
                                  :description "Experimental lambda code"
                                  :components
                                  ((:file package)
                                   (:file lambda)))
                         (:file lambda)
                         (:file trans)))

   (:module specs
            :serial t
            :depends-on (util mixins)
            :description "Internal utility functions"
            :components ((:file package)
                         (:file geb)
                         (:file geb-printer)
                         (:file lambda)
                         (:file poly)
                         (:file poly-printer)
                         (:file extension)
                         (:file extension-printer)
                         (:file bitc)
                         (:file bitc-printer)
                         (:file ../util/package)))

   (:module entry
            :serial t
            :description "Entry point for the GEB codebase"
            :depends-on (util geb vampir specs poly bitc lambda)
            :components ((:file package)
                         (:file entry))))

  :in-order-to ((asdf:test-op (asdf:test-op :geb/test))))

(asdf:defsystem :geb/test
  :depends-on (:geb :parachute :geb/gui)
  :description "Testing geb"
  :pathname "test/"
  :serial t
  :components
  ((:file package)
   (:file meta)
   (:file geb)
   (:file geb-trans)
   (:file lambda)
   (:file lambda-experimental)
   (:file lambda-trans)
   (:file poly)
   (:file bitc)
   (:file pipeline)
   (:file list)
   (:module gui
            :serial t
            :components ((:file test)
                         (:file graphing)))
   (:file run-tests))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :geb-test :run-tests-error)))


(asdf:defsystem :geb/documentation
  :depends-on (:geb :mgl-pax/navigate :MGL-PAX/FULL :cl-environments
                    :geb/test :pythonic-string-reader
                    :geb/gui)
  :description "geb full documentation exploration"
  :pathname "docs/"
  :serial t
  :components ((:file package)
               (:file glossery)
               (:file documentation))
  :in-order-to ((asdf:test-op (asdf:test-op :geb/test))))

(asdf:defsystem :geb/gui
  :depends-on (:geb :mcclim :clim :bordeaux-threads :mcclim-dot :mcclim-svg)
  :description "geb gui presenter"
  :pathname "src/gui/"
  :serial t
  :components ((:module graphing
                        :serial t
                        :description "The graphing algorithm"
                        :components ((:file package)
                                     (:file core)
                                     (:file passes)))
               (:file package)
               (:file common-abstractions)
               (:file shapes)
               (:file present-graph)
               (:file show-view)
               (:file stick-view)
               (:file list-view)
               (:file gui)
               (:file commands))
  :in-order-to ((asdf:test-op (asdf:test-op :geb/test))))

(defun load-docs-with-symbol-macro (&optional (ql nil))
  (if ql
      (progn (ql:quickload :mgl-pax/navigate)
             (ql:quickload :geb/documentation))
      (progn (asdf:load-system :mgl-pax/navigate)
             (asdf:load-system :geb/documentation))))

(defun make-system ()
  (handler-case (asdf:load-system :geb/documentation)
    (error (c)
      (declare (ignorable c))
      (ql:quickload :geb)))
  (asdf:make :geb))

(defun make-docs ()
  (handler-case
      (progn (asdf:load-system :mgl-pax/navigate)
             (asdf:load-system :geb/documentation))
    (error (c)
      (format t "Failed to load system: ~a" c)
      (uiop:quit 1)))
  (handler-case (progn (ql:quickload :geb)
                       (uiop:symbol-call 'geb-docs/docs 'build-docs)
                       (uiop:quit 0))
    (error (c)
      (format t "Failed to build docs: ~a" c)
      (uiop:quit 1))))