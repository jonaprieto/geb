(in-package :geb.entry)

(defparameter +command-line-spec+
              '((("entry-point" #\e)
                 :type string :optional t :documentation
                 "The function to run, should be fully qualified. Default: geb.lambda.main::*entry ")
                (("stlc")
                 :type boolean :optional t :documentation
                 "Use the simply typed lambda calculus frontend")
                (("output" #\o)
                 :type string :optional t :documentation
                 "Save the output to a file rather than printing")
                (("vampir")
                 :type string :optional t :documentation
                 "Return a vamp-ir expression")
                (("version" #\v)
                 :type boolean :optional t :documentation
                 "GEB Binary version information")
                (("help" #\h #\?)
                 :type boolean :optional t :documentation
                 "The current help message")))

(defun main (&rest args)
  (command-line-arguments:handle-command-line
    +command-line-spec+
    #'argument-handlers
    :command-line args
    :name "geb"
    :rest-arity t))

(defun geb-info (field)
  (let ((system (asdf:find-system :geb nil)))
    (when (and system (slot-boundp system field))
          (slot-value system field))))

(defun help-message (stream)
  (format stream
      "GEB: Gödel, Escher, Bach~%
A categorical view of computation.~%

Usage:~%
  geb ((-h | --help) | --version | [LISP-FILE] [[OPTIONS] [arguments...]])~2%
OPTIONS:~%")
  (command-line-arguments:show-option-help +command-line-spec+
                                           :sort-names t)
  (format stream "~%For more information, visit https://anoma.github.io/geb~%"))


(defun argument-handlers (file &key
                               help version
                               entry-point
                               stlc
                               output
                               vampir)
  (format t "file: ~A~%" file)
  (flet ((run (stream)
              (cond
               (version
                 (format stream "~A~%" (geb-info 'asdf:version)))
               ((or help (null file)) (help-message stream))
               (t
                 (load file)
                 (let ((entry (if entry-point entry-point
                                  "geb.lambda.main::*entry*")))
                   (compile-down :vampir vampir
                                 :stlc stlc
                                 :entry entry
                                 :stream stream))))))
    (if output
        (with-open-file (file output :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create)
          (run file))
        (run *standard-output*))))

;; this code is very bad please abstract out many of the components
(defun compile-down (&key vampir stlc entry (stream *standard-output*))
  (let* ((name (read-from-string entry))
         (eval (eval name))
         (vampir-name (renaming-scheme (intern (symbol-name name) 'keyword))))
    (cond ((and vampir stlc)
            (geb.vampir:extract (to-circuit eval vampir-name) stream))
          (stlc
            (format stream "~A" (to-cat nil eval)))
          (vampir
            (geb.vampir:extract (to-circuit eval vampir-name) stream))
          (t
            (format stream eval)))))

;; Very bad of me, copying alucard code, please move elsewhere as
;; well!!

(-> renaming-scheme (symbol) keyword)
(defun renaming-scheme (symb)
  "Renames certain names to be valid for vampir"
  ;; the n here mutates a once only list, so no mutation at all!
  ;; at least after the first substitute
  (intern
    (~>> symb symbol-name
      (substitute #\_ #\-)
      (nsubstitute #\V #\&)
      (string-trim "*")
      (nsubstitute #\V #\%))
    :keyword))
