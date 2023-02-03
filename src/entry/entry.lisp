(in-package :geb.entry)

(defparameter +command-line-spec+
  '((("input" #\i)
     :type string :documentation "Input geb file location")
    (("entry-point" #\e)
     :type string :documentation "The function to run, should be fully qualified I.E. geb::my-main")
    (("stlc" #\l)
     :type boolean :optional t :documentation "Use the simply typed lambda calculus frontend")
    (("output" #\o)
     :type string :optional t :documentation "Save the output to a file rather than printing")
    (("vampir" #\v)
     :type string :optional t :documentation "Return a vamp-ir expression")
    (("help" #\h #\?)
     :type boolean :optional t :documentation "The current help message")))

(defun entry ()
  (setf uiop:*command-line-arguments* (uiop:command-line-arguments))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   #'argument-handlers
   :name "geb"))

(defun argument-handlers (&key help stlc output input entry-point vampir)
  (flet ((run (stream)
           (cond (help
                  (command-line-arguments:show-option-help +command-line-spec+
                                                           :sort-names t))
                 (t
                  (load input)
                  (compile-down :vampir vampir
                                :stlc stlc
                                :entry entry-point
                                :stream stream)))))
    (if output
        (with-open-file (file output :direction :output
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
          (run file))
        (run *standard-output*))))



;; this code is very bad please abstract out many of the components
(defun compile-down (&key vampir stlc entry (stream *standard-output*))
  (let* ((name        (read-from-string entry))
         (eval        (eval name))
         (vampir-name (renaming-scheme (intern (symbol-name name) 'keyword))))
    (cond ((and vampir stlc)
           (geb.vampir:extract
            (list
             (stlc-to-vampir nil
                             (lambda:typed-stlc-type eval)
                             (lambda:typed-stlc-value eval)
                             vampir-name))
            stream))
          (stlc
           (format stream
                   (stlc-to-morph nil
                                  (lambda:typed-stlc-type eval)
                                  (lambda:typed-stlc-value eval))))
          (vampir
           (geb.vampir:extract (list (morph-to-vampir eval vampir-name))))
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
        (nsubstitute #\V #\%))
   :keyword))

;; Very bad of me, please move this to a proper pipeline and properly
;; set it up

(defun morph-to-vampir (morph name)
  (poly::to-circuit (geb:to-poly morph) name))

(defun stlc-to-morph (ctx ty term)
  (conversion:compile-checked-term ctx ty term))

(defun stlc-to-vampir (ctx ty term name)
  (morph-to-vampir (stlc-to-morph ctx ty term) name))