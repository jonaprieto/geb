;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(geb.utils:muffle-package-variance
 (defpackage #:geb.main
   (:documentation "Gödel, Escher, Bach categorical model")
   (:use #:common-lisp #:geb.generics #:geb.extension.spec #:serapeum #:geb.mixins #:geb.utils #:geb.spec)
   (:local-nicknames (#:poly #:geb.poly.spec) (#:bitc #:geb.bitc.spec))
   (:shadowing-import-from #:geb.spec :left :right :prod :case)
   (:export :prod :case :mcar :mcadr :mcaddr :mcdr :name :func :obj :dom :codom
            #:@geb-utility)))

(in-package #:geb.main)

(pax:defsection @geb-utility (:title "Utility")
  "Various utility functions ontop of @GEB-CATEGORIES"
  (pair-to-list      pax:function)
  (same-type-to-list pax:function)
  (cleave            pax:function)
  (const             pax:function)
  (commutes          pax:function)
  (commutes-left     pax:function)
  (!->               pax:function)
  (so-eval           pax:function)
  (so-hom-obj        pax:function)
  (so-card-alg       pax:generic-function)
  (so-card-alg       (pax:method () (<substobj>)))
  (curry             pax:function)
  (coprod-mor        pax:function)
  (prod-mor          pax:function)
  (uncurry           pax:function)
  (text-name         pax:generic-function)

  "These utilities are ontop of [CAT-OBJ]"
  (maybe             (pax:method () (<substobj>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library throughout the codebase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(geb.utils:muffle-package-variance
 (uiop:define-package #:geb.common
   (:documentation "Provides the standard library for any GEB code")
   (:shadowing-import-from #:geb.spec :left :right :prod :case)
   (:import-from #:trivia #:match)
   (:use-reexport #:geb.mixins #:geb.generics #:geb.extension.spec #:geb.spec #:geb.main #:geb.utils
                  #:serapeum #:common-lisp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trans module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(geb.utils:muffle-package-variance
 (defpackage #:geb.trans
   (:documentation "Gödel, Escher, Bach categorical model")
   (:use #:common-lisp #:serapeum #:geb.mixins #:geb.utils #:geb.spec #:geb.main
         #:geb.generics)
   (:local-nicknames (#:poly #:geb.poly.spec) (#:bitc #:geb.bitc.spec))
   (:shadowing-import-from #:geb.spec :left :right :prod :case)
   (:export :prod :case :mcar :mcadr :mcaddr :mcdr :name :func :obj
            #:@geb-translation)))

(in-package #:geb.trans)

(pax:defsection @geb-translation (:title "Translation Functions")
  "These cover various conversions from @GEB-SUBSTMORPH and @GEB-SUBSTMU
into other categorical data structures."
  (to-poly    (pax:method () (<substobj>)))
  (to-poly    (pax:method () (<substmorph>)))
  (to-circuit (pax:method () (<substmorph> t)))
  (to-bitc    (pax:method () (<substmorph>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bool module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(geb.utils:muffle-package-variance
 (uiop:define-package #:geb-bool
   (:documentation "Defines out booleans for the geb language")
   (:mix #:geb.main #:geb.spec #:serapeum #:common-lisp)
   (:shadow :false :true :not :and :or)
   (:export
    :bool :fasle :true :not :and :or
    #:@geb-bool)))

(in-package #:geb-bool)

(pax:defsection @geb-bool (:title "Booleans")
  "Here we define out the idea of a boolean. It comes naturally from the
concept of coproducts. In ML they often define a boolean like

```haskell
data Bool = False | True
```

We likewise define it with coproducts

```lisp
(def bool (coprod so1 so1))

(def true  (->right so1 so1))
(def false (->left  so1 so1))
```

The functions given work on this."
  (true      pax:symbol-macro)
  (false     pax:symbol-macro)
  (false-obj pax:symbol-macro)
  (true-obj  pax:symbol-macro)
  (bool      pax:symbol-macro)
  (not       pax:symbol-macro)
  (and       pax:symbol-macro)
  (or        pax:symbol-macro))


(geb.utils:muffle-package-variance
 (uiop:define-package #:geb-list
   (:documentation "Defines out booleans for the geb language")
   (:use #:geb.common)
   (:export #:@geb-list)))

(in-package #:geb-list)

(pax:defsection @geb-list (:title "Lists")
  "Here we define out the idea of a List. It comes naturally from the
concept of coproducts. Since we lack polymorphism this list is
concrete over [GEB-BOOL:@GEB-BOOL][section] In ML syntax it looks like

```haskell
data List = Nil | Cons Bool List
```

We likewise define it with coproducts, with the recursive type being opaque

```lisp
(defparameter *nil* (so1))

(defparameter *cons-type* (reference 'cons))

(defparameter *canonical-cons-type*
  (opaque 'cons
          (prod geb-bool:bool *cons-type*)))

(defparameter *list*
  (coprod *nil* *cons-type*))
```

The functions given work on this."
  (*nil*       pax:variable)
  (*cons-type* pax:variable)
  (*list*      pax:variable)
  (*car*       pax:variable)
  (*cons*      pax:variable)
  (*cdr*       pax:variable)
  (cons->list  pax:symbol-macro)
  (nil->list   pax:symbol-macro)
  (*canonical-cons-type* pax:variable))

(geb.utils:muffle-package-variance
 (uiop:define-package #:geb-decision
   (:documentation "Defines out a decision datatype for for the geb language")
   (:nicknames :geb-dec)                ; nicer shorthand
   (:shadow :no :yes)
   (:mix #:geb.main #:geb.spec #:serapeum #:common-lisp)))

(in-package #:geb-decision)

(pax:defsection @geb-decision (:title "Decisions")
  "Here we define out the idea of a Decision. Namely it allows us to
model information that may be uncertain. In ADT terms the type would
look something like

```lisp
(deftype decision () `(or yes no maybe))
```

In GEB terms it is defined like

```lisp
(def decision
  (coprod yes (coprod no maybe)))
```

We also define out API functions to operate on this"
  (decision      pax:symbol-macro)
  (yes           pax:symbol-macro)
  (no            pax:symbol-macro)
  (maybe         pax:symbol-macro)
  (inj-maybe     pax:symbol-macro)
  (inj-yes       pax:symbol-macro)
  (inj-no        pax:symbol-macro)
  (demote        pax:symbol-macro)
  (promote       pax:symbol-macro)
  (merge-opinion pax:symbol-macro))



(geb.utils:muffle-package-variance
 (uiop:define-package #:geb
   (:documentation "Gödel, Escher, Bach categorical model")
   (:use #:geb.common)
   (:use-reexport #:geb.spec #:geb.trans #:geb.main)
   (:export #:@geb #:geb-api #:@geb-examples)))

(in-package #:geb)

(pax:defsection @geb (:title "The Geb Model")
  "Everything here relates directly to the underlying machinery of
   GEB, or to abstractions that help extend it."
  (@mixins-cat       pax:section)
  (@generics         pax:section)
  (@geb-categories   pax:section)
  (@geb-accessors    pax:section)
  (@geb-constructors pax:section)
  (@geb-api          pax:section)
  (@geb-examples     pax:section))

(pax:defsection @geb-api (:title "API")
  "Various forms and structures built on-top of @GEB-CATEGORIES"
  (gapply                     (pax:method () (<substmorph> t)))
  (gapply                     (pax:method () (opaque-morph t)))
  (gapply                     (pax:method () (opaque t)))
  (geb-bool:@geb-bool        pax:section)
  (geb-list:@geb-list        pax:section)
  (geb.trans:@geb-translation pax:section)
  (@geb-utility               pax:section))

(pax:defsection @geb-examples (:title "Examples")
  "PLACEHOLDER: TO SHOW OTHERS HOW EXAMPLES WORK"
  "Let's see the transcript of a real session of someone working
  with GEB:

  ```cl-transcript
  (values (princ :hello) (list 1 2))
  .. HELLO
  => :HELLO
  => (1 2)

  (+ 1 2 3 4)
  => 10
  ```")
