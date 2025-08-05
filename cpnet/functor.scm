(define-module (cpnet functor)
  #:use-module (srfi srfi-9)
  #:export (functor?
            functor-name
	    functor-src-cat
	    functor-tgt-cat
	    functor-obj-map
	    functor-mor-map
	    make-functor-record))

(define-record-type <functor>
  (make-functor-record name src-cat tgt-cat obj-map mor-map)
  functor?
  (name functor-name)
  (src-cat functor-src-cat)
  (tgt-cat functor-tgt-cat)
  (obj-map functor-obj-map)
  (mor-map functor-mor-map))

