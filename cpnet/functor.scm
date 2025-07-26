(define-module (cpnet functor)
  #:use-module (srfi srfi-9)
  #:export (functor?
	    functor-src-cat
	    functor-tgt-cat
	    functor-obj-map
	    functor-mor-map
	    make-functor))

(define-record-type <functor>
  (make-functor-record src-cat tgt-cat obj-map mor-map)
  functor?
  (src-cat functor-src-cat)
  (tgt-cat functor-tgt-cat)
  (obj-map functor-obj-map)
  (mor-map functor-mor-map))

(define (make-functor src-cat tgt-cat obj-map mor-map)
  (make-functor-record src-cat tgt-cat obj-map mor-map))
