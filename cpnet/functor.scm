(define-module (cpnet functor)
  #:use-module (srfi srfi-9)
  #:export (make-functor-record
            functor?
            functor-src-cat
            functor-tgt-cat
            functor-obj-map
            functor-mor-map
            functor-name))

;; 函子(Functor)를 나타내는 레코드 타입을 정의합니다.
(define-record-type <functor>
  (make-functor-record name src-cat tgt-cat obj-map mor-map)
  functor?
  (name functor-name)
  (src-cat functor-src-cat)
  (tgt-cat functor-tgt-cat)
  (obj-map functor-obj-map)
  (mor-map functor-mor-map))
