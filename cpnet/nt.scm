(define-module (cpnet nt)
  #:use-module (srfi srfi-9)
  #:export (make-nt-record
            nt?
            nt-name
            nt-src-functor
            nt-tgt-functor
            nt-components))

;; 자연 변환(Natural Transformation)을 나타내는 레코드 타입을 정의합니다.
(define-record-type <nt>
  (make-nt-record name src-functor tgt-functor components)
  nt?
  (name nt-name)
  (src-functor nt-src-functor)
  (tgt-functor nt-tgt-functor)
  (components nt-components))
