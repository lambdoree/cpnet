(define-module (cpnet nt)
  #:use-module (srfi srfi-9)
  #:export (make-nt-record
            nt?
            nt-src-functor
            nt-tgt-functor
            nt-components))

(define-record-type <natural-transformation>
  (make-nt-record src-functor tgt-functor components)
  nt?
  (src-functor nt-src-functor)
  (tgt-functor nt-tgt-functor)
  (components nt-components))

