(define-module (cpnet nt)
  #:use-module (srfi srfi-9)
  #:export (make-nt-record
            nt?
            nt-name
            nt-src-functor
            nt-tgt-functor
            nt-components))

(define-record-type <natural-transformation>
  (make-nt-record name src-functor tgt-functor components)
  nt?
  (name nt-name)
  (src-functor nt-src-functor)
  (tgt-functor nt-tgt-functor)
  (components nt-components))

