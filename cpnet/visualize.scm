(define-module (cpnet visualize)
  #:use-module (srfi srfi-1)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module (cpnet system)
  #:use-module ((cpnet functor) :prefix functor:)
  #:use-module ((cpnet nt) :prefix nt:)
  #:export (system->dot))

;; Helper to draw a single propagator with context-sensitive styling
(define (draw-propagator mor port nt-components-map indent)
  (let* ((dom (cat:arrow-dom mor))
         (cod (cat:arrow-cod mor))
         (id (cat:arrow-id mor))
         (id-str (symbol->string id)))
    (unless (string-contains id-str "id-")
      (let* ((prop-node (format #f "prop_~a" id))
             (nt-info (hash-ref nt-components-map id #f))
             (is-functor-conn (string-prefix? "functor-conn-" id-str))
             (attrs (make-hash-table))
             (prop-label id))
        (hash-set! attrs 'shape "ellipse")
        
        (when nt-info
          (hash-set! attrs 'color "red")
          (hash-set! attrs 'style "\"bold,filled\"")
          (hash-set! attrs 'fillcolor "\"#ffdddd\"")
          (hash-set! attrs 'xlabel (format #f "\"η: ~a\"" nt-info)))
        
        (when is-functor-conn
          (let* ((parts (string-split id-str #\-)))
            (set! prop-label (string->symbol (car (last-pair (string-split (cadddr parts) #\>)))))
            (let ((fname-str (if (> (length parts) 2) (caddr parts) "anon")))
              (hash-set! attrs 'fontcolor "blue")
              (hash-set! attrs 'color "blue")
              (hash-set! attrs 'xlabel (format #f "\"F: ~a\"" fname-str)))))
        
        (hash-set! attrs 'label (format #f "\"~a\"" prop-label))

        (let ((attr-str (string-join
                         (hash-map->list (lambda (k v) (format #f "~a=~a" k v)) attrs)
                         ", ")))
          (format port "~a\"~a\" [~a];\n" indent prop-node attr-str))

        (if (list? dom)
            (for-each
             (lambda (d) (when d (format port "~a\"~a\" -> \"~a\";\n" indent (core:cell-id d) prop-node)))
             dom)
            (when dom (format port "~a\"~a\" -> \"~a\";\n" indent (core:cell-id dom) prop-node)))
        (if (list? cod)
            (for-each
             (lambda (c) (when c (format port "~a\"~a\" -> \"~a\";\n" indent prop-node (core:cell-id c))))
             cod)
            (when cod (format port "~a\"~a\" -> \"~a\";\n" indent prop-node (core:cell-id cod))))))))

(define (draw-category-cluster cat-name table port all-mors nt-components-map drawn-mors indent)
  (let* ((name-str (symbol->string cat-name))
         (parts (string-split name-str #\.))
         (short-cat-name (if (> (length parts) 1) (cadr parts) name-str)))
    (format port "~asubgraph \"cluster_~a\" {\n" indent cat-name)
    (format port "~a  label = \"~a\";\n" indent short-cat-name)
    (format port "~a  style=filled;\n" indent)
    (format port "~a  color=lightgrey;\n" indent)
    ;; Draw cells
    (hash-for-each
     (lambda (cell-name cell)
       (format port "~a  \"~a\" [label=\"~a\"];\n" indent (core:cell-id cell) cell-name))
     table)
    ;; Draw internal propagators
    (let* ((cat-cells (hash-map->list (lambda (k v) v) table))
           (internal-mors (filter
                           (lambda (m)
                             (let* ((dom-nodes (if (list? (cat:arrow-dom m)) (cat:arrow-dom m) (list (cat:arrow-dom m))))
                                    (cod-nodes (if (list? (cat:arrow-cod m)) (cat:arrow-cod m) (list (cat:arrow-cod m))))
                                    (all-nodes (append dom-nodes cod-nodes)))
                               ;; A mor is internal if all its cells are in this category's cell list
                               (and (not (null? all-nodes))
                                    (every (lambda (c) (and c (member c cat-cells))) all-nodes))))
                           all-mors)))
      (for-each
       (lambda (mor)
         (draw-propagator mor port nt-components-map (string-append indent "  "))
         (hash-set! drawn-mors (cat:arrow-id mor) #t))
       internal-mors))
    (format port "~a}\n" indent)))

(define (system->dot system file-path)
  (let ((port (open-output-file file-path))
        (all-mors (cat:category-morphisms (system-get-net system)))
        (drawn-mors (make-hash-table))
        (nt-components-map (make-hash-table))
        (categories-by-system (make-hash-table))
        (morphisms-by-system (make-hash-table)))

    ;; Build a map from NT component morphisms to NT names
    (for-each
     (lambda (nt)
       (let ((nt-name (nt:nt-name nt)))
         (for-each
          (lambda (component)
            (let ((mor (cdr component)))
              (hash-set! nt-components-map (cat:arrow-id mor) nt-name)))
          (nt:nt-components nt))))
     (system-nts system))

    ;; Group categories by subsystem
    (hash-for-each
     (lambda (cat-name table)
       (let* ((name-str (symbol->string cat-name))
              (parts (string-split name-str #\.)))
         (if (> (length parts) 1)
             (let* ((sys-name-str (car parts))
                    (sys-name (string->symbol sys-name-str)))
               (hash-set! categories-by-system sys-name
                          (cons cat-name (hash-ref categories-by-system sys-name '()))))
             (let ((toplevel-key #f)) ; special key for non-prefixed categories
               (hash-set! categories-by-system toplevel-key
                          (cons cat-name (hash-ref categories-by-system toplevel-key '())))))))
     (system-get-cell-tables system))

    ;; Group morphisms by subsystem
    (for-each
     (lambda (mor)
       (let* ((id-str (symbol->string (cat:arrow-id mor)))
              (parts (string-split id-str #\.)))
         (if (> (length parts) 1)
             (let* ((sys-name-str (car parts))
                    (sys-name (string->symbol sys-name-str)))
               (if (hash-ref categories-by-system sys-name #f)
                   (hash-set! morphisms-by-system sys-name
                              (cons mor (hash-ref morphisms-by-system sys-name '())))
                   (let ((toplevel-key #f))
                     (hash-set! morphisms-by-system toplevel-key
                                (cons mor (hash-ref morphisms-by-system toplevel-key '()))))))
             (let ((toplevel-key #f))
               (hash-set! morphisms-by-system toplevel-key
                          (cons mor (hash-ref morphisms-by-system toplevel-key '())))))))
     all-mors)
    

    (format port "digraph G {\n")
    (format port "  rankdir=LR;\n")
    (format port "  node [shape=box, style=rounded];\n")
    (format port "  graph [compound=true, label=\"System: ~a\", fontcolor=darkgreen, fontsize=18];\n"
            (system-name system))

    ;; Draw categories and their internal propagators, grouped by system
    (let ((cell-tables (system-get-cell-tables system)))
      ;; Draw subsystem clusters
      (hash-for-each
       (lambda (sys-name cat-names)
         (when sys-name ; not toplevel
           (let* ((sys-name-str (symbol->string sys-name))
                  (is-engine (string-suffix? "-engine" sys-name-str))
                  (label (if is-engine (string-drop-right sys-name-str 7) sys-name-str)))
             (format port "  subgraph \"cluster_~a\" {\n" sys-name)
             (format port "    label = \"~a\";\n" label)
             (format port "    style=filled;\n")
             (if is-engine
                 (format port "    color=lightblue;\n")
                 (format port "    color=whitesmoke;\n"))
             ;; Draw internal categories
             (for-each
              (lambda (cat-name)
                (draw-category-cluster cat-name (hash-ref cell-tables cat-name)
                                       port all-mors nt-components-map drawn-mors "    "))
              (reverse cat-names))
             ;; Draw system-level propagators
             (let ((sys-mors (reverse (hash-ref morphisms-by-system sys-name '()))))
               (for-each
                (lambda (mor)
                  (unless (hash-ref drawn-mors (cat:arrow-id mor) #f)
                    (draw-propagator mor port nt-components-map "    ")
                    (hash-set! drawn-mors (cat:arrow-id mor) #t)))
                sys-mors))
             (format port "  }\n"))))
       categories-by-system)

      ;; Draw toplevel categories
      (let ((toplevel-cats (reverse (hash-ref categories-by-system #f '()))))
        (for-each
         (lambda (cat-name)
           (draw-category-cluster cat-name (hash-ref cell-tables cat-name)
                                  port all-mors nt-components-map drawn-mors "  "))
         toplevel-cats)))

    ;; Draw propagators that are not in any subsystem (e.g. functor connections)
    (let ((toplevel-mors (reverse (hash-ref morphisms-by-system #f '()))))
      (for-each
       (lambda (mor)
         (unless (hash-ref drawn-mors (cat:arrow-id mor) #f)
           (draw-propagator mor port nt-components-map "  ")))
       toplevel-mors))

    ;; Draw Legend
    (format port "  legend [shape=none, margin=0, label=<\n")
    (format port "    <table border=\"1\" cellborder=\"0\" cellspacing=\"0\" cellpadding=\"4\">\n")
    (format port "      <tr><td colspan=\"2\"><b>Legend</b></td></tr>\n")
    (format port "      <tr><td align=\"right\">Cell</td><td>Rounded Box</td></tr>\n")
    (format port "      <tr><td align=\"right\">Propagator</td><td>Ellipse</td></tr>\n")
    (format port "      <tr><td align=\"right\">Category</td><td>Light-gray Box</td></tr>\n")
    (format port "      <tr><td align=\"right\">System</td><td>Light-smoke Box</td></tr>\n")
    (format port "      <tr><td align=\"right\" port=\"i1\">Functor Impl.</td><td align=\"left\"><font color=\"blue\">Blue Ellipse (F: name)</font></td></tr>\n")
    (format port "      <tr><td align=\"right\" port=\"i2\">NT Component</td><td align=\"left\"><font color=\"red\">Red Ellipse (η: name)</font></td></tr>\n")
    (format port "    </table>\n")
    (format port "  >];\n")


    (format port "}\n")
    (close-output-port port)))
