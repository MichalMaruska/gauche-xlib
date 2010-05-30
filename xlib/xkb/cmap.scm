
;; operating on the Client map (of XKB)

(define-module xlib.xkb.cmap
  (export
   find-type-by-name
   ;;find-new-keycode
   ;; xkb-allocate-keycode   ... what is this? ... fork
   xkb-set-key-types
   type-names->types

   check-types-vs-matrix
   define-keycode-keysyms
   xkb-dump-keysyms

   ;; fancy:  return strings, not objects
   type->levels
   dump-types
   keycode->type-name
   xkb-keycode->keysym

   ;; iterator:
   xkb-filter-keycode
   xkb-for-each-keycodes
   )
  (use xlib)
  (use mmc.simple)
  (use xlib.xkb.geometry)
  (use mmc.log)
  (use srfi-1)
  (use mmc.exit)
  )

(select-module xlib.xkb.cmap)

(define max-num-groups 4)


(define (type-names->types desc type-names)
  (map (cute find-type-by-name desc <>) type-names))

;;; make the KEYCODE have types types-list
(define (xkb-set-key-types desc keycode types-list . rest)
  (let-optionals* rest
      ((commit? #f))
    (if (> (length types-list) max-num-groups)
        (error "types assiciated with a keycode can be 4 at max"))
    ;; find the types  string->
    (let* ((real-types
            (map
                (lambda (type-name)
                  (receive (type index)
                      (find-type-by-name desc type-name)
                    (cons type index)))
              types-list))
           ;;
           (width
            (fold
             (lambda (type-pair width)
               (max width (ref (car type-pair) 'num-levels))) ; isn't it a rectangle?
             0
             real-types
             ))
           (total-size (* (length real-types) width)))
      ;; allocate
      (xkb-resize-keysyms desc keycode total-size)
      (xkb-resize-key-actions desc keycode total-size)
      ;; set
      (xkb-change-types-of-keys desc keycode (map cdr real-types))
      ;; commit
      (if commit?
          (xkb-change-keycode desc keycode))
      width)))



;; types is a list of real <xkb-type> objects
;; use: (types (type-names->types desc type-names))
(define (check-types-vs-matrix types keysyms-matrix)
  (cond
   ((not (= (length keysyms-matrix)
            (length types)))
    (error "mismatch between the number of types provided, and lists of keysyms."))
   ;; now, check that every type has the right number of
   ((not (every (lambda (keysyms type)
                  (= (length keysyms)
                     (slot-ref type 'num-levels)))
             keysyms-matrix
           types))
    (error "the matrix has different rows than the types have (shift) levels."))
   (else
    #t)))





(define (define-keycode-keysyms desc keycode type-names keysyms-matrix modifier)
  (let ((types (type-names->types desc type-names))
        (cmap (ref desc 'map)))

    (check-types-vs-matrix types keysyms-matrix)

    ;; mmc: why not the types?
    (let1 width (xkb-set-key-types desc keycode type-names)
      ;(logformat "width: ~d\n" width)
      ;;
      (fold
       (lambda (type keysyms offset)
         ;; put ...
         (for-numbers* i 0 (- (length keysyms) 1) ;shoule be eqv? levels of type
           (xkb-client-map-set-keysym!
            cmap (+ offset i)
            (x-string->keysym (list-ref keysyms i))))
         (+ offset width))
       ;; fixme: actions??
       (ref (xkb-keycode->node desc keycode) 'offset)
       types
       keysyms-matrix)
      ;;
      (xkb-modmap-set! desc keycode modifier)
      (xkb-change-keycode desc keycode))))

;; `inverse' to `define-keycode-keysyms'
(define (xkb-dump-keysyms desc keycode)
  (let* ((node (xkb-keycode->node desc keycode))
         (client-map (ref desc 'map))
         ;;
         (types (map-numbers* group 0 (ref node 'groups)
                  (xkb-client-map->type client-map (xkb-key-type desc keycode group))))
         ;; these are objects/types
         (type-names (map (lambda (t)
                            (x-atom-name (ref desc 'dpy) (ref t 'name)))
                       types))
         (width (ref node 'width))
         
         (keysym-matrix
          (reverse
           (third
            (fold
             (lambda (type seed)
                                        ;(logformat "another step\n")
               (receive (offset type-index matrix) (apply values seed)
                                        ;(logformat "another step:\n")
                 (list
                  (+ width offset)
                  (+ 1 type-index)
                  (cons
                   ;; levels -> keysyms
                   (map-numbers* level 0 (slot-ref type 'num-levels)
                     (xkb-keycode->keysym desc keycode type-index level))
                   matrix))))
             (list (ref node 'offset) 0 '())
             types)))))
    ;; types
    ;; keysyms
    (values type-names keysym-matrix (xkb-modmap desc keycode)))) ;modmap? xkb-modmap


;; xkb key type 
;; return (values type index)
(define (find-type-by-name desc name)
  (let* ((dpy (ref desc 'dpy))
         (client-map (ref desc 'map))
         (num-types (ref client-map 'num-types))
         (found #f)
         (index #f))
    ;; fixme: throw it!
    (map-numbers 0 num-types
      (lambda (i)
        (let ((type (xkb-client-map->type client-map i)))
          (when (string=? name (x-atom->name dpy (slot-ref type 'name)))
            (set! found type)
            (set! index i)))))
    (values found index)))

;;; fancy
;; return a list of level _names_
(define (type->levels type desc)
  (map-numbers 0 (slot-ref type 'num-levels)
    (lambda (i)
      (x-atom->name (ref desc 'dpy)
        (xkb-type->level type i)))))

(define (dump-types client-map dpy)
  (map-numbers 0 (slot-ref client-map 'num-types)
    (lambda (i)
      (let ((type (xkb-client-map->type client-map i)))
        (list
         i
         (x-atom->name dpy
                       (slot-ref type 'name))
         (type->levels type dpy))))))


;; Get the type name  of keycode/group pair.
(define (keycode->type-name desc keycode . rest)
  (let-optionals* rest
      ((group 0))
    ;(slot-ref desc 'map)
    (x-atom->name (ref desc 'dpy)
      (ref (xkb-client-map->type
            (slot-ref desc 'map)
            (xkb-key-type desc keycode group))
           'name))))


;; the keycode _name_  at keycode/group/shift-level
(define (xkb-keycode->keysym desc keycode group shift-level)
  (let* ((node (xkb-keycode->node desc keycode)))
    ;; (map (ref desc 'map))
    ;; fixme: does it have at least one?  the group & the shift-level !!!
    ;; check, that the shift-level is available for the type!
    (x-keysym->string
     (xkb-index->keysym
      (ref desc 'map)
      (+ (slot-ref node 'offset)
         (* group (slot-ref node 'width))
         shift-level)
      ))))



;; general iterator over the keycodes.
(define (xkb-filter-keycode desc predicate)
  (let1 collector '()
    (for-numbers* i (ref desc 'min-key-code)
                  (ref desc 'max-key-code)
      (with-chained-exception-handler*
          (lambda (c next)
            (logformat "problems with keycode ~d\n" i)
            (next c))
        (if (predicate i desc)
                 (push! collector i))))
    collector))

;; if the user has DESC, it's useless to pass back to function.
;; use (cute function desc <>)
(define (xkb-for-each-keycodes desc function)
  (for-numbers* keycode (ref desc 'min-key-code)
                (ref desc 'max-key-code)
    (function keycode)))




(provide "xlib/xkb/cmap")
