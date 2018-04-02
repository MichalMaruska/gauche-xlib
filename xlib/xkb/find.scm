(define-module xlib.xkb.find
  (export
   xkb-types=?                          ;an ugly hack
   xkb-find-key-by-keysyms   xkb-find-key-by-keysyms-or-create
   xkb-find-key-by-action    xkb-find-key-by-action-or-create

   ;; partial
   keysym+group+level->keycode
   keysym->keycode

   keysym->modifier

   filter-keycodes  map-keycodes

   ;;
   xkb-move-keysyms
   xkb-copy-keysyms
   xkb-copy-actions
   ;;
   check-modifier!
   ;;
   virtual->real-bit
   old-virtual->real
   vmodifier->real
   )


  (use srfi-1)
  (use srfi-13)

  (use mmc.simple)
  (use mmc.log)
  (use mmc.throw)

  (use adt.bits)
  (use xlib)
  (use xlib.xkb)
  (use xlib.xkb.cmap)
  (use xlib.xkb.server)
  (use xlib.xkb.fork)

  (use adt.alist)
  (use adt.list)                        ;?? list-constant?
  (use alg.find)

  (use gauche.sequence)
  )


(select-module xlib.xkb.find)

;; fixme: elsewhere
(define (xkb-types=? a b)
  (=
   (ref a 'name)
   (ref b 'name)))



;; find keycodes w/ given types & keysyms bound:
(define (xkb-find-key-by-keysyms desc type-names keysyms-matrix) ; modifier-name
  ;; types is a list
  (let ((types (type-names->types desc type-names))
        (group-count (length type-names))
        (client-map (ref desc 'map)))
    ;; fixme: i should check that keysyms is
    (check-types-vs-matrix types keysyms-matrix)

    (xkb-filter-keycode
     desc
     (lambda (keycode desc)
       (let1 ngroups (ref (xkb-keycode->node desc keycode) 'groups)
         (cond
          ((not (= group-count ngroups))
           ;;(logformat "~d has a different number of groups: ~d\n" keycode ngroups)
           #f)
          ((not (every xkb-types=?
                    types
                  (map-numbers* i 0 ngroups
                    (xkb-client-map->type client-map
                                          (xkb-key-type desc keycode i)))))

           '(logformat "~d has ~d group, but a different types\n" keycode
                       ngroups)
           ;;(ref (xkb-client-map->type client-map (xkb-key-type desc keycode 0))
           ;; 'name)
           #f)
          ;; check the keysyms:
          ((catch 'found
             (for-numbers* group 0 (- ngroups 1)
               ;; group -> type
               (let1 type (xkb-client-map->type client-map
                                                (xkb-key-type desc keycode group))
                 (unless
                     (list=
                      equal? ;; fixme: either #f or a string! string=?
                      (map-numbers* level 0 (slot-ref type 'num-levels)
                        (xkb-keycode->keysym desc keycode group level))
                      (list-ref keysyms-matrix group))
                   (throw 'found #t))))
             #f)
           '(logformat "~d has different keysym: ~a\n" keycode
                       (xkb-keycode->keysym desc keycode 0 0))
           #f)
          (else #t)))))))


;; fixme: the modifier is not used for searching. we don't either use the actions,
;; so Finds the minimum keycode!  OVERWRITE ... the modifier is forced. This should
;; be removed?
(define (xkb-find-key-by-keysyms-or-create desc type-names keysyms-matrix
                                           modifier . rest)
  (let-optionals* rest
      ((overwrite #t))
    (let1 existing (xkb-find-key-by-keysyms desc type-names keysyms-matrix)
      ;;fixme: modifier not considered!
      (if (null? existing)
          (let1 new-keycode (xkb-allocate-keycode desc)
            (logformat "xkb-find-key-by-keysyms: we have to CREATE a new keycode: ~d\n" new-keycode)
            (define-keycode-keysyms
              desc
              new-keycode
              type-names
              keysyms-matrix
              modifier)
            new-keycode)

        (let1 candidate (apply min existing)
          ;; does the modifier match?
          (if (= modifier (xkb-modmap desc candidate))
              candidate
            (begin
              (logformat "a possible candidate ~d does not hold the modifier bit ~d, but has ~d!\n"
                candidate modifier (xkb-modmap desc candidate))
              (when overwrite
                (logformat "overwriting!\n")
                (xkb-modmap-set! desc candidate modifier)
                (xkb-change-keycode desc candidate))
              candidate)))))))



;;; fixme: should i check the keysym too?
(define (xkb-find-key-by-action desc action) ; modifier-name
  ;; types is a list
  (let ((type (find-type-by-name desc "ONE_LEVEL")) ;fixme: is it correct to assume all use this one?
        (client-map (ref desc 'map))
        (action-matcher (action-matcher action)))
    (xkb-filter-keycode
     desc
     (lambda (keycode desc)
       (and (= 1 (ref (xkb-keycode->node desc keycode) 'groups))
            (xkb-types=? type
                         (xkb-client-map->type client-map
                                               (xkb-key-type desc keycode 0)))

            (xkb-key-has-actions desc keycode)
            ;; is this useless: ?
            (= 1 (xkb-key-num-actions desc keycode))
            (action-matcher
             (xkb-key-action desc keycode 0)))))))


(define (xkb-find-key-by-action-or-create desc action keysym modifier vmodifier)
  (let1 found (xkb-find-key-by-action desc action)
    (if (null? found)
        (let ((new-keycode (xkb-allocate-keycode desc))
              (type-names '("ONE_LEVEL")))
          (define-keycode-keysyms
            desc
            new-keycode
            type-names
            `((,keysym))
            modifier)
          ;; actions
          (define-keycode-actions desc new-keycode type-names `((,action)))
          (list new-keycode))
      (car found))))


;; todo copy actions!
(define (xkb-copy-keysyms desc from-keycode to-keycode)
  (receive (types keysyms-matrix modifier) (xkb-dump-keysyms desc from-keycode)
    (logformat "copying types ~a and keysyms ~s\n" types keysyms-matrix)
    (define-keycode-keysyms desc to-keycode types keysyms-matrix modifier)))



;;
(define (xkb-move-keysyms desc from-keycode to-keycode)
  (let1 dpy (ref desc 'dpy)
    (unless (null? (forks-to to-keycode))
        (error "xkb-move-keysyms: assertion failed"))
    ;; i have to see who forks to `from-keycode'
    ;; and i assume to-keycode is unused.
    (let1 forked-from (forks-to dpy from-keycode)
      (xkb-copy-keysyms desc from-keycode to-keycode)
      ;; fixme: move the actions!!!
      ;; re-fork
      (for-each
          (lambda (keycode)
            (fork-to! dpy keycode to-keycode))
        forked-from)
      (x-flush dpy))))


;; same types!!!
(define (xkb-copy-actions desc from-keycode to-keycode)
  (cond
   ((xkb-key-has-actions desc from-keycode)
    ;; vmodifier
    (receive (types actions-matrix) (xkb-dump-actions desc from-keycode)
      (logformat "copying actions ~s\n" actions-matrix)
      (define-keycode-actions desc to-keycode
        types actions-matrix)))         ; vmodifier
   (else
    ;; set NO actions!  needs C support.
    (logformat "xkb-set-no-actions!  needed. Skipped now\n")
    ;(xkb-set-no-actions! desc to-keycode)
    )))






;; keycode + level + group -> keysym ?
;; (xkb-keycode->keysym desc keycode group shift-level)


(define (map-keycodes desc function)
  (let ((min (ref desc 'min-key-code))
        (max (ref desc 'max-key-code)))
    (map function (iota (- max min) min))))

(define (filter-keycodes desc function)
  (let ((min (ref desc 'min-key-code))
        (max (ref desc 'max-key-code)))
    (filter function (iota (- max min) min))))


;; fixme:
;; Find the keycode to the left of Space:
;; Where is space ? in group 0 and level 0:
(define (keysym+group+level->keycode desc keysym group level)
  (let1 client-map (ref desc 'map)
    (filter-keycodes desc
    ;; filter
      (lambda (keycode)
        (let1 node (xkb-keycode->node desc keycode)

          '(logformat "trying ~d: ~d ~d\t" i
                      (slot-ref (xkb-client-map->node client-map keycode) 'offset)
                      (xkb-index->keysym
                       (ref desc 'map)
                       (slot-ref (xkb-client-map->node xkb-map keycode) 'offset)))
          ;; "space"
          (and
           ;; fixme: this is wrong:
           (> (ref node 'groups) group) ;i use group 0!
           ;;fixme:  the type of the group has the level
           (xkb-keysym-of desc keycode group level)
           (string=? keysym (xkb-keysym-of desc keycode group level))))))))


;; todo: also the bit?
(define (check-modifier! desc keycode modifier-name)
  (logformat "check-modifier! ~d ~a\n" keycode modifier-name)
  (unless (= (xkb-modmap desc keycode)
             (xkb-vmod-name desc (string->modifier desc modifier-name)))
    (logformat "keycode ~d with ~a keysym does not have the modifier bit ~d set! Only ~d\n"
      keycode
      modifier-name
      (xkb-vmod-name desc (string->modifier desc modifier-name))
      (xkb-modmap desc keycode))
    ;'(error 1)
    )
  ;; repeatable
  (xkb-control-keyrepeats-set! (xkb-desc->controls desc) keycode #f))


;; not very nice, but
(define (keysym->keycode desc keysym)
  (let1 all (keysym+group+level->keycode
             desc keysym 0 0)
    (if (null? all)
        #f ;(error "no keycode is bound to keysym (at group/level 0 0)" keysym)
      (car all))))


;; the old way?   fixme: might accept a list of keysyms?
(define (keysym->modifier desc keysym)
  (let* ((keycodes (keysym+group+level->keycode desc keysym 0 0))
         (modifiers (map (cut xkb-modmap desc <>) keycodes)))
    (unless (list-constant? modifiers)
      ;; (logformat "keysym ~s is associated with different modifiers: ~s.
      ;;   Aborting\n" keysym modifiers)
      (errorf "keysym ~s is associated with different modifiers: ~s.  Aborting\n"
              keysym modifiers))
    (car modifiers)))



;;;
;; new:
(define (virtual->real-bit desc name)
  (let1 index (find-index
               (cut string=? <> name)
               (list->vector (xkb-get-virtual-modifier-list desc)))

    ;; todo: is (xkb-get-vmods desc index) better?
    (if index
        (xkb-get-vmods desc index)
      #f)))

(define virtual->keysym-table
  '(("shift" "Shift_R" "Shift_L")
    ("meta" "Meta_L" "Meta_R")
    ("alt" "Alt_L" "Alt_R")
    ("hyper" "Hyper_L" "Hyper_R")
    ("super" "Super_L" "Super_R")))

(define (old-virtual->real desc modifier)
  (let* ((keysyms (aget virtual->keysym-table modifier))
         (keycode
          (find-value
           (cut keysym->keycode desc <>)
           keysyms)))
    (if keycode
        (xkb-modmap desc keycode)
      ;(error "old-virtual->real: cannot find any of " keysyms)
      64
      )))


;; canonize the vmodifier?
(define (vmodifier->real desc vmodifier)
  (let* ((vmodifier-list (xkb-get-virtual-modifier-list desc))
         (name (car (member vmodifier vmodifier-list string-ci=)))
         (bit (let1 real (virtual->real-bit desc name)
                (if (zero? real)
                    (old-virtual->real desc vmodifier)
                  real)))
         )
    ;; the name:
    (logformat "found the virtual modifier ~a as ~a: bit ~d\n" vmodifier name bit)
    bit
    ;; the real bits?
    ))




(provide "xlib/xkb/find")
