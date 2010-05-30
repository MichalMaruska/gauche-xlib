
;; todo:
;; I should have an object for Changes.

;; Hi level xkb config operations, useful for xfork.
(define-module xlib.xkb.fork
  (export
   make-modifier-key
   make-group-key
   xkb-allocate-keycode
   ;make-modifier-forked
   push-modifier-to-forked
   fork-to!
   fork-from
   forks-to
   xfork:debug
   ;; forked-pairs... get a list of forks.
   )
  (use mmc.log)
  (use xlib)
  (use xlib.xkb.cmap)
  (use xlib.xkb.geometry)
  ;(use gauche.parameter)
  )

(select-module xlib.xkb.fork)

;(make-parameter fork-device XkbUseCoreKbd)
;(fork-device)

(define (xfork:debug dpy device yes?)
    ;; debuggin (tracing output)
    (xfork-configure dpy device fork_configure_debug (if yes? 1 0)))



;; mmc: not on geometry & forked-to
;; `keycode-reached?'
;; xlib.xkb.geometry


;; find first keycode, which is not on the keyboard,
;; and has no groups/types/ i.e. keysyms associated!
;; todo: non forked to (i.e. fork destination).
(define (xkb-allocate-keycode desc device)     ;find-new-keycode
  ;; a list to avoid, or predicate.
  (let* ((busy (xkb-unallocated-keycodes desc))
                                        ;(let1 client (ref desc 'map)
         (new (let step ((i (+ 1 ;(ref desc 'min-key-code)
                               49)))
                (if (or (vector-ref busy i)
                        ;; ??
                        (let1 node (xkb-keycode->node desc i)
                          (not (zero? (ref node 'groups))))
                        (not (null? (forks-to (ref desc 'dpy) i)))
                        ;;(not (zero? (ref (xkb-keycode->node desc i) 'groups)))
                        )
                    (step (+ 1 i))
                  i))))
    (logformat "xkb-allocate-keycode: ~d\n" new)
    new))
    

;(vector-ref (xkb-unallocated-keycodes desc) 101)




;; obsolete?
;; Initialize a 'new' keycode to be a modifier.
;; it's not necessarily a modifier: mod = 0
(define (make-modifier-key desc keycode keysym mod)
  ;; only/just 1
  (xkb-set-key-types desc keycode (list "ONE_LEVEL"))
    ;; keysym:
  (xkb-client-map-set-keysym!
   (ref desc 'map)
   (ref (xkb-keycode->node desc keycode) 'offset)
   keysym)

  ;; make the `modifier' bit
  (xkb-modmap-set! desc keycode mod)

  (unless (zero? mod)
    ;(define-keycode-actions desc keycode types actions)
    (let1 action (xkb-key-action desc keycode 0)
      (slot-set! action 'real_mods mod)
      (slot-set! action 'flags 5)))
  ;; no auto-repeat!
  (let1 xkb-c (xkb-desc->controls desc)
    (xkb-control-keyrepeats-set! xkb-c keycode #f))
  ;; commit:
  ;(xkb-set-controls dpy XkbPerKeyRepeatMask desc)
  ;(xkb-change-keycode desc keycode)
  )




;; obsolete?
(define (make-group-key desc keycode group) ;-number
  ;; only/just 1
  (xkb-set-key-types desc keycode (list "ONE_LEVEL"))

  ;; keysym:  `irrelevant!'
  (xkb-client-map-set-keysym!
   (ref desc 'map)
   (ref (xkb-keycode->node desc keycode) 'offset)
   0)

  ;; make the `action' bit
  (unless (zero? group)
    (let1 action (xkb-key-action desc keycode 0)
      ;; 5 is for group type
      ;XkbSA_SetGroup)
      (slot-set! action 'flags 0)
      (slot-set! action 'group_XXX 2)))

  ;; log-ior
  (xkb-explicit-set! desc keycode 16)
  ;; commit:
  ;(xkb-change-keycode desc keycode)
  )


;; todo
(define-class x-device ()
  ((dpy)
   (device)
   ))
  

;; scheme version of the C program  xfork ?
(define (fork-to! dpy device original forked)
  (xfork-configure-key dpy device fork_configure_key_fork
                       original forked))


(define (fork-from dpy device keycode)
  (xfork-get-configure-key
   dpy
   device
   fork_configure_key_fork
   keycode))


;; list of keycodes which fork to FORK-KEYCODE
(define (forks-to dpy device fork-keycode)
  (xkb-filter-keycode
   (xkb-get-desc dpy)			;device
   (lambda (keycode desc)
     (= fork-keycode
        (fork-from dpy device keycode)))))



;; I have a key bound to a keycode, which is a modifer.
;; I want to associate the key with another keycode
;; and let it fork to the original keycode (modifier).
(define (push-modifier-to-forked desc device keycode new-keycode)
  ;; i should test if keycode is really a modifier?
  (xkb-switch-keycodes desc keycode new-keycode)
  (fork-to! (ref desc 'dpy) device keycode new-keycode))


(define (xkb-switch-keycodes desc one two)
  (xkb-map-switch desc one two)
  ;; switch the per-key-repeat
  (let* ((xkb-c (xkb-desc->controls desc))
         (control-1 (xkb-control-keyrepeats xkb-c one)))
    ;; fixme  not boolean
    (xkb-control-keyrepeats-set! xkb-c one
                                 (= (xkb-control-keyrepeats xkb-c two) 1))
    (xkb-control-keyrepeats-set! xkb-c two (= 1 control-1)))
  (xkb-change-keycode desc one)
  (xkb-change-keycode desc two))
                             

(select-module xlib.xkb.fork)
(provide "xlib/xkb/fork")
