
;; todo:
;; I should have an object for Changes.

;; Hi level xkb config operations, useful for xfork.
(define-module xlib.xkb.fork
  (export
   push-keycode-to-forked
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
(define (push-keycode-to-forked desc device keycode new-keycode)
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
