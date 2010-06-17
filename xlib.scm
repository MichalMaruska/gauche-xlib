
(define-module xlib
  (export-all
   )
  (use gauche.uvector)
  (use mmc.simple)
  (use srfi-1)
  (use mmc.log)
  )

(select-module xlib)
(dynamic-load "xlib")



;; xkbdpy/xdpy should have these slots:
;; xkb base
;; xkb error
;; version

(define-class <xkb-dpy> ()              ;<xdpy>
  (
   ;; how to do it in C ?
   (dpy :init-keyword :dpy)
   
   (base :init-keyword :base)
   (error :init-keyword :error)
   (version :init-keyword :version)
   ;(major :init-keyword :major)
   ;(major :init-keyword :major)
   ))


(define (xkb-open display)
  (receive (dpy base error major minor)
      (xkb-open-display display)
    (make <xkb-dpy>
      :dpy dpy
      :base base
      :error error
      :version (cons major minor)
      )))

(define (x-open . rest)
  (let-optionals* rest
      ((display (sys-getenv "DISPLAY")))
    (if (not display)
        (error "environment variable DISPLAY not set!")
      (let1 dpy (xkb-open display)
        (slot-ref dpy 'dpy)))))


  
;(xkb-open-display ":1")

'(
  (use xlib)
  (define my-xdpy (xkb-open ":0"))
  (x-get-wm-name  (slot-ref my-xdpy 'dpy) 81957677)
  (slot-ref my-xdpy 'version)
  (slot-ref my-xdpy 'base) 
  (slot-ref my-xdpy 'error) 

  ;; (deselect
  ;;(slot-ref my-xdpy 'xdpy)
  ;; my-xdpy
  (xkb-select-events
   (slot-ref my-xdpy 'dpy)
   XkbUseCoreKbd
   XkbActionMessageMask
   XkbActionMessageMask
   )

  )

;;; Atoms
(define (x-atom->name dpy atom)
  (if (< atom 1)
      (error "bad atom number" atom)
    (x-atom-name dpy atom)))



;;; desc
(define (xkb-get-desc dpy)
  ;; xkb-get-keyboard <--- that was a bad idea
  (let1 desc (xkb-get-desc* dpy XkbAllMapComponentsMask XkbUseCoreKbd)
    ;;XkbAllComponentsMask
    (xkb-get-names desc XkbAllNamesMask)
    ;(xkb-get-geometry desc)
    desc))                                  ;XkbModifierMapMask (logior


(define (xkb-set-fork dpy code fork . rest)
  (let-optionals rest
      ((device XkbUseCoreKbd))
    (xfork-configure-key dpy device Xkb_configure_key_fork_repeat fork code)))



;(define (xfork-get-configure-key dpy what code)
;   (xfork-get-configure-key-twin  dpy what code 0))

(define (key-forkable? dpy device code)
  (not (zero? (xfork-get-configure-key dpy device
				       Xkb_configure_key_fork code))))



(define (keycode->string dpy keycode)
  (x-keysym-name
   (xkb-keycode-to-keysym dpy keycode 0 0)))


(provide "xlib")
