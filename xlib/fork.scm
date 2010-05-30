(define-module xlib.fork
  (export
   fork-connect
   fork-push-to
   fork-keys-to
   fork-commit
   
   fork:physical-meta-keys
   )
  (use xlib)
  (use xlib.handy)

  (use mmc.simple)
  (use mmc.log)
  (use macros.reverse)
  (use srfi-1)
  (use adt.list)


  (use xlib.xkb)
  (use xlib.xkb.cmap)
  (use xlib.xkb.fork)
  (use xlib.xkb.find)

  (use xlib.xkb.geometry)
  (use xlib.xkb.server)
  )
(select-module xlib.fork)


;; given a list/set of keycodes, return subset of those present on the GEOMETRY!
;; fixme: so, is this buggy for 94?
;; mmc: sort of `pre-image' of the key->keycode mapping!
(define (filter-geometry fork keycodes)
  ;; fixme: (let1 geometry-mapping (xkb-create-inverse-mapping desc)
  (filter
      (lambda (keycode)
	(vector-ref (ref fork 'geometry-mapping) keycode))
    keycodes))





;; we keep a `state'. and have some accesses.

;; `state' is `desc' with

(define-class <xfork-state> ()
  ;; 
  ((dpy :init-keyword :dpy)
   (device :init-keyword :device)
   (desc :init-keyword :desc)
   ;; Here we update the autorepeat bits.
   (controls :init-keyword :controls)

   ;; mmc: Useful?
   (geometry-mapping :init-keyword :geometry-mapping)
   ;; Groups
   (physical-keys)
   (forked-keys)
   (unused-keycodes)
   ))

;;; Prepare to monitor & update forking stuff in the X server.
;;; 
(define (fork-connect . rest)	;todo! let-optionals
  (let-optionals* rest
      ((display #f)
       (debug #f)
       (device XkbUseCoreKbd))

    (unless display
      (set! display (sys-getenv "DISPLAY")))
(logformat "setting debug on ~d to ~a\n" device debug)
    (let* ((dpy (x-open
		 display))
	   (desc (xkb-get-desc dpy)))
      (xkb-get-controls dpy XkbAllControlsMask desc)
      (let* ((controls (xkb-desc->controls desc))
	     (fork (make <xfork-state>
		     :dpy dpy
		     :device device
		     :desc desc
		     :controls controls))
	     (geometry-mapping (xkb-create-inverse-mapping desc)))
	(vector-set! geometry-mapping 49
					;(section row column)
		     '(1 1 1))
	(slot-set! fork 'geometry-mapping geometry-mapping)

	(logformat "setting debug on ~d to ~a\n" device debug)
	;; Find 
	(slot-set! fork 'physical-keys
	  (filter-keycodes desc
	    (lambda (keycode)
	      (vector-ref geometry-mapping keycode))))
	;; fixme: i'm lazy to touch xkb desc files. Much easier a fix:
	;; Another one (besides 49)!
	(push! (ref fork 'physical-keys) 94)

	;;
	(logformat "setting debug on ~d to ~a\n" device debug)
	(xfork:debug (ref fork 'dpy) (ref fork 'device) debug)
	(slot-set! fork 'forked-keys
	  (apply append
		 (map			;this  queries the X server. should save on this! todo!
		     (cute forks-to
			   (ref fork 'dpy)
			   (ref fork 'device)
			  <>)
		   (slot-ref fork 'physical-keys))))
	(slot-set! fork 'unused-keycodes
	  (filter-keycodes desc
	    (lambda (keycode)
	      (not (or (member keycode
			       (slot-ref fork 'forked-keys))
		       (member keycode
			       ;; really:
			       (slot-ref fork 'physical-keys)))))))
	fork))))


;;; 
(define (get-new-keycode fork)
  (let1 unused-keycodes (slot-ref fork 'unused-keycodes)
    (if (null? unused-keycodes)
	(error "keycodes exhausted"))
    ;; begin0
    (let1 k (car unused-keycodes)
      ;; todo: pop!
      (slot-set! fork 'unused-keycodes (cdr unused-keycodes))
      k)))

;;;
;; I want to make all "keys" (not keycodes) fork from "Escape" keysym to Meta keysym/modifier
;; inits some of them!!! fixme!
(define (fork:physical-meta-keys fork)	; physical  249!
  (let* ((desc (ref fork 'desc))
	 (possible
	  (filter-geometry fork
			   (keysym+group+level->keycode desc "Meta_L" 0 0))))
    ;; (51 61 93 94 116)
    (when (null? possible)
      (logformat "WARNING: couldn't find Meta_L, using 64.\n *** And redefining it to proper Meta_L!\n")
      (set! possible
                                        ;(sys-exit 1)
	    (list 64))
      (for-each-reverse possible
	(lambda (keycode)
	  (define-keycode-keysyms (ref fork 'desc) keycode
	    '("ONE_LEVEL")
	    '(("Meta_L"))
	    8)
	  (xkb-control-keyrepeats-set! (ref fork 'controls) keycode #f)
	  ;; not repeatable!
	  )))
    possible))


;; specific to this file.
(define (fork-to-new-key fork keycode types keysyms mod actions)
  (let ((desc (ref fork 'desc))
	(controls (ref fork 'controls)))
    (let1 new-keycode (get-new-keycode fork)
      (logformat "new keycode ~d for keysym ~a \n" new-keycode (caar keysyms))

					; check (= (lenght actions) (lenght keysyms)))  (lenght types)
      (define-keycode-keysyms desc new-keycode types keysyms mod)
      (define-keycode-actions desc new-keycode types actions)
      ;; fixme: dont repeat ... this should be `optional'!
      (xkb-control-keyrepeats-set! controls new-keycode #f)
      (xkb-change-keycode desc new-keycode)
      (fork-to! (ref fork 'dpy)
		(ref fork 'device)
		keycode
		new-keycode))))


;; (logformat "forking _reverse_ physical meta (~a) to escape(s)\n" physical-meta-keys)
(define (fork-push-to fork keycode types keysyms mod actions)
  (let ((desc (ref fork 'desc))
	(controls (ref fork 'controls)))
    (let1 new-keycode (get-new-keycode fork)
      (define-keycode-keysyms desc new-keycode types keysyms mod)

      (logformat "forking ~d -> ~d\n" new-keycode keycode)
      (push-modifier-to-forked desc (ref fork 'device)
			       keycode new-keycode)
      ;; fixme: dont repeat
      (xkb-control-keyrepeats-set! controls new-keycode #f))))


;; fixme: ??
(define (fork-keys-to fork keys . description)
  (for-each-reverse keys 
    (lambda (keysym)
      (apply fork-to-new-key fork
	     (if (number? keysym)
		 keysym
	       (keysym->keycode (ref fork 'desc) keysym))
	     ;;(keysym->keycode desc keysym)
	     description))))

(define (fork-commit fork)
  (let ((dpy (ref fork 'dpy))
	(desc (ref fork 'desc)))
    (xkb-set-controls dpy XkbPerKeyRepeatMask desc)
    (x-flush dpy)
    (x-close-display dpy)
    ;; todo: invalidate the fork itself!
    ))

(provide "xlib/fork")
