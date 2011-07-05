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
(define (filter-geometry geometry keycodes)
  ;; fixme: (let1 geometry-mapping (xkb-create-inverse-mapping desc)
  (filter
      (lambda (keycode)
	(vector-ref geometry keycode))
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
(define (fork-connect . rest)	;todo! let-keywords
  (let-optionals* rest
      ((display #f)
       (debug #f)
       (device XkbUseCoreKbd))

    (unless display
      (set! display (sys-getenv "DISPLAY")))
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

	;; Find
	(slot-set! fork 'physical-keys
	  (filter-keycodes desc
	    (lambda (keycode)
	      (vector-ref geometry-mapping keycode))))
	;; fixme: i'm lazy to touch xkb desc files. Much easier a fix:
	;; Another one (besides 49)!
	(push! (ref fork 'physical-keys) 94)

	;;(logformat "setting debug on ~d to ~a\n" device debug)
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
(define (reset-keycodes-to-meta desc controls keycodes)
  (let1 meta-modifier 8
    (for-each-reverse keycodes
      (lambda (keycode)
	(define-keycode-keysyms desc keycode
	  '("ONE_LEVEL")
	  '(("Meta_L"))
	  meta-modifier)
	(xkb-control-keyrepeats-set! controls keycode #f)
	;; not repeatable!
	))))

;; find all keycodes which are defined as Meta_L. If none is found, we
;; suppose my configuration (fixme!) has been applied, and key 64 should be returned
;; to be such one.
(define (fork:physical-meta-keys fork)	; physical  249!
  (let* ((desc (ref fork 'desc))
	 (possible
	  (filter-geometry (ref fork 'geometry-mapping)
			   (keysym+group+level->keycode desc "Meta_L" 0 0))))
    ;; (51 61 93 94 116)
    (when (null? possible)
      (logformat "WARNING: couldn't find Meta_L, using 64. And redefining it to proper Meta_L!\n")
      (set! possible (list 64))
      (reset-keycodes-to-meta (ref fork 'desc) (ref fork 'controls) possible))
    possible))


;; not API.
;; define
(define XkbRepeatKeysMask (ash 1 30))

;;
(define (redefine-keycode desc keycode type-names keysyms mod actions)
  (logformat "redefining keycode ~d for keysym ~a \n" keycode keysyms)
  ; (xkb-set-key-types desc keysyms type-names)
  ;; xkb-change-types-of-keys desc keycode type-indices)
  ;; check (= (lenght actions) (lenght keysyms)))  (lenght types)
  (define-keycode-keysyms desc keycode type-names keysyms mod)
  (define-keycode-actions desc keycode type-names actions)
  (xkb-change-keycode desc keycode))


;; This is an inverse fork:
;; the original meaning is only when forked,
;; otherwise, a new keycode is emitted (I.e. the original keycode is redefined)
(define (fork-push-to fork keycode types keysyms mod actions)
  (let ((desc (ref fork 'desc))
	(controls (ref fork 'controls)))
    (let1 new-keycode (get-new-keycode fork)
      (redefine-keycode desc new-keycode types keysyms mod actions)

      (logformat "forking ~d -> ~d\n" new-keycode keycode)
      (push-keycode-to-forked desc (ref fork 'device)
			      keycode new-keycode)
      ;; note: we apply the non-AR to the original key
      (xkb-control-keyrepeats-set! controls new-keycode #f))))


;; fork KEYCODE to a new keycode, whose definition is
;; (TYPES KEYSYMS MOD ACTIONS)
(define (fork-to-new-key fork keycode types keysyms mod actions)
  (let ((desc (ref fork 'desc))
	(controls (ref fork 'controls)))
    (let1 new-keycode (get-new-keycode fork)
      (redefine-keycode desc new-keycode types keysyms mod actions)
        ;; fixme: dont repeat ... this should be `optional'!
      (xkb-control-keyrepeats-set! controls new-keycode #f)
      ;(xkb-set-controls (ref fork 'dpy) XkbRepeatKeysMask desc)
      (fork-to! (ref fork 'dpy)
		(ref fork 'device)
		keycode
		new-keycode))))

;; apply fork-to-new-key to all KEYS ;; mmc! hilight
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
    (logformat "~d ~d\n" XkbPerKeyRepeatMask (ash 1 30))
    (x-flush dpy)
    (x-close-display dpy)
    ;; todo: invalidate the fork itself!
    ))

(provide "xlib/fork")
