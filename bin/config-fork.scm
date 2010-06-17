#! /usr/bin/gosh

;;; Aim:
;;     define "forks": tell the "fork" plugin what keycodes to produce when a key forks
;;    (is pressed too much).

;; Formally: define a mapping F from keys to keycodes; possibly enlarge the domain of
;; keycodes by defining for the new keycode the associated keysyms.

;; How is the (mapping) F constructed?  There are some contraints, but let's see the base:

;;;  objects with which we work:
;;  =========
;;  keys         ... on your keyboard
;;  keycodes     ... sent from the X server to clients (in response of pressing keys)
;;  keysyms      ... interpretation of keycodes & "state"  by the (client-side) X lib

;; schema of where the mappings are used:
;; 
;; keyboard           Server                                                        Client
;; ---------------------------------------------------------------------------------------------------------
;; keys ------> medved translates to keycode (mapping K)                 /
;;        K      |             identity                                  /
;;               +-----> Fork -------->  pass through        ->          /
;;                        |   F                                          /             Xlib
;;                        +----->  fork   to F(keycode)  ->              /        keycode + state -> keysym
;;


;; Inverse mapping  K^-1 is given by the Geometry!

;;; constraints on the F and K mappings:
;; =================
;; mapping K: key -> keycode  must be injective: otherwise i could press the 2 keys, release one, and find myself in a strange situation:
;;    a key is down, yet the system doesn't know it.
;; 
;; F must maintain this injectivity, for the same reason. So if i want to fork a key "a" to "Shift", i must get a new keycode (number), and
;;  associate the semantics of Shift (keysyms & modifier bits) with that code. I call that process cloning.

;; Once again: why injectivity: If i press "a" and fork, then press "shift" and release "a" the Shift would be forgotten
;; (as the keycode was released).


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







(define dpy (x-open (sys-getenv "DISPLAY")))  ;(define dpy (x-open ":1"))

(define desc (xkb-get-desc dpy))

(xkb-get-controls dpy XkbAllControlsMask desc)

;; Here we update the autorepeat bits.
(define controls (xkb-desc->controls desc))


;;; The set of (valid) keycodes can be divided into 3 groups of keys: physical (image of the K mapping),
;;;   forked (image of the F mapping) and the rest(unused.)
;;; The (first 2) must be disjoint, i.e
;;; We should not fork to the same, or physical keycode. Otherwise, pressing >1 of them, and releasing not all of them
;;; is confusing.

;; so, at the beginning i should construct these sets...

(define geometry-mapping (xkb-create-inverse-mapping desc))



;; This keys 49 is not on the US layout of MS natural pro, but it is on european layout. As the xfree86 xkb files
;; have only the geomtry data of the US one, I need this workaround.

;; As a workaround for lacking of geomtry for MS natural pro, EU version,
;; I have to add:
(vector-set! geometry-mapping 49
             ;(section row column)
             '(1 1 1))




;; given a list/set of keycodes, return subset of those present on the GEOMETRY!
;; fixme: so, is this buggy for 94?
;; mmc: sort of `pre-image' of the key->keycode mapping!
(define (filter-geometry desc keycodes)
  ;; fixme: (let1 geometry-mapping (xkb-create-inverse-mapping desc)
  (filter
   (lambda (keycode)
     (vector-ref geometry-mapping keycode))
   keycodes))



;;; Pool of the 256 `keycodes'

;;; Observe this `mappings'
;;          
;; HW keys ---> keycodes
;;  |
;;  v
;; forks   ->  keycodes



;; get a list of keycodes, which are images of hw keys
(define physical-keys
  (filter-keycodes desc
    (lambda (keycode)
      (vector-ref geometry-mapping keycode))))
;; Another one (besides 49)!
(push! physical-keys 94)                ; <> on my european keyboards!
                                        ; XFree86 XKB geometry for MS keyboards is for US model



(define forked-keys
  (apply append
         (map				;this  queries the X server. should save on this! 
             (cut forks-to dpy <>)
           physical-keys)))


;;; pool of `unused' keycodes:
(define unused-keycodes
  (filter-keycodes desc
    (lambda (keycode)
      (not (or (member keycode forked-keys)
               (member keycode physical-keys))))))

(define (get-new-keycode)
  (if (null? unused-keycodes)
      (error "keycodes exhausted"))
  (let1 k (car unused-keycodes)
    (set! unused-keycodes (cdr unused-keycodes))
    k))

;;; =======================  forking configuration  itself ===================

;; I want to make all "keys" (not keycodes) fork from "Escape" keysym to Meta keysym/modifier
(define physical-meta-keys          ;physical  249!
  (let1 possible
      (filter-geometry
       desc
       (keysym+group+level->keycode desc "Meta_L" 0 0))

    ;; (51 61 93 94 116)
    (if (null? possible)
        (begin
          (logformat "WARNING: couldn't find Meta_L, using 64.\n *** And redefining it to proper Meta_L!\n")

          (set! possible
                                        ;(sys-exit 1)
                (list 64))
          (for-each-reverse possible
            (lambda (keycode)
              (define-keycode-keysyms desc keycode
                '("ONE_LEVEL")
                '(("Meta_L"))
                8)
              (xkb-control-keyrepeats-set! controls keycode #f)
              ;; not repeatable!
              ))))
    possible))


;; fixme:  switch off debugging!
(xfork-configure dpy fork_configure_debug 0)

(logformat "forking _reverse_ physical meta (~a) to escape(s)\n" physical-meta-keys)
;;; Make a new Escape. This will be switched w/ the meta. fixme: why not reuse escape??
(for-each-reverse physical-meta-keys
  (lambda (meta-keycode)
    (let1 new-keycode (get-new-keycode)
      (define-keycode-keysyms
        desc
        new-keycode
        '("ONE_LEVEL")
        '(("Escape"))
        0
        )

      (logformat "forking ~d -> ~d\n" new-keycode meta-keycode)
      (push-modifier-to-forked desc meta-keycode new-keycode)
      (xkb-control-keyrepeats-set! controls new-keycode #f))
    ;; dont repeat
    ))


(pump-x-events dpy)


;;; make the hyper under F key ... todo: find F via Geometry

(logformat-color 'green "forking to hyper\n")


;; specific to this file.
(define (fork-to-new-key keycode types keysyms mod actions)
  (let1 new-keycode (get-new-keycode)
    (logformat "new keycode ~d for keysym ~a \n" new-keycode (caar keysyms))

    ; check (= (lenght actions) (lenght keysyms)))  (lenght types)
    (define-keycode-keysyms desc new-keycode types keysyms mod)
    (define-keycode-actions desc new-keycode types actions)
    ; dont repeat
    (xkb-control-keyrepeats-set! controls new-keycode #f)
    (xkb-change-keycode desc new-keycode)
    (fork-to! dpy
              keycode
              new-keycode)))


(for-each-reverse (list "l" "f")
    (lambda (keysym)
      (fork-to-new-key
       (keysym->keycode desc keysym)    ; must be 1 just key?
       '("ONE_LEVEL")
       '(("Hyper_L"))
       128
       '(((set-mod 1 128 128 0 0))))))


(logformat-color 'green  "forking to <SHIFT>\n")
;;; shift on space!
;; find the Shift modifier!!!
;; 1x1 key w/ Shift_L/R + modmap

(for-each-reverse (list "space" "v" 47) ;fixme!
  (lambda (keysym)
      (fork-to-new-key
       (if (number? keysym)
           keysym
         (keysym->keycode desc keysym))
       '("ONE_LEVEL")
       '(("Shift_R"))
       1
       '(((set-mod 5 1 1 0 0))))))



(logformat-color 'green  "forking to <ALT>\n")

(for-each-reverse (list "s")
  (lambda (keysym)
    (fork-to-new-key
     (keysym->keycode desc keysym)
     '("ONE_LEVEL")
     '(("Alt_R"))
     32
     '(((set-mod 5 32 32 0 0))); 5 flags (+ XkbSA_UseModMapMods XkbSA_ClearLocks)
     ; 32  mask
     )))


;;; Group selectors!!
;; find the key CPSL ?
;; i want D and A to switch group 1:


(logformat-color 'green  "forking to <group-selectors>\n")

; 66
(for-each-reverse (list "d" "j")
  (lambda (keysym)
    (fork-to-new-key
     (keysym->keycode desc keysym)

     '("ONE_LEVEL")
     '(("Mode_switch")) ;
     0
     '(((set-group 0 1))); XkbSA_GroupAbsolute
     )))


(for-each-reverse (list "k" "a")
  (lambda (keysym)
    (fork-to-new-key
     (keysym->keycode desc keysym)

     '("ONE_LEVEL")
     '(("ISO_Group_Latch")) ;"ISO_Latch_Group" ISO_Level2_Shift
     0
     '(((set-group 0 2))); XkbSA_GroupAbsolute
     )))


;; m -> group3
(fork-to! dpy 58 49)


(xkb-set-controls dpy XkbPerKeyRepeatMask desc)

(xkb-update-map dpy desc)
(x-flush dpy)
(x-close-display dpy)
(sys-exit 0)
