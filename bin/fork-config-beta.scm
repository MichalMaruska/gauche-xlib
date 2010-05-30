#! /usr/bin/gosh

(use xlib.fork)
(use xlib.xkb.fork)
(use mmc.log)
(use macros.reverse)

(let1 fork (fork-connect)


  ;; Beta will like it!

;;; Make a new Escape. This will be switched w/ the meta. fixme: why not reuse escape??
  (for-each-reverse (fork:physical-meta-keys fork)
    ;;(logformat "forking _reverse_ physical meta (~a) to escape(s)\n" physical-meta-keys)
    (lambda (meta-keycode)
      (fork-push-to fork meta-keycode
		    '("ONE_LEVEL")
		    '(("Escape"))
		    0
		    ())))
;;;
  (logformat-color 'green "forking to hyper\n")
  (fork-keys-to fork (list "l" "f")
		'("ONE_LEVEL")
		'(("Hyper_L"))
		128
		'(((set-mod 1 128 128 0 0))))

  (logformat-color 'green  "forking to <SHIFT>\n")
  (fork-keys-to fork (list "space" "v" 47)	;
		'("ONE_LEVEL")
		'(("Shift_R"))
		1
		'(((set-mod 5 1 1 0 0))))


  (logformat-color 'green  "forking to <ALT>\n")
  (fork-keys-to fork (list "s")
		'("ONE_LEVEL")
		'(("Alt_R"))
		32
		'(((set-mod 5 32 32 0 0)))
		)

  (logformat-color 'green  "forking to <group-selectors>\n")
  (fork-keys-to fork (list "j" "a")
		'("ONE_LEVEL")
		'(("Mode_switch"))	;
		0
		'(((set-group 0 1))))


  (fork-keys-to fork (list "d" "k")
		'("ONE_LEVEL")
		'(("ISO_Group_Latch"))	;"ISO_Latch_Group" ISO_Level2_Shift
		0
		'(((set-group 0 2))))

  ;; ??? m to level3
  ;; bug! famous problem!
  (fork-to! (ref fork 'dpy) 58 49)

  (fork-commit fork)
  (sys-exit 0)
  )
