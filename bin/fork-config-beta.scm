#! /usr/bin/gosh

(use xlib.fork)
(use xlib.xkb.fork)
(use mmc.log)
(use macros.reverse)

(define (main args)
  (let ((device (string->number (cadr args)))
	)
    (logformat "device ~d\n" device)
    (let1 fork (fork-connect #f #t device)
      (xfork:debug (ref fork 'dpy)
		   (ref fork 'device) #t)

      ;;(logformat "forking _reverse_ physical meta (~a) to escape(s)\n" physical-meta-keys)
      ;; Make a new Escape. This will be switched w/ the meta. 
      ;; fixme: why not reuse escape?? ... because having 2 hw keys on the same keyboard combined to 1
      ;; keycode is wrong!
      (for-each-reverse (fork:physical-meta-keys fork)
	;;(logformat "forking _reverse_ physical meta (~a) to escape(s)\n" physical-meta-keys)
	(lambda (meta-keycode)
	  (fork-push-to fork meta-keycode
			'("ONE_LEVEL")
			'(("Escape"))
			0
			'((no-action)))))

      ;; we hope 128 is the hyper modifier!
      (logformat-color 'green "forking to hyper\n")
      (fork-keys-to fork (list "l" "f")
		    '("ONE_LEVEL")
		    '(("Hyper_L"))
		    128
		    '(((set-mod 1 128 128 0 0))))

      (logformat-color 'green  "forking to <SHIFT>\n")
      (fork-keys-to fork (list "space" "v" 47)
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
		    '(("ISO_Group_Latch")) ;"ISO_Latch_Group" ISO_Level2_Shift
		    0
		    '(((set-group 0 2))))

      (fork-keys-to fork (list "m" "c")
		    ;; No modifier changes the function:
		    '("ONE_LEVEL")
		    '(("ISO_Level3_Shift")) ;correct!
		    0
		    ;; fixme: this is about group 4
		    '(((set-group 0 3))))

					;(or (xkb-set-explicit! desc keycode XkbAllExplicitMask)
					;XkbExplicitInterpretMask
					;    (error "set-explicit failed"))


      ;; 
      ;; (fork-to! (ref fork 'dpy) 58 49)
      (fork-commit fork)
      (sys-exit 0)
      )))

