#! /usr/bin/gosh

(use xlib)
(use mmc.log)



(define-values (dpy xkb-event-base)
  (receive (dpy event err major minor)
      (xkb-open-display (sys-getenv "DISPLAY"))
    (values dpy event)))

(xkb-select-events dpy XkbUseCoreKbd
		   (logior XkbActionMessageMask
			   XkbNewKeyboardNotifyMask
			   XkbStateNotifyMask)
		   (logior XkbActionMessageMask
			   XkbNewKeyboardNotifyMask
			   XkbStateNotifyMask))


;; cycle and report modifiers & group
(let1 state (xkb-get-state dpy XkbUseCoreKbd)
  (do ((ev (x-next-event dpy)
	   (x-next-event dpy)))
      (#f #f)
    ;; fixme: must be xkb event!
    (if (= (x-event-type ev)
	   xkb-event-base)
	(let1 xev (x-event->xkb-event ev)
	  (when (= XkbStateNotify (xkb-event-type xev))
	    (xkb-apply-event-to-state xev state)
	    (logformat "~d ~d (gr)\n"
	      (ref state 'mods)
	      (ref state 'group))
	    ))
      (logformat "non XKB event\n")
      )))


