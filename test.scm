

(use xlib)

;; 
(define-values (dpy xkb-event-base)
  (receive (dpy event err major minor)
      (xkb-open-display ":0")
    (values dpy event)))

(x-extension-version dpy "XKB")
;; => 1 3

(x-list-input-devices dpy)


(xkb-select-events dpy XkbUseCoreKbd
		   (logior XkbActionMessageMask
			   XkbNewKeyboardNotifyMask
			   XkbStateNotifyMask)
		   (logior XkbActionMessageMask
			   XkbNewKeyboardNotifyMask
			   XkbStateNotifyMask))

;; <xkb-event>
;; <fork-event>
(define ev (x-next-event dpy))
ev
xkb-event-base
(x-event-type ev)
(define xev (x-event->xkb-event ev))

(x-events-queued dpy QueuedAlready)
(x-events-queued dpy QueuedAfterReading)

(xkb-event-type xev)
(xkb-event->xkb-message xev)

(define state (xkb-get-state dpy XkbUseCoreKbd))
(d state)


;; cycle and report modifiers & group
(do ((ev (x-next-event dpy)
	 (x-next-event dpy)))
    (#f #f)
  ;; fixme: must be xkb event!
  (let1 xev (x-event->xkb-event ev)
    (when (= XkbStateNotify (xkb-event-type xev))
      (xkb-apply-event-to-state xev state)
      (logformat "~d ~d\n"
	(ref state 'mods)
	(ref state 'group))
      )))
    
xkb-apply-event-to-state
