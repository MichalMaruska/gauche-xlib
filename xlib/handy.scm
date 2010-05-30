(define-module xlib.handy
  (export
   pump-x-events
   )
  (use xlib)
  )
(select-module xlib.handy)


;; hack to see X errors.
(define (pump-x-events dpy)
  ;; I have to read event to get errors!!
  (unless (zero? (x-events-queued dpy QueuedAfterFlush))
    (do ()
        ((zero? (x-events-queued dpy
                                 QueuedAlready))
         #t)
      (x-next-event dpy))))


(provide "xlib/handy")