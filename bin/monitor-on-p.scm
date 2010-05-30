#! /usr/bin/gosh

;; report the DPMS params on the current DISPLAY

(use xlib)
(use mmc.log)

(define (main args)
  (define dpy (x-open))
  (dpms-query-extension dpy)
  (receive (major minor) (dpms-get-version dpy)
    (logformat "DPMS extension is version: ~d/~d\n" major minor))
  (receive (power state) (dpms-info dpy)
    (logformat "power: ~d state ~d\n" power state))

  (receive (standby suspend off) (dpms-get-timeouts dpy)
    (logformat "standby: ~d suspend ~d off ~d\n" standby suspend off))
  0
  )
