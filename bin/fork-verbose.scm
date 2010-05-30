#! /usr/bin/gosh

;; toggle the TRACING (debuggin) in the FORK plugin


(use xlib.fork)
(use xlib.xkb.fork)
(use mmc.log)
(use macros.reverse)



(define (main args)
  (let ((param (and (not (null? (cdr args)))
		    (cadr args))))
    (if (not param)
	(logformat "usage: ~a [0|1]\n" *program-name*)
      (let1 fork (fork-connect)
	(if (string=? "1" param)
	    (xfork:debug (ref fork 'dpy) #t)
	  ;(else
	  (xfork:debug (ref fork 'dpy) #t))
	(fork-commit fork)))
    (sys-exit 0)))
