

;;; things about xkb desc which are not a catalogable task.

(define-module xlib.xkb
  (export
   xkb-get-virtual-modifier-list
   string->modifier                     ;should be name->modifier ??
   keycode-name
   ;; todo:  name->keycode
   )
  (use xlib)
  (use mmc.simple)
  (use srfi-1)
  )


(select-module xlib.xkb)

;; Getting the names/atoms? of virtual modifiers

(define (keycode-name desc keycode)
  (xkb-name-key (xkb-desc->names desc) keycode)) ;fixme! GC 

;; get the list of names of virtual modifiers:
(define (xkb-get-virtual-modifier-list desc)
  (xkb-get-names desc XkbAllNamesMask)
  (let ((mods '()))
    (for-numbers* i 0 (- XkbNumVirtualMods 1) ; why -2
      (let1 atom (xkb-vmod-name desc i)
        (unless (zero? atom)
          (push! mods (x-atom->name (slot-ref desc 'dpy) atom)))))
  (reverse mods)))


(define (string->modifier desc name)
  (let1 atom (string->x-atom (ref desc 'dpy) name #f)
    (find-in-numbers* i 0 (- XkbNumVirtualMods 1) ; why -2
      (= atom (xkb-vmod-name desc i)))))

(provide "xlib/xkb")
