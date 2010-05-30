
;;; data-mining the Geometry part of XKB configuration

(define-module xlib.xkb.geometry
  (export
   ;; access & look-up
   xkb-geom->sections  xkb-section-by-name
   xkb-section->rows
   xkb-row->keys
   xkb-key-at

   ;; reverse mapping
   xkb-iterate-keys
   xkb-name->keycode-hash
   xkb-create-inverse-mapping ;;;  keycode -> position   (section row column)
   

   ;; misc
   xkb-unallocated-keycodes
   )
  (use xlib)
  (use mmc.log)
  (use gauche.sequence)                 ;why?
  (use mmc.simple)
  )
(select-module xlib.xkb.geometry)

(define (xkb-geom->sections geom)
  ;;(ref xkb-geom 'sections)
  (map-numbers* i 0 (ref geom 'num-sections)
    (xkb-geometry-nth-section geom i)))

(define (xkb-section-by-name desc name)
  (let* ((geometry (ref desc 'geometry))
         (dpy  (ref desc 'dpy))
         (sections (xkb-geom->sections geometry)))
    (find
        (lambda (section)
          (string=? name
                    (x-atom-name
                     dpy
                     (ref section 'name))))
      sections)))

(define (xkb-section->rows section)
  ;;(ref xkb-geom 'rows)
  (map-numbers* i 0 (ref section 'num-rows)
    (xkb-geometry-nth-row section i)))


(define (xkb-row->keys row)
  ;;(ref xkb-geom 'rows)
  (map-numbers* i 0 (ref row 'num-keys)
    (xkb-geometry-nth-key row i)))


(define (xkb-key-at desc section-name row key)
  (let1 section (xkb-section-by-name desc section-name)
    (xkb-geometry-nth-key
     (xkb-geometry-nth-row section (if (< row 0)
                                      row row))
     (if (< key 0)
         key key))))


;; call FUNCTION for each section/row/key 
(define (xkb-iterate-keys geometry function)
  (for-each 
      (lambda (section)
        (for-each 
            (lambda (row)
              (for-each 
                  (lambda (key)
                    (function section row key))
                (xkb-row->keys row)))
          (xkb-section->rows section)))
    (xkb-geom->sections geometry)))


;; should go into `xlib.xkb.names'
;; prepare for fast lookup:
(define (xkb-name->keycode-hash desc)
  (let ((names (xkb-desc->names desc))
        (name->keycode (make-hash-table 'string=?)))
    ;(for-each-with-index
    (for-numbers* keycode (ref desc 'min-key-code) (ref desc 'max-key-code)
      (hash-table-put! name->keycode (xkb-name-key names keycode)
                       keycode))
    name->keycode))


;; position -> keycode
;; Returns a vector:  v[keycode] = (section row column)
(define (xkb-create-inverse-mapping desc)
  (let* ((dpy (ref desc 'dpy))
         (geom (ref desc 'geometry))
         (max (ref desc 'max-key-code))
         (name->keycode (xkb-name->keycode-hash desc))
         
         (vector (make-vector max       ;(- max min)
                              #f)))
    (for-each
        (lambda (section)
          (let1 section-name (x-atom-name dpy (ref section 'name))
            ;;(xkb-section->rows section))))
            ;(logformat "~d\n" (ref section 'num-rows))
            (for-numbers<* i 0 (ref section 'num-rows) ;fixme:  avoid reverse !!
              (let1 row (xkb-geometry-nth-row section i)
                ;;(xkb-row->keys row)
                ;(logformat "in  row ~d: ~d keys\n" i (ref row 'num-keys))
                (map-numbers* j 0 (ref row 'num-keys)
                  ;;
                  ;(logformat "key at position: ~d, ~d\n" i j)
                  (let* ((key (xkb-geometry-nth-key row j)) ;(ref (xkb-geometry-nth-key row i) 'name)
                         (name (ref key 'name)))
                    (when (hash-table-exists? name->keycode name)
                      (vector-set! vector
                                   (hash-table-get name->keycode name)
                                   (list section row key)))
                    '(logformat  "section ~a row ~d ~d key ~a: (~d)\n"
                                section-name i j
                                name
                                (if (hash-table-exists? name->keycode name)
                                    (hash-table-get name->keycode name)
                                  "-1"))))))))
      (xkb-geom->sections geom))
    vector))

;; return  a `bitarray' with #t for keycodes which are found in the geometry.
;; #f -> not found.
(define (xkb-unallocated-keycodes desc)
  (let ((vector (make-vector (ref desc 'max-key-code) #f))
        (hash (xkb-name->keycode-hash desc)))
    ;; go through the geometry....
    (xkb-iterate-keys
     (ref desc 'geometry)
     (lambda (section row key)
       ;(logformat "visiting ~a\n" (ref key 'name))
       (if (hash-table-exists? hash (ref key 'name))
           (vector-set! vector
                        (hash-table-get hash (ref key 'name))
                        #t))))
    vector))



(provide "xlib/xkb/geometry")
