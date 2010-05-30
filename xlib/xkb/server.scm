
;; the server map.
(define-module xlib.xkb.server
  (export
   action-matcher
   ;action->list
   decode-action
   encode-action
   ;list->action

   xkb-dump-actions
   define-keycode-actions
   )
  (use srfi-1)
  (use xlib)
  (use xlib.xkb.cmap)
  (use mmc.simple)
  (use mmc.log)
  (use srfi-1)
  (use adt.alist)
  )
(select-module xlib.xkb.server)

;;; C provides actions as a list of 8 small integers
;;; We in scheme handle symbolic lists

;;; 
(define (action-matcher model-action);  type . rest
  (lambda (action)
    (equal? action model-action))); list=

; (cond 
;    ((eq? type <xkb-group-action>)
;     (let ((flags (car rest))
;           (group (cadr rest)))
;       (lambda (action)
;         (and (is-a? action <xkb-group-action>)
;              (= (ref action 'group_XXX) group)
;              (= (ref action 'flags) flags)))))
;    (else
;     #t)))




(define
  xkb-action-type->name
  `(;(0  no-action 0)
   (,XkbSA_SetMods  set-mod 5 ); 
   (,XkbSA_LatchMods  latch-mod 5)
   (,XkbSA_LockMods  lock-mod 5)
   ;;
   (,XkbSA_SetGroup  set-group 2)
   (,XkbSA_LatchGroup  latch-group 2)
   (,XkbSA_LockGroup  lock-group 2)

   (,XkbSA_MovePtr  move-ptr 6)
   (,XkbSA_PtrBtn press-button 3)
   (,XkbSA_LockPtrBtn  lock-button 3)
   (,XkbSA_SetPtrDflt  set-button 3)

   (,XkbSA_ISOLock iso-lock 7)                      ;
   ;; terminate
   ;; XkbSA_Terminate
   ;; XkbSA_SwitchScreen

   ;; XkbSA_SetControls
   (,XkbSA_LockControls lock-controls 5)
   
   (,XkbSA_ActionMessage message 7)   ;16?
   (,XkbSA_RedirectKey redirect 7)
   (,XkbSA_DeviceBtn device 4)
   ;XkbSA_LockDeviceBtn
   ;XkbSA_DeviceValuator
   ))

;;; symbolic -> C (8-list of small ints)
(define (decode-action action)
  (cond
   ((eq? action 'no-action)
    (make-list 8 0))
   ((eq? (car action) 'message)
    (cons*
     XkbSA_ActionMessage
     (second action)
     (map
         char->integer
       (string->list (third action)))))
   (else  
    (let1 info (find
                (lambda (info)
                  (eq? (second info) (car action)))
                xkb-action-type->name)
      ;; no-action
      (if info
          (append
           (cons (car info) '())
           (take (cdr action) (third info))
           (make-list (- 7 (third info)) 0))
        action)))))

;; action is given as 4-list of numbers  and returned is symbolic list.
(define (encode-action action)
  (receive (type arguments) (car+cdr action)
    (if (= type XkbSA_ActionMessage)
        (list 'message (second action)
              (apply string-append
                     (map (lambda (i)
                         (make-string 1 (integer->char i)))
                    (cddr action))))
      (let1 info (aget xkb-action-type->name (car action))
        (if info
            (cons (car info)
                  (take arguments (second info)))
                                        ;(let1 type (ref action 'type)   1
          (cond
           ((list= = action (make-list 8 0))
            'no-action)
           (                        ;(is-a? action <xkb-group-action>)
            (memv (car action) (list XkbSA_SetGroup XkbSA_LatchGroup XkbSA_LockGroup))
            (list                       ;'group
             (aget xkb-action-type->name (first action))
             (take  (cdr action) 3)
                                        ;(second action)
                                        ;(third action); (ref action 'group_XXX)
                                        ;(ref action 'flags)
                                        ;(fourth action)
             ))
           (                          ;(is-a? action <xkb-mod-action>)
            (memv (car action) (list XkbSA_SetMods XkbSA_LatchMods XkbSA_LockMods))
            (cons                       ;'mod 
             (aget xkb-action-type->name (first action))
             (take (cdr action) 5)
                                        ;(second action); (ref action 'flags)
                                        ; (third action) ;(ref action 'mask)
                                        ;           (fourth action) ;(ref action 'real_mods)
                                        ;           (fifth action) ;(ref action 'vmods1)
                                        ;           (sixth action);(ref action 'vmods2)
             ))
           ((list? action)              ; pair?
            (case (car action)
              ((7)                      ;XkbSA_MovePtr
               (cons 'move-ptr
                     (take arguments 6)))
              ((9)                      ;XkbSA_LockPtrBtn
               (cons 'lock-button
                     (take arguments 3)))
              ((10)                     ;XkbSA_SetPtrDflt
               (cons 'set-button
                     (take arguments 3)))
              ((8)                      ;XkbSA_PtrBtn
               (cons 'press-button
                     (take arguments 3)))
              (else
               action)))
           (else
            'no-action
                                        ;(error "unknown action" action)
            )))))))


;; todo: dump the virtual modifier bound???
;; i need some info from the client map too.
(define (xkb-dump-actions desc keycode)
  (let* ((node (xkb-keycode->node desc keycode))
         (client-map (ref desc 'map))
         ;;
         (types (map-numbers* group 0 (ref node 'groups)
                  (xkb-client-map->type client-map (xkb-key-type desc keycode group))))
         ;; these are objects/types
         (type-names (map (lambda (t)
                            (x-atom-name (ref desc 'dpy) (ref t 'name)))
                       types))
         (width (ref node 'width))
         
         (action-matrix
          (reverse
           (third
            (fold
             (lambda (type seed)
                                        ;(logformat "another step\n")
               (receive (offset type-index matrix) (apply values seed)
                                        ;(logformat "another step:\n")
                 (list
                  (+ width offset)
                  (+ 1 type-index)
                  (cons
                   ;; levels -> keysyms
                   (map-numbers* level 0 (slot-ref type 'num-levels)
                     (encode-action
                      (xkb-key-action-entry desc keycode type-index level)))
                   matrix))))
             (list (ref node 'offset) 0 '())
             types)))))
    ;; types
    ;; keysyms
    (values type-names action-matrix)))



(define (define-keycode-actions desc keycode types actions)
  ;; check the types ...
  (let* ((node (xkb-keycode->node desc keycode))
         (width (ref node 'width)))
    ;; fixme!
    (for-numbers* group 0 (- (length types) 1)
      (let1 action-row (list-ref actions group)
        (for-numbers* level 0 (- (length action-row) 1)
          (let1 decoded (decode-action (list-ref action-row level))
	    (unless (xkb-key-has-actions desc keycode)
	      (logformat "this is nonsense! ~d\n" keycode))
	    (logformat "calling xkb-key-set-action: ~d gr:~d lev:~d (~d)\n" keycode group level
		       (xkb-key-num-actions desc keycode))
            (xkb-key-set-action desc keycode (+ (* group width) level) decoded)
	    ))))
    (xkb-change-keycode desc keycode)))

(provide "xlib/xkb/server")
