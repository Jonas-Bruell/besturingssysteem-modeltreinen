;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> gui/main.rkt <<<                            ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui
(require (prefix-in railway: "railway-tab.rkt")
         (prefix-in manual: "manual-tab.rkt")
         )
(provide start-gui ; (string -> object)
         )

(define FRAME_WIDTH 800)
(define FRAME_HEIGHT 1000)

(define test-callback
  (λ (var) (λ (t e) (println var))))

; callbacks-all-objects
; = '((name (label . callback) (label . callback) ...)) ...)
(define (make-switches-callback-list list-switch-ids list-callbacks)
  (let make-list ((list-switch-ids list-switch-ids)
                  (switch-callback-list '()))
    (if (null? list-switch-ids)
        switch-callback-list
        (let ((switch-id (car list-switch-ids)))
          (make-list
           (cdr list-switch-ids)
           (append switch-callback-list
                   (list (list switch-id
                               (cons "position 1"
                                     (λ (t e) ((vector-ref list-callbacks 0)
                                               switch-id)))
                               (cons "position 2"
                                     (λ (t e) ((vector-ref list-callbacks 1)
                                               switch-id)))))))))))
(define (make-trains-callback-list list-train-ids list-callbacks)
  (let make-list ((list-train-ids (car list-train-ids))
                  (train-callback-list '()))
    (if (null? list-train-ids)
        train-callback-list
        (let ((train-id (car list-train-ids)))
          (make-list
           '() ;bug! 
           (append train-callback-list
                   (list (list train-id
                               (cons "set test-train speed 20"
                                     (λ (t e) ((vector-ref list-callbacks 0)
                                               train-id)))
                               (cons "set test-train speed 0"
                                     (λ (t e) ((vector-ref list-callbacks 1)
                                               train-id)))
                               (cons "set test-train speed -20"
                                     (λ (t e) ((vector-ref list-callbacks 2)
                                               train-id)))))))))))
(define (make-crossings-callback-list list-crossing-ids list-callbacks)
  (let make-list ((list-crossing-ids list-crossing-ids)
                  (crossing-callback-list '()))
    (if (null? list-crossing-ids)
        crossing-callback-list
        (let ((crossing-id (car list-crossing-ids)))
          (make-list
           (cdr list-crossing-ids)
           (append crossing-callback-list
                   (list (list crossing-id
                               (cons "open crossing"
                                     (λ (t e) ((vector-ref list-callbacks 0)
                                               crossing-id)))
                               (cons "close crossing"
                                     (λ (t e) ((vector-ref list-callbacks 1)
                                               crossing-id)))))))))))
(define (make-lights-callback-list list-light-ids list-callbacks)
  (let make-list ((list-light-ids list-light-ids)
                  (light-callback-list '()))
    (if (null? list-light-ids)
        light-callback-list
        (let ((light-id (car list-light-ids)))
          (make-list
           (cdr list-light-ids)
           (append light-callback-list
                   (list (list light-id
                               (cons "red"
                                     (λ (t e) ((vector-ref list-callbacks 0)
                                               light-id)))
                               (cons "green"
                                     (λ (t e) ((vector-ref list-callbacks 1)
                                               light-id)))
                               (cons "red/white"
                                     (λ (t e) ((vector-ref list-callbacks 2)
                                               light-id)))
                               (cons "green/8"
                                     (λ (t e) ((vector-ref list-callbacks 3)
                                               light-id)))
                               (cons "orange/white"
                                     (λ (t e) ((vector-ref list-callbacks 4)
                                               light-id)))
                               (cons "orange/white/8"
                                     (λ (t e) ((vector-ref list-callbacks 5)
                                               light-id)))
                               (cons "white"
                                     (λ (t e) ((vector-ref list-callbacks 6)
                                               light-id)))
                               (cons "green/white/8/6"
                                     (λ (t e) ((vector-ref list-callbacks 7)
                                               light-id)))))))))))
  
(define gui%
  (class frame%
    (init-field title
                switches-callback-list
                trains-callback-list
                crossings-callback-list
                lights-callback-list)
    (super-new (label title) (width FRAME_WIDTH) (height FRAME_HEIGHT))

    (define tab-panels '())

    (define (change-tab tab-panel event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (let ((selection (send tab-panel get-selection)))
          ((case selection
             ((0) railway:fill-tab)
             ((1) manual:fill-tab)
             )
           tab-panels tab-panel (send tab-panel get-selection)))))
    
    (define tab-panel
      (new tab-panel% (parent this) (choices '()) (callback change-tab)))

    (define (append-tab! name tab)
      (set! tab-panels (append tab-panels (list tab)))
      (send tab-panel append name))
    
    (append-tab! "RAILWAY" (railway:make-tab tab-panel
                                             test-callback))
    (append-tab! "MANUAL" (manual:make-tab tab-panel
                                           switches-callback-list
                                           trains-callback-list
                                           crossings-callback-list
                                           lights-callback-list))
    ;resolving side-effect
    (railway:fill-tab tab-panels tab-panel 0)))

(define (start-gui title provider)
  (send
   (make-object gui%
     title
     (make-switches-callback-list
      (send provider get-switch-ids)
      (vector (λ (switch) (send provider set-switch-position! switch 1))
              (λ (switch) (send provider set-switch-position! switch 2))
              ))
     (make-trains-callback-list
      (send provider get-test-train-list)
      (vector (λ (train) (send provider set-train-speed! train 20))
              (λ (train) (send provider set-train-speed! train 0))
              (λ (train) (send provider set-train-speed! train -20))
              ))
     (make-crossings-callback-list
      (send provider get-crossing-ids)
      (vector (λ (crossing) (send provider set-crossing-state! crossing 'open))
              (λ (crossing) (send provider set-crossing-state! crossing 'close))
              ))
     (make-lights-callback-list
      (send provider get-light-ids)
      (vector (λ (light) (send provider set-light-signal! light 'Hp0))
              (λ (light) (send provider set-light-signal! light 'Hp1))
              (λ (light) (send provider set-light-signal! light 'Hp0+Sh0))
              (λ (light) (send provider set-light-signal! light 'Ks1+Zs3))
              (λ (light) (send provider set-light-signal! light 'Ks2))
              (λ (light) (send provider set-light-signal! light 'Ks2+Zs3))
              (λ (light) (send provider set-light-signal! light 'Sh1))
              (λ (light) (send provider set-light-signal! light 'Ks1+Zs3+Zs3v))
              )))
   show #t))