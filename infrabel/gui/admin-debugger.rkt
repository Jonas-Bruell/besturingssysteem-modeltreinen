;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> gui/main.rkt <<<                            ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require "../../config.rkt"
         "elements.rkt")
(provide make-adm&dbg)


(define (make-adm&dbg sim system)
  (make-object gui% system))

(define gui%
  (class frame%
    (init-field infrabel ;; terug abstraheren naar callbacks!!! ################
                ;switches-callback-list
                ;trains-callback-list
                ;crossings-callback-list
                ;lights-callback-list
                )
    
    (super-new
     (label "Admin panel and Debugger")
     (width SERVER_A&D_WIDTH)
     (height SERVER_A&D_HEIGHT))

    ;; UPDATE :: append callbacks to list for automatic updating
    (define callbacks-list '())
    (define/public (update!)
      (for-each (λ (callback)
                  (send (car callback) set-label ((cdr callback))))
                callbacks-list))

    ;; defining and filling of panels
    (let* ((upper-columns
            (new horizontal-pane% (parent this)))

           (left-column
            (new vertical-pane% (parent upper-columns)))
           (right-column
            (new vertical-pane% (parent upper-columns)))

           (switches-group
            (new horizontal-group-box-panel%
                 (label "SWITCHES")
                 (parent left-column)
                 (min-width (/ SERVER_A&D_WIDTH 2))))
           (switches-panel-L
            (new vertical-panel% (parent switches-group)))
           (switches-panel-R
            (new vertical-panel% (parent switches-group)))
           
           (emergency-panel
            (new group-box-panel%
                 (label "EMERGENCY")
                 (parent right-column)
                 (min-width (/ SERVER_A&D_WIDTH 2))))

           (switches*-panel
            (new group-box-panel% (label "SPECIAL SWITCHES")
                 (parent right-column)))
           (crossings-panel
            (new group-box-panel% (label "CROSSINGS") (parent right-column)))
           (lights-panel
            (new group-box-panel% (label "LIGHTS") (parent right-column)))

           
           (blocks-group
            (new group-box-panel% (label "SECTION BLOCKS") (parent this)))
           (blocks-panel-1
            (new horizontal-panel% (parent blocks-group)))
           (blocks-panel-2
            (new horizontal-panel% (parent blocks-group)))
           (blocks-panel-U
            (new horizontal-panel% (parent blocks-group)))
           (trains-panel
            (new group-box-panel% (label "TRAINS") (parent this)))
           

           )

      
      
      (define (fill-panel panel callbacks-all-objects)
        ; callbacks-all-objects
        ; = '((name (label . callback) (label . callback) ...)) ...)
        (let fill ((callbacks-all-objects callbacks-all-objects))
          (let ((local-panel (new horizontal-panel% (parent panel)))
                (callbacks-this-object (car callbacks-all-objects)))
            (new message%
                 (label (symbol->string (car callbacks-this-object)))
                 (parent local-panel))
            (let make-buttons ((callbacks (cdr callbacks-this-object)))
              (new button%
                   (label (caar callbacks))
                   (parent local-panel)
                   (callback (cdar callbacks)))
              (unless (null? (cdr callbacks))
                (make-buttons (cdr callbacks))))
            (unless (null? (cdr callbacks-all-objects))
              (fill (cdr callbacks-all-objects))))))

      
      ; filling panels with messages and buttons
      (let fill ((switch (map (λ (l)
                                (list l
                                      (λ () (send infrabel
                                                  get-switch-position
                                                  (car l)))
                                      (λ (t e) (send infrabel
                                                     set-switch-position!
                                                     (car l) 1))
                                      (λ (t e) (send infrabel
                                                     set-switch-position!
                                                     (car l) 2))
                                      ))
                              (cdr (assoc 'switch (send infrabel get-track)))))
                 (index 0))
        (new switch-panel%
             (parent (if (< index 8)
                         switches-panel-L
                         switches-panel-R))
             (name (symbol->string (caar (car switch))))
             (in (symbol->string (cadar (car switch))))
             (check (cadr (car switch)))
             (button1 (caddr (car switch)))
             (button2 (cadddr (car switch)))
             (out1 (symbol->string (caddr (caar switch))))
             (out2 (symbol->string (cadddr (caar switch)))))
        
        (unless (null? (cdr switch))
          (fill (cdr switch) (+ index 1))))

      (let ((switch (map (λ (l)
                           (list l
                                 (λ () (send infrabel
                                             get-switch-position
                                             (cadr l)))
                                 (λ () (send infrabel
                                             get-switch-position
                                             (caddr l)))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (cadr l) 1))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (cadr l) 2)
                                   (send infrabel
                                         set-switch-position!
                                         (caddr l) 1))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (cadr l) 2)
                                   (send infrabel
                                         set-switch-position!
                                         (caddr l) 2))
                                 ))
                         (cdr (assoc 'threeway
                                     (cdr (assoc
                                           'switch*
                                           (send infrabel get-track))))))))
        (new three-way-switch-panel%
             (parent switches*-panel)
             (name (symbol->string (caar (car switch))))
             (in (symbol->string (car (cdddar (car switch)))))
             (check1 (cadr (car switch)))
             (check2 (cadr (cdar switch)))
             (button1 (caddr (cdar switch)))
             (button2 (cadddr (cdar switch)))
             (button3 (cadddr (cddar switch)))
             (out1 (symbol->string (cadr (cdddar (car switch)))))
             (out2 (symbol->string (caddr (cdddar (car switch)))))
             (out3 (symbol->string (cadddr (cdddar (car switch))))))
        )

      (let ((switch (map (λ (l)
                           (list l
                                 (λ () (send infrabel
                                             get-switch-position
                                             (cadr l)))
                                 (λ () (send infrabel
                                             get-switch-position
                                             (caddr l)))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (cadr l) 1))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (cadr l) 2))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (caddr l) 1))
                                 (λ (t e)
                                   (send infrabel
                                         set-switch-position!
                                         (caddr l) 2))
                                 ))
                         (cdr (assoc
                               'cross
                               (cdr
                                (assoc 'switch* (send infrabel get-track))))))))
        (new cross-switch-panel%
             (parent switches*-panel)
             (name (symbol->string (caar (car switch))))
             (in1 (symbol->string (car (cdddar (car switch)))))
             (in2 (symbol->string (cadr (cdddar (car switch)))))
             (check1 (cadr (car switch)))
             (check2 (cadr (cdar switch)))
             (button1 (caddr (cdar switch)))
             (button2 (cadddr (cdar switch)))
             (button3 (cadddr (cddar switch)))
             (button4 (cadddr (cdddar switch)))
             (out1 (symbol->string (caddr (cdddar (car switch)))))
             (out2 (symbol->string (cadddr (cdddar (car switch))))))
        )

      (let fill
        ((block
          (map (λ (l)
                 (list l
                       (λ () (format "~a" (send infrabel
                                                get-detection-block-state
                                                (car l))))
                       ))
               (cdr (assoc 'detection-block-1
                           (cdr
                            (assoc 'block
                                   (send infrabel get-track))))))))
        (let ((status (new status%
                           (parent blocks-panel-1)
                           (name (symbol->string (caaar block)))
                           (callback (cadar block)))))
          (set! callbacks-list
                (append callbacks-list
                        (list (cons status (cadar block))))))

        (unless (null? (cdr block))
          (fill (cdr block))))

      (let fill
        ((block
          (map (λ (l)
                 (list l
                       (λ () (format "~a" (send infrabel
                                                get-detection-block-state
                                                (car l))))
                       ))
               (cdr (assoc 'detection-block-2
                           (cdr (assoc 'block
                                       (send infrabel get-track))))))))
        (let ((status (new status%
                           (parent blocks-panel-2)
                           (name (symbol->string (caaar block)))
                           (callback (cadar block)))))
          (set! callbacks-list
                (append callbacks-list
                        (list (cons status (cadar block))))))

        (unless (null? (cdr block))
          (fill (cdr block))))

      (let fill
        ((block (map (λ (l)
                       (list l
                             (λ () (format "~a" (send infrabel
                                                      get-segment-state
                                                      (car l))))
                             ))
                     (cdr (assoc 'segment
                                 (cdr (assoc 'block
                                             (send infrabel get-track))))))))
        (new status%
             (parent blocks-panel-U)
             (name (symbol->string (caaar block)))
             (callback (cadar block)))

        (unless (null? (cdr block))
          (fill (cdr block))))

      (fill-panel crossings-panel
                  (list
                   (list 'C-1 (cons "activate" (λ (t e) (void))))
                   (list 'C-2 (cons "activate" (λ (t e) (void))))
                   ))

      
      (fill-panel lights-panel
                  (list
                   (list 'L-1
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void))))
                   (list 'L-1
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void))))
                   (list 'L-2
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void))))
                   (list 'L-2
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void)))
                         (cons "activate" (λ (t e) (void))))
                   ))
      
      )
    ))