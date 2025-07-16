#lang racket/gui

(provide tab-crossings-lights%)

(define tab-crossings-lights%
  (class vertical-panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent)
               (border 30))

    (define log-event (add-to-log "Signals Tab")) ; curryied

    (new panel% (parent this)) ; filler

    ;; CROSSINGS
    (define crossing-panel (new vertical-panel% (parent this) (alignment '(left top))))
    (define cgbp (new group-box-panel% (label "Crossings") (parent crossing-panel)
                      (stretchable-width #f) (stretchable-height #f) (border 3)))
    (for-each
     (λ (crossing-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string crossing-id)) (horiz-margin 9)
                        (parent cgbp) (stretchable-width #f) (stretchable-height #f)))
              (hzp (new horizontal-panel% (parent gbp))))
         (for-each
          (λ (crossing-position)
            (new button% (label (symbol->string crossing-position)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Crossing Button Pressed"
                                      (string-append* `("change crossing '"
                                                        ,(symbol->string crossing-id)
                                                        " to position '"
                                                        ,(symbol->string crossing-position))))
                    (send connection set-crossing-position! crossing-id crossing-position)))))
          (send connection get-crossing-positions))))
     (send connection get-crossing-ids))

    ;; LIGHTS
    (define lights-panel (new vertical-panel% (parent this) (alignment '(left top))))
    (define lgbp (new group-box-panel% (label "Lights") (parent lights-panel)
                      (stretchable-width #f) (stretchable-height #f) (border 3)))
    (for-each
     (λ (light-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string light-id)) (horiz-margin 9)
                        (parent lgbp) (stretchable-width #f) (stretchable-height #f)))
              (hzp (new horizontal-panel% (parent gbp))))
         (for-each
          (λ (light-signals)
            (new button% (label (symbol->string light-signals)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Lights Button Pressed"
                                      (string-append* `("change light '"
                                                        ,(symbol->string light-id)
                                                        " to signal '"
                                                        ,(symbol->string light-signals))))
                    (send connection set-light-signal! light-id light-signals)))))
          (send connection get-light-signals))))
     (send connection get-light-ids))

    (new panel% (parent this)) ; filler

    #| </tab-crossings-lights > |#))


#| ; komt uit control panel

(require "config.rkt")
(provide control-panel%)

(define control-panel%
  (class frame%
    (init-field railway)
    (init-field track-info)
    (super-new (label "Railway Controle Panel")
               (width SERVER_A&D_WIDTH)
               (height SERVER_A&D_HEIGHT))

    ;
    ; update!
    ;
    (define callbacks-list '())
    (define (add-to-callbacks-list id callback)
      (append callbacks-list (list id callback)))
    (define/public (update! id)
      (let ((elements-tail (member id callbacks-list)))
        (if elements-tail
            ((cadr elements-tail))
            (error "railway/control-panel.rkt - update! - id not found: " id))))

    ;
    ; define all panels & panes
    ;
    (let* ((menu-bar (new menu-bar% (parent this)))
           (menu-bar-menu (new menu% (label "Menu") (parent menu-bar)))

           (trains-panel (new horizontal-pane% (parent this)))
           (emergency-stop-pane (new vertical-pane% (parent trains-panel)))
           (trains-pane (new vertical-pane% (parent trains-panel)))
           
           (tab-panel (new tab-panel% (parent this) (choices '("Switches" "Other"))))

           (left-column (new vertical-pane% (parent this) (min-width (/ SERVER_A&D_WIDTH 2))))
           (right-column (new vertical-pane% (parent this) (min-width (/ SERVER_A&D_WIDTH 2))))
           )
      
      (new menu-item% (label "stop") (parent menu-bar-menu) (callback (λ (t e) (send railway stop))))




      (void)
      )))
#|
   

      
|#
      ; FILL CROSSINGS
      (define crossing-panel%
        (class horizontal-group-box-panel%
          (init-field parent name)
          (super-new (label name) (parent parent))

          (new button% (label "open") (parent this))
          (new message% (label "opened") (parent this))
          (new button% (label "close") (parent this))
    
          ))
      (fill-panel crossings-panel
                  (list
                   (list 'C-1
                         (cons "open" (λ (t e) (send railway set-crossing-position! 'C-1 'open)))
                         (cons "close" (λ (t e) (send railway set-crossing-position! 'C-1 'closed))))
                   (list 'C-2
                         (cons "open" (λ (t e) (send railway set-crossing-position! 'C-2 'open)))
                         (cons "close" (λ (t e) (send railway set-crossing-position! 'C-2 'closed))))
                   ))

      ; FILL LIGHTS
      (define light-panel%
        (class group-box-panel%
          (init-field parent name)
          (super-new (label name) (parent parent))

          ))
      (fill-panel lights-panel
                  (list
                   (list 'L-1
                         (cons "Hp0" (λ (t e) (send railway set-light-signal! 'L-1 'Hp0)))
                         (cons "Hp1" (λ (t e) (send railway set-light-signal! 'L-1 'Hp1)))
                         (cons "Hp0+Sh0" (λ (t e) (send railway set-light-signal! 'L-1 'Hp0+Sh0)))
                         (cons "Ks1+Zs3" (λ (t e) (send railway set-light-signal! 'L-1 'Ks1+Zs3))))
                   (list 'L-1
                         (cons "Ks2" (λ (t e) (send railway set-light-signal! 'L-1 'Ks2)))
                         (cons "Ks2+Zs3" (λ (t e) (send railway set-light-signal! 'L-1 'Ks2+Zs3)))
                         (cons "Sh1" (λ (t e) (send railway set-light-signal! 'L-1 'Sh1)))
                         (cons "Ks1+Zs3+Zs3v" (λ (t e) (send railway set-light-signal! 'L-1 'Ks1+Zs3+Zs3v))))
                   (list 'L-2
                         (cons "Hp0" (λ (t e) (send railway set-light-signal! 'L-2 'Hp0)))
                         (cons "Hp1" (λ (t e) (send railway set-light-signal! 'L-2 'Hp1)))
                         (cons "Hp0+Sh0" (λ (t e) (send railway set-light-signal! 'L-2 'Hp0+Sh0)))
                         (cons "Ks1+Zs3" (λ (t e) (send railway set-light-signal! 'L-2 'Ks1+Zs3))))
                   (list 'L-2
                         (cons "Ks2" (λ (t e) (send railway set-light-signal! 'L-2 'Ks2)))
                         (cons "Ks2+Zs3" (λ (t e) (send railway set-light-signal! 'L-2 'Ks2+Zs3)))
                         (cons "Sh1" (λ (t e) (send railway set-light-signal! 'L-2 'Sh1)))
                         (cons "Ks1+Zs3+Zs3v" (λ (t e) (send railway set-light-signal! 'L-2 'Ks1+Zs3+Zs3v))))
                   ))
      (define Hp0          'Hp0)
      (define Hp1          'Hp1)
      (define Hp0+Sh0      'Hp0+Sh0)
      (define Ks1+Zs3      'Ks1+Zs3)
      (define Ks2          'Ks2)
      (define Ks2+Zs3      'Ks2+Zs3)
      (define Sh1          'Sh1)
      (define Ks1+Zs3+Zs3v 'Ks1+Zs3+Zs3v)

      )

|#