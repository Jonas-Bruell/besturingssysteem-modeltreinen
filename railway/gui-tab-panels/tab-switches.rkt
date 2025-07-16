#lang racket/gui

(provide tab-switches%)

(define COLOR_FREE "green")
(define COLOR_RESERVED "blue")
(define COLOR_OCCUPIED "red")

(define tab-switches%
  (class vertical-panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent))

    (define log-event (add-to-log "Switches Tab")) ; curryied

    (define switches-container
      (new horizontal-panel% (parent this)))
    (define switches-panel%
      (class vertical-panel% (super-new (parent switches-container) (alignment '(left top)))))
    (define switches-panel-0 (new switches-panel%))
    (define switches-panel-1 (new switches-panel%))
    (define switches-panel-2 (new switches-panel%))
    (define switches-panel-3 (new switches-panel%))

    (define panel-counter 0) ; initialise with las
    (define (which-panel)
      (define (inc) (set! panel-counter (modulo (+ panel-counter 1) 4)))
      (cond ((= panel-counter 0) (inc) switches-panel-0)
            ((= panel-counter 1) (inc) switches-panel-1)
            ((= panel-counter 2) (inc) switches-panel-2)
            ((= panel-counter 3) (inc) switches-panel-3)))

    ; Initialise switches
    (for-each
     (λ (switch-id)
       (let* ((gbp
               (new group-box-panel% (label (symbol->string switch-id)) (vert-margin 20)
                    (parent (which-panel)) (stretchable-width #f) (stretchable-height #f)))
              (hzp
               (new horizontal-panel% (parent gbp)))
              (in-pane
               (new vertical-pane% (parent hzp) (alignment '(center center))))
              (btn-pane
               (new vertical-pane% (parent hzp) (alignment '(center center))))
              (out-pane
               (new vertical-pane% (parent hzp) (alignment '(center center))))
              (in-label
               (new message% (parent in-pane) (horiz-margin 10)
                    (label (symbol->string (send connection get-switch-prev switch-id)))))
              (out-label-left
               (new message% (parent out-pane) (horiz-margin 10)
                    (label (symbol->string (send connection get-switch-next-left switch-id)))))
              (out-label-right
               (new message% (parent out-pane) (horiz-margin 10)
                    (label (symbol->string (send connection get-switch-next-right switch-id))))))
        
         (define (update-switch-colors switch-id)
           (let ((postion (send connection get-switch-position switch-id)))
             (cond ((eq? postion 'left)
                    (send out-label-left set-color COLOR_FREE)
                    (send out-label-right set-color COLOR_OCCUPIED))
                   ((eq? postion 'right)
                    (send out-label-left set-color COLOR_OCCUPIED)
                    (send out-label-right set-color COLOR_FREE)))))
         
         (for-each
          (λ (switch-position)
            (new button% (label (symbol->string switch-position)) (parent btn-pane)
                 (callback
                  (λ (t e) (log-event "Switch Button Pressed"
                                      (string-append* `("change switch '"
                                                         ,(symbol->string switch-id)
                                                         " to position '"
                                                         ,(symbol->string switch-position))))
                    (send connection set-switch-position! switch-id switch-position)
                    (update-switch-colors switch-id)))))
          (send connection get-switch-positions))
         (send in-label set-color COLOR_FREE)
         (update-switch-colors switch-id)))
     (send connection get-switch-ids)) 

    #| </tab-switches%> |#))


#| ; komt uit control-panel
     
      ;;
      ;; FILL 3-WAY-SWITCH
      ;;
      (define three-way-switch-panel%
        (class horizontal-group-box-panel%
          (init-field parent
                      name
                      in
                      check1 check2
                      button1 button2 button3
                      out1 out2 out3)
          (super-new (label name) (parent parent))
          ;; color active track
          (define (change-colors)
            (let ((position1 (check1))
                  (position2 (check2)))
              (cond ((eq? position1 'left)
                     (send out-label1 set-color COLOR_FREE)
                     (send out-label2 set-color COLOR_OCCUPIED)
                     (send out-label3 set-color COLOR_OCCUPIED))
                    ((and (eq? position2 'right))
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_FREE)
                     (send out-label3 set-color COLOR_OCCUPIED))
                    ((and (eq? position2 'right))
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_OCCUPIED)
                     (send out-label3 set-color COLOR_FREE)))))
          ;; panes
          (define in-pane
            (new pane% (parent this)))
          (define button-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          (define out-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          ;; content
          (define in-label
            (new message% (label in) (parent in-pane)))
          (define out-label1
            (new message% (label out1) (parent out-pane)))
          (define out-label2
            (new message% (label out2) (parent out-pane)))
          (define out-label3
            (new message% (label out3) (parent out-pane)))
          (define the-button1
            (new button% (label "pos-1") (parent button-pane)
                 (callback (wrap-callback button1 change-colors))))
          (define the-button2
            (new button% (label "pos-2") (parent button-pane)
                 (callback (wrap-callback button2 change-colors))))
          (define the-button3
            (new button% (label "pos-3") (parent button-pane)
                 (callback (wrap-callback button3 change-colors))))
          (send in-label set-color COLOR_FREE)
          (change-colors)))
      (let ((switch
             (map (λ (l)
                    (list l
                          (λ () (send railway get-switch-position (cadr l)))
                          (λ () (send railway get-switch-position (caddr l)))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right)
                            (send railway set-switch-position! (caddr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right)
                            (send railway set-switch-position! (caddr l) 'right))))
                  (cdr (assoc 'threeway
                              (cdr (assoc 'switch*
                                          track-info)))))))
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

      ;;
      ;; FILL CROSS-SWITCH
      ;;
      (define cross-switch-panel%
        (class horizontal-group-box-panel%
          (init-field parent
                      name
                      in1 in2
                      check1 check2
                      button1 button2 button3 button4
                      out1 out2)
          (super-new (label name) (parent parent))
          ;; color active track
          (define (change-colors)
            (let ((position1 (check1))
                  (position2 (check2)))
              (cond ((eq? position1 'left)
                     (send in-label1 set-color COLOR_FREE)
                     (send in-label2 set-color COLOR_OCCUPIED))
                    ((eq? position1 'right)
                     (send in-label1 set-color COLOR_OCCUPIED)
                     (send in-label2 set-color COLOR_FREE)))
              (cond ((eq? position2 'left)
                     (send out-label1 set-color COLOR_FREE)
                     (send out-label2 set-color COLOR_OCCUPIED))
                    ((eq? position2 'right)
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_FREE)))))
          ;; panes
          (define in-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          (define button-pane
            (new horizontal-pane% (parent this) (alignment '(center center))))
          (define button-pane-L
            (new vertical-pane% (parent button-pane) (alignment '(center center))))
          (define button-pane-R
            (new vertical-pane% (parent button-pane) (alignment '(center center))))
          (define out-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          ;; content
          (define in-label1
            (new message% (label in1) (parent in-pane)))
          (define in-label2
            (new message% (label in2) (parent in-pane)))
          (define out-label1
            (new message% (label out1) (parent out-pane)))
          (define out-label2
            (new message% (label out2) (parent out-pane)))
          (define the-button1
            (new button% (label "pos-1") (parent button-pane-L)
                 (callback (wrap-callback button1 change-colors))))
          (define the-button2
            (new button% (label "pos-2") (parent button-pane-L)
                 (callback (wrap-callback button2 change-colors))))
          (define the-button3
            (new button% (label "pos-1") (parent button-pane-R)
                 (callback (wrap-callback button3 change-colors))))
          (define the-button4
            (new button% (label "pos-2") (parent button-pane-R)
                 (callback (wrap-callback button4 change-colors))))
          (change-colors)))
      (let ((switch
             (map (λ (l)
                    (list l
                          (λ () (send railway get-switch-position (cadr l)))
                          (λ () (send railway get-switch-position (caddr l)))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right))
                          (λ (t e)
                            (send railway set-switch-position! (caddr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (caddr l) 'right))))
                  (cdr (assoc 'cross
                              (cdr (assoc 'switch*
                                          track-info)))))))
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
             (out2 (symbol->string (cadddr (cdddar (car switch)))))))


|#