;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                          >>> railway/gui-tab-panels/tab-states.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(provide tab-states%)

(define tab-states%
  (class panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent)
               (style '(auto-vscroll)))
    (define tab-panel (new vertical-panel% (parent this)))

    (define log-event (add-to-log "Rails Tab")) ; curryied

    
    (define (set-color! msg-object state)
      (send msg-object set-color (cond ((eq? state 'free) "green")
                                       ((eq? state 'reserved) "orange")
                                       ((eq? state 'occupied) "red")
                                       (else "black"))))

    ;;
    ;; DETECTION BLOCKS
    ;;
    (define dblocks-panel (new group-box-panel% (label "Detection Blocks") (parent tab-panel)
                               (stretchable-height #f) (horiz-margin 20) (vert-margin 10)))
    (define dblocks-container (new horizontal-panel% (parent dblocks-panel)))
    
    (define dblocks-column%
      (class vertical-panel% (super-new (parent dblocks-container) (alignment '(left top)))))
    (define dblocks-column-0 (new dblocks-column%))
    (define dblocks-column-1 (new dblocks-column%))
    (define dblocks-column-2 (new dblocks-column%))
    (define dblocks-column-3 (new dblocks-column%))

    (define dblocks-panel-counter 0)
    (define (which-dblock-column)
      (define (inc) (set! dblocks-panel-counter (modulo (+ dblocks-panel-counter 1) 4)))
      (cond ((= dblocks-panel-counter 0) (inc) dblocks-column-0)
            ((= dblocks-panel-counter 1) (inc) dblocks-column-1)
            ((= dblocks-panel-counter 2) (inc) dblocks-column-2)
            ((= dblocks-panel-counter 3) (inc) dblocks-column-3)))
    
    (for-each
     (λ (dblock-id)
       (let* ((gbp (new group-box-panel% (parent (which-dblock-column))  (horiz-margin 9)
                        (label (symbol->string dblock-id))
                        (stretchable-width #f) (stretchable-height #f)))
              (hgph (new horizontal-pane% (parent gbp)))
              (msg (new message% (label "__________") (parent hgph)))
              (hzp (new vertical-pane% (parent hgph))))
         (add-to-update
          (λ () (send msg set-label (symbol->string
                                     (send connection get-detection-block-state dblock-id)))
            (set-color! msg (send connection get-detection-block-state dblock-id))))
         (for-each
          (λ (dblock-state)
            (new button% (label (symbol->string dblock-state)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Detection Block Button Pressed"
                                      (string-append* "change detection block '"
                                                      (symbol->string dblock-id)
                                                      " to state '"
                                                      (symbol->string dblock-state)))
                    (send connection set-detection-block-state! dblock-id dblock-state)))))
          (send connection get-detection-block-states))))
     (send connection get-detection-block-ids))

    ;;
    ;; SWITCHES
    ;;
    (define switches-panel (new group-box-panel% (label "Switches") (parent tab-panel)
                                (stretchable-height #f) (horiz-margin 20) (vert-margin 0)))
    (define switches-container (new horizontal-panel% (parent switches-panel)))
    
    (define switches-column%
      (class vertical-panel% (super-new (parent switches-container) (alignment '(left top)))))
    (define switches-column-0 (new switches-column%))
    (define switches-column-1 (new switches-column%))
    (define switches-column-2 (new switches-column%))
    (define switches-column-3 (new switches-column%))

    (define switches-panel-counter 0)
    (define (which-switches-column)
      (define (inc) (set! switches-panel-counter (modulo (+ switches-panel-counter 1) 4)))
      (cond ((= switches-panel-counter 0) (inc) switches-column-0)
            ((= switches-panel-counter 1) (inc) switches-column-1)
            ((= switches-panel-counter 2) (inc) switches-column-2)
            ((= switches-panel-counter 3) (inc) switches-column-3)))
    
    (for-each
     (λ (switch-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string switch-id)) (horiz-margin 9)
                        (parent (which-switches-column))
                        (stretchable-width #f) (stretchable-height #f)))
              (msg (new message% (label "__________") (parent gbp)))
              (hzp (new horizontal-panel% (parent gbp))))
         (add-to-update
          (λ () (send msg set-label (symbol->string
                                     (send connection get-switch-state switch-id)))
            (set-color! msg (send connection get-switch-state switch-id))))
         (for-each
          (λ (switch-state)
            (new button% (label (symbol->string switch-state)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Switch Button Pressed"
                                      (string-append* "change switch '"
                                                      (symbol->string switch-id)
                                                      " to state '"
                                                      (symbol->string switch-state)))
                    (send connection set-switch-state! switch-id switch-state)))))
          (send connection get-switch-states))))
     (send connection get-switch-ids))

    ;;
    ;; SEGMENTS
    ;;
    (define segments-panel (new group-box-panel% (label "Segments") (parent tab-panel)
                                (stretchable-height #f) (horiz-margin 20) (vert-margin 10)))
    (define segments-container (new horizontal-panel% (parent segments-panel)))
    
    (define segments-column%
      (class vertical-panel% (super-new (parent segments-container) (alignment '(left top)))))
    (define segments-column-0 (new segments-column%))
    (define segments-column-1 (new segments-column%))
    (define segments-column-2 (new segments-column%))
    (define segments-column-3 (new segments-column%))

    (define segments-panel-counter 0)
    (define (which-segments-column)
      (define (inc) (set! segments-panel-counter (modulo (+ segments-panel-counter 1) 4)))
      (cond ((= segments-panel-counter 0) (inc) segments-column-0)
            ((= segments-panel-counter 1) (inc) segments-column-1)
            ((= segments-panel-counter 2) (inc) segments-column-2)
            ((= segments-panel-counter 3) (inc) segments-column-3)))
    
    (for-each
     (λ (segment-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string segment-id)) (horiz-margin 9)
                        (parent (which-segments-column))
                        (stretchable-width #f) (stretchable-height #f)))
              (msg (new message% (label "__________") (parent gbp)))
              (hzp (new horizontal-panel% (parent gbp))))
         (add-to-update
          (λ () (send msg set-label (symbol->string
                                     (send connection get-segment-state segment-id)))
            (set-color! msg (send connection get-segment-state segment-id))))
         (for-each
          (λ (segment-state)
            (new button% (label (symbol->string segment-state)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Segment Button Pressed"
                                      (string-append* "change segment '"
                                                      (symbol->string segment-id)
                                                      " to position '"
                                                      (symbol->string segment-state)))
                    (send connection set-segment-state! segment-id segment-state)))))
          (send connection get-segment-states))))
     (send connection get-segment-ids))





    #| </tab-segments-detection-blocks%> |#))




#| ; komt uit control panel

    (define horizontal-group-box-panel%
      (class horizontal-pane%
        (init label parent)
        (super-new
         (parent (new group-box-panel% (label label) (parent parent)))
         (alignment '(center center)))))

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
           
           (switches*-panel
            (new group-box-panel% (label "SPECIAL SWITCHES") (parent right-column)))
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

      ;;
      ;; SEGMENTS & SWITCHES
      ;;
      (define status%
        (class pane%
          (init-field parent
                      name
                      callback
                      )
          (super-new
           (parent parent)
           )
    
          (define group-box (new group-box-panel% (label name) (parent this)))
           
          (define message
            (new message% (label (callback)) (parent group-box) (auto-resize #t)))

          (define/public (set-label msg)
            (send message set-label msg))
          (define/public (set-color clr)
            (send message set-color clr))
          ))
      
      ; FILL D-BLOCKS
      (let fill
        ((block
          (map (λ (l)
                 (list l (λ () (format "~a" (send railway
                                                  get-detection-block-state
                                                  (car l))))))
               (cdr (assoc 'detection-block track-info))))
         (index 0))
        (let ((status (new status%
                           (parent (if (< index 10)
                                       blocks-panel-1
                                       blocks-panel-2))
                           (name (symbol->string (caaar block)))
                           (callback (cadar block)))))
          (set! callbacks-list
                (append callbacks-list
                        (list (cons status (cadar block))))))
        (unless (null? (cdr block))
          (fill (cdr block) (+ index 1))))

      ; FILL BLOCKS
      (let fill
        ((block (map (λ (l) (list l (λ () (format "~a" (send railway
                                                             get-segment-state
                                                             (car l))))))
                     (cdr (assoc 'segment track-info)))))
        (new status%
             (parent blocks-panel-U)
             (name (symbol->string (caaar block)))
             (callback (cadar block)))
        (unless (null? (cdr block))
          (fill (cdr block))))
|#