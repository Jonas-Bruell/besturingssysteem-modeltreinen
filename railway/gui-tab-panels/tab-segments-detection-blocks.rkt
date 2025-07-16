#lang racket/gui

(provide tab-segments-detection-blocks%)

(define tab-segments-detection-blocks%
  (class vertical-panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent))

    (define log-event (add-to-log "Rails Tab")) ; curryied
    
    (new button% (parent this) (label "segments-dblocks"))

    (send connection get-detection-block-ids)

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