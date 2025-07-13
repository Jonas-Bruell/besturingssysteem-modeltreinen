#lang racket/gui

(require "config.rkt")
(provide infrabel-gui%)

(struct counter-panel
  (top-panel
   [value-thunk #:mutable] 
   [inc-callback #:mutable]
   [dec-callback #:mutable]))

(define infrabel-gui%
  (class frame%
    (super-new (label (string-append* `(,APPLICATION_NAME " -- Control Panel")))
               (width APPLICATION_WIDTH)
               (height APPLICATION_HEIGHT))

    (init-field infrabel railway-tab-panels-list)
    (init-field set-simulator-panel simulator-panel)

    ;; Global Structure
    (define menu-bar (new menu-bar% (parent this)))
    (define global-panel (new horizontal-panel% (parent this)))
    (define info-panel (new vertical-panel% (parent global-panel) (min-width TRACK_WIDTH)))

    ;; Tab Panel
    (define tabs (map car railway-tab-panels-list))
    (define (fill-tab tab-panel x)
      (send tab-panel change-childeren
            (lambda (c*)
              (list (counter-panel-top-panel (list-ref tabs x))))))
    
    (define tab-panel (new tab-panel%
                           (parent global-panel)
                           (choices tabs)
                           (style '(can-reorder can-close))
                           (min-width (- APPLICATION_WIDTH TRACK_WIDTH))
                           (vert-margin 7) (horiz-margin 10)
                           (callback (lambda (t e) (void)))))

    ;; Sim Panel
    (define sim-outer-panel
      (new panel% (parent info-panel) (horiz-margin 5) (style '(border)) (min-height TRACK_HEIGHT)))
    (define sim-inner-panel
      (new simulator-panel (embedded? sim-outer-panel)))
    (set-simulator-panel sim-inner-panel) ;; needed for track/simulator/graphics & /simulator
    (define/public (toggle-sim)
      (send sim-outer-panel show (not (send sim-outer-panel is-shown?))))

    ;; Log Panel
    (define logs-callback (λ (callback) (set! logs-callback callback)))
    (define (add-to-log-callback string)
      (send logs-callback insert
            (string-append "INFRABEL : " string)))
    (define log-panel
      (new panel% (parent info-panel) (min-height (- APPLICATION_HEIGHT TRACK_HEIGHT))))
    (let* ((text (new text%))
           (editor 
            (new editor-canvas%
                 (parent log-panel)
                 (editor text)
                 (style '(transparent no-focus))
                 (vert-margin 9)
                 (horiz-margin 5)
                 )))
      (logs-callback text))
    (define/public (toggle-log)
      (send log-panel show (not (send log-panel is-shown?))))

    (new button% (parent tab-panel) (label "do something") (callback (λ (t e) (send infrabel set-crossing-position! 'C-1 'closed) (add-to-log-callback "closing 'C-1"))))


    ;;???

    
    (define counter-panels '())



    (define (change-tab tp event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (fill-tab tp (send tp get-selection))))

    (let* ((menu-bar-menu
            (new menu% (label "Menu") (parent menu-bar)))
           (menu-bar-menu-stop
            (new menu-item% (label "Stop") (parent menu-bar-menu)
                 (callback (λ (t e) (void)))))
           (menu-bar-view
            (new menu% (label "View") (parent menu-bar)))
           (menu-bar-view-simulator
            (new menu-item% (label "Simulator") (parent menu-bar-view)
                 (callback (λ (t e) (send this toggle-sim) (send global-panel reflow-container)))))
           (menu-bar-view-logs
            (new menu-item% (label "Logs") (parent menu-bar-view)
                 (callback (λ (t e) (send this toggle-log) (send global-panel reflow-container)))))
           (menu-bar-help
            (new menu% (label "Help") (parent menu-bar)))


           )
      (void)

      )))

;(define gui (new infrabel-gui%))
;(send gui show #t)