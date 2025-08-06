;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                    >>> infrabel/gui.rkt <<<                                    ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(require "config.rkt")

(provide infrabel-gui%)

(define infrabel-gui%
  (class frame%
    (init-field infrabel railway-tab-panels-list)
    (init-field logs-callback add-to-log add-to-update stop-infrabel)
    (init-field set-simulator-panel simulator-panel)
    (super-new (label (string-append APPLICATION_NAME "  --  Control Panel"))
               (width APPLICATION_WIDTH)
               (height APPLICATION_HEIGHT))
    (define/augment (on-close) (stop-infrabel))

    ;; Menu Bar
    (define menu-bar (new menu-bar% (parent this)))
    (let* ((menu-bar-menu (new menu% (label "Menu") (parent menu-bar)))
           (menu-bar-view (new menu% (label "View") (parent menu-bar)))
           (menu-bar-help (new menu% (label "Help") (parent menu-bar))))
      (new menu-item% (label "Stop") (parent menu-bar-menu)
           (callback (λ (t e) (void))))
      (new menu-item% (label "Simulator") (parent menu-bar-view)
           (callback (λ (t e) (send this toggle-sim) (send global-panel reflow-container))))
      (new menu-item% (label "Logs") (parent menu-bar-view)
           (callback (λ (t e) (send this toggle-log) (send global-panel reflow-container))))
      )
    
    (define global-panel (new horizontal-panel% (parent this)))
    (define input-panel (new vertical-panel% (parent global-panel)))
    (define log-event (add-to-log "INFRABEL")) ; curryied

    ;; Status Panel
    (define status-panel (new group-box-panel% (parent input-panel) (min-height 25)
                              (label "Statussen") (horiz-margin 10)))
    (define hgbp (new horizontal-pane% (parent status-panel)))
    (define train-status-pane (new vertical-pane% (parent hgbp)))
    (for-each
     (λ (id)
       (let* ((tr-str "Train '")
              (id-str (symbol->string id))
              (lc-str " on location : ")
              (get-tr (λ () (symbol->string (send infrabel get-train-location id))))
              (msg (new message% (parent train-status-pane)
                        (label (string-append tr-str id-str lc-str "____")))))
         (add-to-update
          (λ () (send msg set-label (string-append tr-str id-str lc-str (get-tr)))))))
     (send infrabel get-train-ids))
    (define crossing-status-pane (new vertical-pane% (parent hgbp)))
    (for-each
     (λ (id)
       (let* ((cr-str "Crossing '")
              (id-str (symbol->string id))
              (ps-str " has position : ")
              (get-cr (λ () (symbol->string (send infrabel get-crossing-position id))))
              (msg (new message% (parent crossing-status-pane)
                        (label (string-append cr-str id-str ps-str "_________")))))
         (add-to-update (λ () (send msg set-label (string-append cr-str id-str ps-str (get-cr)))))))
     (send infrabel get-crossing-ids))
    (define light-status-pane (new vertical-panel% (parent hgbp)))
    (for-each
     (λ (id)
       (let* ((lg-str "Light '")
              (id-str (symbol->string id))
              (sg-str " has signal : ")
              (get-lg (λ () (symbol->string (send infrabel get-light-signal id))))
              (msg (new message% (parent light-status-pane)
                        (label (string-append lg-str id-str sg-str "________________")))))
         (add-to-update (λ () (send msg set-label (string-append lg-str id-str sg-str (get-lg)))))))
     (send infrabel get-light-ids))
    
    ;; Tab Panel 
    (define tabs (map car railway-tab-panels-list))
    (define tab-panes-list '())
    
    (define (fill-tab tab-panel nr)
      (send tab-panel change-children
            (lambda (c*)
              (list (list-ref tab-panes-list nr)))))
    
    (define (change-tab tab-panel event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (fill-tab tab-panel (send tab-panel get-selection))))
   
    (define tab-panel (new tab-panel% (parent input-panel) (min-height 880)
                           (choices tabs) (style '(can-reorder can-close))
                           (min-width (- APPLICATION_WIDTH TRACK_WIDTH))
                           (vert-margin 7) (horiz-margin 10) (callback change-tab)))
    (for-each
     (λ (tab-pane)
       (set! tab-panes-list
             (append tab-panes-list (list (new tab-pane
                                               (parent tab-panel)
                                               (connection infrabel)
                                               (add-to-log log-event)
                                               (add-to-update add-to-update))))))
     (map cdr railway-tab-panels-list))
    (fill-tab tab-panel 0)

    (define info-panel (new vertical-panel% (parent global-panel) (min-width TRACK_WIDTH)))

    ;; Sim Panel
    (define sim-outer-panel
      (new panel% (parent info-panel) (horiz-margin 5) (style '(border)) (min-height TRACK_HEIGHT)))
    (new button% (parent sim-outer-panel) (label "Noodstop") (horiz-margin 14)
         (callback (λ (t e) (displayln "Noodstop"))))
    (define sim-inner-panel
      (new simulator-panel (embedded? sim-outer-panel)))
    (set-simulator-panel sim-inner-panel) ;; needed for track/simulator/graphics & /simulator
    (define/public (toggle-sim)
      (send sim-outer-panel show (not (send sim-outer-panel is-shown?))))

    ;; Log Panel
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

    #| </infrabel-gui%> |#))