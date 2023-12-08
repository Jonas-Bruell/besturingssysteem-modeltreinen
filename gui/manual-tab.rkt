;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                           >>> manual-tab.rkt <<<                           ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui
(provide make-tab
         fill-tab)

(struct tab-panel (manual-panel))

(define (make-tab parent
                  switches-callback-list
                  trains-callback-list
                  crossings-callback-list
                  lights-callback-list)
  ; defining panels
  (define manuel-panel (new horizontal-panel% (parent parent)))
  (define switches-panel (new vertical-panel% (parent manuel-panel)))
  (define tcl-panel (new vertical-panel% (parent manuel-panel)))
  (define trains-panel (new vertical-panel% (parent tcl-panel)))
  (define crossings-panel (new vertical-panel% (parent tcl-panel)))
  (define lights-panel (new vertical-panel% (parent tcl-panel)))
  ; higher order functions
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
  (fill-panel switches-panel switches-callback-list)
  (fill-panel trains-panel trains-callback-list)
  (fill-panel crossings-panel crossings-callback-list)
  (fill-panel lights-panel lights-callback-list)
  ; initialising panel
  (tab-panel manuel-panel))

(define (fill-tab tab-panels tab-panel tab)
  (send tab-panel
        change-children
        (λ (c*)
          (list (tab-panel-manual-panel (list-ref tab-panels tab))))))