;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                              >>> infrabel/logic/conductor.rkt <<<                              ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../../railway/algorithms/search-railway.rkt")

(provide conductor%)

(define conductor%
  (class object%
    (init-field infrabel provider-name train-name add-to-log add-to-update)
    (super-new)

    (define log-event (add-to-log (string-append "Conductor '" (symbol->string train-name))))

    (define active-threads '())
    (define/public (stop!) (map kill-thread active-threads))

    (define (auto-start-train speed)
      (log-event "Auto Start Train called"
                 (string-append "setting speed to " (number->string speed)))
      (send infrabel set-train-speed! train-name speed))
    
    (define (auto-stop-train&free reserved-elements)
      (log-event "Auto Stop Train & Free Block called"
                 (string-append "stopping train & freeing block"))
      (send infrabel set-train-speed! train-name 0)
      (send infrabel free-block reserved-elements))

    (define (drive-train-to-next-dblock direction-sign reachable-dblocks reserved-elements)
      (define loop-speed 2)
      (define allow-train-to-enter-segment 1.5) ; has to be shorter than loop-speed!
      (define this-thread
        (thread
         (λ () (auto-start-train (* direction-sign 32))
           (let check-on-next-dblock-loop () (sleep loop-speed)
             (let/cc return
               (for-each
                (λ (reachable-dblock)
                  (when (eq? (send infrabel get-detection-block-state reachable-dblock) 'occupied)
                    (begin (sleep allow-train-to-enter-segment)
                           (auto-stop-train&free reserved-elements)
                           (return #f))))
                (map cdr reachable-dblocks)) ; get id from element
               (check-on-next-dblock-loop)))))) ; only repeat loop after checking all dblocks
      (set! active-threads (append active-threads (list this-thread))))

    (define (go-in-direction direction)
      (define current-loc (send infrabel get-train-location train-name))
      (define next-loc
        (cdr (case direction
               ('next (send infrabel get-detection-block-next current-loc))
               ('prev (send infrabel get-detection-block-prev current-loc)))))
      (define search-reachable-dblocks
        (case direction
          ('next search-reachable-dblocks-only-next)
          ('prev search-reachable-dblocks-only-prev)))
      (let ((reserved-elements (send infrabel reserve-block current-loc next-loc))
            (reachable-blocks (remove-duplicates (search-reachable-dblocks infrabel current-loc)))
            (direction-sign (case direction ('next -1) ('prev +1))))
        (if reserved-elements
            (drive-train-to-next-dblock direction-sign reachable-blocks reserved-elements)
            #f)))

    (define/public (go-to-next) (go-in-direction 'next))
    (define/public (go-to-prev) (go-in-direction 'prev))
    
    (define/public (follow-route route)
      (void))

    ))