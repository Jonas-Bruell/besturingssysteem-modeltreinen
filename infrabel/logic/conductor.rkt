;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                              >>> infrabel/logic/conductor.rkt <<<                              ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../../railway/algorithms/search-track.rkt")

(provide conductor%)

(define conductor%
  (class object%
    (init-field infrabel provider-name train-name add-to-log add-to-update)
    (super-new)

    ; de dispatcher regelt momenteel het reserveren/freeen van "blocken"
    ; de conductor moet dus met de dispatcher afspreken om blocken te reserveren, alvorens hier te rijden
    ; wanneer de conductor geen block kan reserveren, moet de trein dus stoppen

    (define/public (go-in-direction direction)
      (define current-location (send infrabel get-train-location train-name))
      (define next-location
        (cdr (case direction
               ('next (send infrabel get-detection-block-next current-location))
               ('prev (send infrabel get-detection-block-prev current-location)))))
      (define search-reachable
        (case direction
          ('next search-reachable-dblocks-only-next)
          ('prev search-reachable-dblocks-only-prev)))
      (let ((reserved-elements (send infrabel reserve-block current-location next-location)))
        (if reserved-elements
            (search-reachable infrabel current-location) ;; hier moet er een "add to update" ofzo komen deze automatisch de vorige freed en de volgende reserved + trein stoppen wanneer niet kan reserven
            #f)))

    (define/public (follow-route route)
      (void))

    ))