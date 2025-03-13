;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       >>> infrabel/interface.rkt <<<                       ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 5 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../railway/interface.rkt")
(provide infrabel%)

(define infrabel%
  (class railway%
    (super-new)

    ;
    ; deze methodes nog allemaal aanpassen naar de "infrabel methodes"
    ; ipv gewoon aliasses te zijn voor de "railway methodes
    ;

    ;### !! abstractielaag niet doorbreken -> admin&debug !!
    (define/override (get-track)
      (super get-track))
    (define/override (stop)
      (super stop))
    (define/override (start-control-panel)
      (super start-control-panel))
    
    ; segments
    (define/override (get-segment-ids)
      (super get-segment-ids))
    (define/override (get-segment-next segment)
      (super get-segment-next segment))
    (define/override (get-segment-prev segment)
      (super get-segment-prev segment))
    (define/override (get-segment-state segment)
      (super get-segment-state segment))
    (define/override (set-segment-state! segment new-state)
      (super set-segment-state! segment new-state))

    ; detection-blocks
    (define/override (get-detection-block-ids)
      (super get-detection-block-ids))
    (define/override (get-detection-block-next detection-block)
      (super get-detection-block-next detection-block))
    (define/override (get-detection-block-prev detection-block)
      (super get-detection-block-prev detection-block))
    (define/override (get-detection-block-state detection-block)
      (super get-detection-block-state detection-block))
    (define/override (set-detection-block-state! detection-block new-state)
      (super set-detection-block-state! detection-block new-state))

    ; switches
    (define/override (get-switch-ids)
      (super get-switch-ids))
    (define/override (get-switch-next switch)
      (super get-switch-next switch))
    (define/override (get-switch-next-left switch)
      (super get-switch-next-left switch))
    (define/override (get-switch-next-right switch)
      (super get-switch-next-right switch))
    (define/override (get-switch-prev switch)
      (super get-switch-prev switch))
    (define/override (get-switch-state switch)
      (super get-switch-state switch))
    (define/override (set-switch-state! switch new-state)
      (super set-switch-state! switch new-state))
    (define/override (get-switch-position switch)
      (super get-switch-position switch))
    (define/override (set-switch-position! switch new-position)
      (super set-switch-position! switch new-position))

    ; crossings
    (define/override (get-crossing-ids)
      (super get-crossing-ids))
    (define/override (get-crossing-segments crossing)
      (super get-crossing-segments crossing))
    (define/override (get-crossing-position crossing)
      (super get-crossing-position crossing))
    (define/override (set-crossing-position! crossing new-position)
      (super set-crossing-position! crossing new-position))

    ; lights
    (define/override (get-light-ids)
      (super get-light-ids))
    (define/override (get-light-segment light)
      (super get-light-segment light))
    (define/override (get-light-signal light)
      (super get-light-signal light))
    (define/override (set-light-signal! light new-signal)
      (super set-light-signal! light new-signal))

    ; trains
    (define/override (get-train-ids)
      (super get-train-ids))
    (define/override (get-train-speed train)
      (super get-train-speed train))
    (define/override (set-train-speed! train new-speed)
      (super set-train-speed! train new-speed))
    
    ))