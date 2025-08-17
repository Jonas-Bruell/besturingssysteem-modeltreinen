;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                  >>> railway/interface.rkt <<<                                 ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "components/segment.rkt"
         "components/detection-block.rkt"
         "components/switch.rkt"
         "components/crossing.rkt"
         "components/light.rkt"
         "components/train.rkt")

(provide railway%)

(define (get-track-info-loop connection)
  (let try-to-get-track-info ((track-info? (send connection get-track-info)))
    (unless track-info? (try-to-get-track-info (send connection get-track-info)))
    track-info?))

(define (get-train-info-loop connection)
  (let try-to-get-train-info ((train-info? (send connection get-train-info)))
    (unless train-info? (try-to-get-train-info (send connection get-train-info)))
    train-info?))

(define railway%
  (class object%
    (super-new)
    (init-field connection add-to-log add-to-update)
    (init-field (track-info (get-track-info-loop connection)))
    (init-field (train-info (get-train-info-loop connection)))
    
    (define (search id object-list) (cdr (assoc id object-list)))
    (define log-event (add-to-log "RAILWAY")) ; curryied

    (define/public (get-track-info) track-info)
    (define/public (get-train-info) train-info)

    ; stop simulator
    (define/public (stop)
      (send connection stop))

    ;;
    ;; segments
    ;;
    (define segments-list
      (let* ((segments (cdr (assoc 'segment track-info)))
             (segment-ids  (map car segments))
             (segment-ins  (map cadr segments))
             (segment-outs (map caddr segments)))
        (map (λ (id in out)
               (cons id (make-object segment% log-event id connection in out 'free)))
             segment-ids segment-ins segment-outs)))
    (define/public (get-segment-ids) (map car segments-list))
    (define/public (get-segment-states)
      (send (cdar segments-list) get-states))
    (define/public (get-segment-next segment)
      (send (search segment segments-list) get-next))
    (define/public (get-segment-prev segment)
      (send (search segment segments-list) get-prev))
    (define/public (get-segment-state segment)
      (send (search segment segments-list) get-state))
    (define/public (set-segment-state! segment new-state)
      (send (search segment segments-list) set-state! new-state))

    ;;
    ;; detection-blocks
    ;;
    (define detects-list
      (let* ((detects (cdr (assoc 'detection-block track-info)))
             (detect-ids  (map car detects))
             (detect-ins  (map cadr detects))
             (detect-outs (map caddr detects)))
        (map (λ (id in out)
               (cons id
                     (make-object detection-block% log-event id connection in out 'free)))
             detect-ids detect-ins detect-outs)))
    (define/public (get-detection-block-ids) (map car detects-list))
    (define/public (get-detection-block-states)
      (send (cdar detects-list) get-states))
    (define/public (get-detection-block-next detection-block)
      (send (search detection-block detects-list) get-next))
    (define/public (get-detection-block-prev detection-block)
      (send (search detection-block detects-list) get-prev))
    (define/public (get-detection-block-state detection-block)
      (send (search detection-block detects-list) get-state))
    (define/public (set-detection-block-state! detection-block new-state)
      (send (search detection-block detects-list) set-state! new-state))

    ;;
    ;; switches
    ;;
    (define switches-list
      (let* ((switches (cdr (assoc 'switch track-info)))
             (switch-ids  (map car switches))
             (switch-ins  (map cadr switches))
             (switch-outs (map (λ (a d) (cons a d))
                               (map caddr switches)
                               (map cadddr switches))))
        (map (λ (id in outs)
               (cons id
                     (make-object switch% log-event id connection in outs 'free
                       (send connection get-switch-position id))))
             switch-ids switch-ins switch-outs)))
    (define/public (get-switch-ids) (map car switches-list))
    (define/public (get-switch-positions)
      (send (cdar switches-list) get-positions))
    (define/public (get-switch-states)
      (send (cdar switches-list) get-states))
    (define/public (get-switch-next switch)
      (send (search switch switches-list) get-next))
    (define/public (get-switch-next-left switch)
      (send (search switch switches-list) get-next-left))
    (define/public (get-switch-next-right switch)
      (send (search switch switches-list) get-next-right))
    (define/public (get-switch-prev switch)
      (send (search switch switches-list) get-prev))
    (define/public (get-switch-state switch)
      (send (search switch switches-list) get-state))
    (define/public (set-switch-state! switch new-state)
      (send (search switch switches-list) set-state! new-state))
    (define/public (get-switch-position switch)
      (send (search switch switches-list) get-position))
    (define/public (set-switch-position! switch new-position)
      (send (search switch switches-list) set-position! new-position))

    ;;
    ;; crossings
    ;;
    (define init-position 'open)
    (define crossings-list
      (let* ((crossings (cdr (assoc 'crossing track-info)))
             (crossing-ids (map car crossings))
             (crossing-segments-lists (map cadr crossings)))
        (map (λ (id segments)
               (cons id (make-object crossing% log-event id connection segments init-position)))
             crossing-ids crossing-segments-lists)))
    (define/public (get-crossing-ids) (map car crossings-list))
    (define/public (get-crossing-positions)
      (send (cdar crossings-list) get-positions))
    (define/public (get-crossing-segments crossing)
      (send (search crossing crossings-list) get-segments))
    (define/public (get-crossing-position crossing)
      (send (search crossing crossings-list) get-position))
    (define/public (set-crossing-position! crossing new-position)
      (send (search crossing crossings-list) set-position! new-position))

    ;;
    ;; lights
    ;;
    (define init-sgnl 'Hp1)
    (define lights-list
      (let* ((lights (cdr (assoc 'light track-info)))
             (light-ids (map car lights))
             (light-segments (map cadr lights)))
        (map (λ (id segment)
               (cons id (make-object light% log-event id connection segment init-sgnl init-sgnl)))
             light-ids light-segments)))
    (define/public (get-light-ids) (map car lights-list))
    (define/public (get-light-signals)
      (send (cdar lights-list) get-signals))
    (define/public (get-light-segment light)
      (send (search light lights-list) get-segment))
    (define/public (get-light-signal light)
      (send (search light lights-list) get-signal))
    (define/public (get-prev-light-signal light)
      (send (search light lights-list) get-prev-signal))
    (define/public (set-light-signal! light new-signal)
      (send (search light lights-list) set-signal! new-signal))

    ;;
    ;; trains
    ;;
    (define trains-list
      (let* ((train-ids   '(T-3));train-info)
             (train-prevs '(U-2 #|S-25 S-4 S-8|#)) ;TEMP! TODO: vervangen door "add train" knoppen
             (train-currs '(1-3 #|1-8  2-7 2-5|#)))
        (map (λ (id prev curr)
               (cons id (make-object train% log-event id this connection prev curr)))
             train-ids train-prevs train-currs)))
    (define/public (get-train-ids) (map car trains-list))
    (define/public (unlock! train)
      (send (search train trains-list) unlock!))
    (define/public (get-train-location train)
      (send (search train trains-list) get-location))
    (define/public (get-train-speed train)
      (send (search train trains-list) get-train-speed))
    (define/public (set-train-speed! train new-speed)
      (send (search train trains-list) set-train-speed! new-speed))
    (define/public (manual-stop! train)
      (send (search train trains-list) manual-stop!))
    (define/public (follow-route train route)
      (send (search train trains-list) follow-route route))
    
    ))