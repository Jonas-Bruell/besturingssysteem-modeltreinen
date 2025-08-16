;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                 >>> infrabel/interface.rkt <<<                                 ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../railway/interface.rkt"
         "logic/dispatcher.rkt"
         "logic/conductor.rkt")
(provide infrabel%)

(define infrabel%
  (class railway% ; INHERITED FUNCTIONS!
    (inherit-field add-to-log add-to-update)
    (init-field stop-infrabel server)
    (super-new)

    (define log-event (add-to-log "INFRABEL")) ; curryied

    (define/override (stop)
      (super stop)
      (stop-infrabel))

    ;;
    ;; server
    ;;
    (define/public (send-all-providers x . l)
      (send server send-all-providers x l))

    ;;
    ;; detection-blocks
    ;;

    ;;
    ;; switches
    ;;
    (define/override (set-switch-position! switch new-position)
      (let ((old-position (send this get-switch-position switch)))
        (unless (eq? new-position old-position)
          (super set-switch-position! switch new-position)
          (send-all-providers "set-switch-position!" switch new-position))))

    ;;
    ;; crossings
    ;;
    (define/override (set-crossing-position! crossing new-position)
      (let ((old-position (send this get-crossing-position crossing)))
        (unless (eq? new-position old-position)
          (super set-crossing-position! crossing new-position)
          (send-all-providers "set-crossing-position!" crossing new-position))))
    
    ;;
    ;; lights
    ;;
    (define/override (set-light-signal! light new-signal)
      (let ((old-signal (send this get-light-signal light)))
        (unless (eq? new-signal old-signal) 
          (super set-light-signal! light new-signal)
          (send-all-providers "set-light-signal!" light new-signal))))

    ;;
    ;; trains
    ;;
    (define dispatcher (new dispatcher% (infrabel this)
                            (add-to-log log-event) (add-to-update add-to-update)))
    
    (define/public (reserve-block curr next)
      (send dispatcher reserve-block curr next))

    (define/public (free-block list)
      (send dispatcher free-block list))

    (define conductors '())

    (define/public (get-conductors) conductors)

    (define/public (add-conductor-to-train provider-name train-name)
      (define conductor (new conductor% (infrabel this)
                             (provider-name provider-name) (train-name train-name)
                             (add-to-log add-to-log) (add-to-update add-to-update)))
      (set! conductors (append conductors (list (cons train-name conductor)))))

    (define/public (get-conductor conductor-name)
      (cdr (assoc conductor-name conductors)))

    (define/public (instruct-conductor-go-in-direction conductor-name element)
      (send (get-conductor conductor-name) go-in-direction element))

    (define/public (instruct-conductor-follow-route conductor-name route)
      (send (get-conductor conductor-name) follow-route route))
    
    #|infrabel%|#))