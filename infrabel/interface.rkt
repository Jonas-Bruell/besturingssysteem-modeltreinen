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

    (define/public (add-conductor-to-train train-object provider-name conductor-name)
      (define conductor-object (new conductor% (train-object train-object)
                                    (provider-name provider-name) (add-to-update add-to-update)))
      (set! conductors (append conductors (cons conductor-name conductor-object))))

    (define (get-conductor conductor-name)
      (cdr (assoc conductors conductor-name)))

    (define/public (instruct-conductor-follow-route conductor-name route)
      (send (get-conductor conductor-name) follow-route route))
    
    #|infrabel%|#))