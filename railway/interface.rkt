;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> railway/main.rkt <<<                          ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "../track/interface.rkt"
         "segment.rkt"
         "detection-block.rkt"
         "switch.rkt"
         "crossing.rkt"
         "light.rkt"
         "train.rkt")
(provide railway%)

(define railway%
  (class object%
    (super-new)
    (init-field track)
    (init-field (track-info (send track get-meta-data)))

    ; higher-order procedures
    (define (search-object id object-list) (cdr (assoc id object-list)))
    (define (make-railway-objects object id-list in-list out-list state . position)
      (let setup-object ((ids id-list) (ins in-list) (outs out-list) (objs '()))
        (if (null? ids)
            objs
            (let* ((id (car ids)) (in (car ins)) (out (car outs)))
              (setup-object
               (cdr ids) (cdr ins) (cdr outs)
               (append objs
                       (if (null? position)
                           `((,id . ,(make-object object id track in out state)))
                           `((,id . ,(make-object object id track in out state (car position)))))))))))
    
    ; segments
    (define segments-list
      (let* ((segments (cdr (assoc 'segment track-info)))
             (segment-ids  (map car segments))
             (segment-ins  (map cadr segments))
             (segment-outs (map caddr segments)))
        (make-railway-objects
         segment% segment-ids segment-ins segment-outs 'free)))
    (define/public (get-segment-ids) (map car segments-list))
    (define/public (get-segment-state segment)
      (send (search-object segment segments-list) get-state))

    ; detection-blocks
    (define detects-list
      (let* ((detects (cdr (assoc 'detection-block track-info)))
             (detect-ids  (map car detects))
             (detect-ins  (map cadr detects))
             (detect-outs (map caddr detects)))
        (make-railway-objects
         detection-block% detect-ids detect-ins detect-outs 'free)))
    (define/public (get-detection-block-ids) (map car detects-list))
    (define/public (get-detection-block-state detection-block)
      (send (search-object detection-block detects-list) get-state))

    ; switches
    (define switches-list
      (let* ((switches (cdr (assoc 'switch track-info)))
             (switch-ids  (map car switches))
             (switch-ins  (map cadr switches))
             (switch-outs (map (λ (a d) (cons a d))
                               (map caddr switches)
                               (map cadddr switches))))
        (make-railway-objects
         switch% switch-ids switch-ins switch-outs 'free 'left)))
    (define/public (get-switch-ids) (map car switches-list))
    (define/public (get-switch-state switch)
      (send (search-object switch switches-list) get-state))
    (define/public (get-switch-position switch)
      (send (search-object switch switches-list) get-position))
    (define/public (set-switch-position! switch new-position)
      (send (search-object switch switches-list) set-position! switch new-position))

    ; crossings
    (define crossings-list
      (let* ((crossings (cdr (assoc 'crossing track-info)))
             (crossing-ids  (map car crossing))
             (crossing-ins  (map cadr crossing))
             )
        (make-railway-objects
         crossing% crossing-ids crossing-ins switch-outs 'free 'left)))
    (define/public (get-crossing-ids) (map car crossings-list))

    ;;
    ;; TODO: "railway-objects" aanpassen zodanig dat het ook kan werken met enkel een "segment(-list)" ipv met in/out
    ;; + waarom testen switch plots rood???
    ;;


    
    ))
      
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
    ; crossings
    (abstract crossings-list)
    (define crossings (crossings-list crossing%))
    (define/public (get-crossing-ids)
      (map car (crossings)))
    (define/public (get-crossing-state crossing)
      (send (search-object crossing (crossings)) get-state))
    (define/public (set-crossing-state! crossing new-state)
      (send (search-object crossing (crossings)) set-state! new-state))
    ; lights
    (abstract lights-list)
    (define lights (lights-list light%))
    (define/public (get-light-ids)
      (map car (lights)))
    (define/public (get-light-signal light)
      (send (search-object light (lights)) get-signal))
    (define/public (set-light-signal! light new-signal)
      (send (search-object light (lights)) set-signal! new-signal))


    
    ; trains ###################################################################
    #|
    (define trains '())
    (define/public (get-the-test-train-list connection)
      (list (cons 'test-train (new train%
                                   (connection connection)
                                   (id 'test-train)
                                   (previous '1-5)
                                   (current '1-4)))))
    (define/public (add-a-train! connection id previous-segment current-segment)
      (set! trains
            (append trains (list (cons id
                                       (new train%
                                            (connection connection)
                                            (id id)
                                            (previous previous-segment)
                                            (current current-segment)))))))
    (define/public (set-train-speed! train new-speed)
      (send (search-object train trains) set-train-speed! new-speed))
    (define/public (test train)
      (search-object train trains))
    |#
    ))
|#