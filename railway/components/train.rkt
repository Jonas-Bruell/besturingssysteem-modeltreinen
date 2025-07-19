;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> train.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "../algorithms/search-track.rkt")
(provide train%)

(define train%
  (class object%
    (super-new)

    ; This class represents an atomic abstraction of a train
    ;
    ; @param add-to-log-callback :: function to add a string to the log callback
    ; @param id :: the name of the train
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param previous :: previous track segment of train
    ; @param current :: current track segment of train
    ;
    (init-field add-to-log id railway connection previous current-location)

    (define unlocked? #t)
    (define route '())
    
    ; Adding train to track -- Only works on simulator, retuns false on hardware
    (send connection add-loco id previous current-location)

    ;
    ; add-to-log :: add a message of type string to the log
    ;
    ; @param string :: the string to be added to the log
    ;
    (define log-event (add-to-log (string-append "Train '" (symbol->string id)))) ; currying

    (define (lock!) (set! unlocked? #f) (log-event "Function lock! called"
                                                   (string-append "locking train")))
    (define/public (unlock!) (set! unlocked? #t) (log-event "Method unlock! called"
                                                            (string-append "unlocking train")))
    
    ;; Logic for get-location
    (define possible-next-locations '())
    (define (get-possible-next-locations current-dblock)
      (define next-dblocks-list '())
      (define (search-rec previous current)
        (display "new rec ") (display current)
        (cond ((eq? current 'NIL) (void))
              ((eq? (car current) 'dblock)
               (set! next-dblocks-list (append next-dblocks-list (list current))))
              ((eq? (car current) 'switch)
               (let* ((curr (cdr current))
                      (switch-prev (send railway get-switch-prev curr))
                      (switch-next-left (send railway get-switch-next-left curr))
                      (switch-next-right (send railway get-switch-next-right curr)))
                 (when (equal? switch-prev previous)
                   (search-rec current switch-next-left)
                   (search-rec current switch-next-right))
                 (when (equal? switch-next-left previous) (search-rec current switch-prev))
                 (when (equal? switch-next-right previous) (search-rec current switch-prev))))
              ((eq? (car current) 'segment)
               (let* ((curr (cdr current))
                      (segment-prev (send railway get-segment-prev curr))
                      (segment-next (send railway get-segment-next curr)))
                 (when (equal? segment-prev previous) (search-rec current segment-next))
                 (when (equal? segment-next previous) (search-rec current segment-prev))))
              (else (error "train > railway element does not exist" current))))
      (search-rec (cons 'dblock current-dblock)
                  (send railway get-detection-block-next current-dblock))
      (search-rec (cons 'dblock current-dblock)
                  (send railway get-detection-block-prev current-dblock))
      (newline) (display "possible locations: ") (displayln next-dblocks-list)
      next-dblocks-list)
    
    (define (on-next-location? occupied-blocks)
      (display "occupied blocks") (displayln occupied-blocks)
      (when (null? possible-next-locations)
        (set! possible-next-locations (search-reachable-dblocks railway current-location (λ (x) (void)) (λ (x) (void)) (λ (x) (void)))))
      (ormap (λ (possible-next) (member (cdr possible-next) occupied-blocks))
             possible-next-locations))

    ;
    ; get-location :: get the location on track of the train
    ;
    ; @returns symbol :: location of the train
    ;
    (define/public (get-location)
      (let* ((occupied-dblocks (send connection get-occupied-detection-blocks))
             (position-known? (member current-location occupied-dblocks)))
        (display "''position-known?'': ") (displayln position-known?)
        (if position-known?
            (begin
              (unless (null? possible-next-locations)
                (set! possible-next-locations '()))
              current-location)
            (begin
              (let ((on-next? (on-next-location? occupied-dblocks)))
                (display "''on-next-location?'': ") (displayln on-next?)
                (when on-next? (set! current-location (car on-next?))))
              current-location))))

    ;
    ; get-next-location :: get the next location on track of the train
    ;
    ; @returns symbol :: next location of the train
    ;
    (define/public (get-next-location) current-location)

    ;
    ; get-train-speed :: get the speed of the train, represented by a number
    ;
    ; @returns number :: speed of the train
    ;
    (define/public (get-train-speed)
      (send connection get-loco-speed id))

    ;
    ; set-train-speed! :: change the speed of the train, only when it isn't locked by automatic mode
    ;
    ; @param new-position new-speed :: the new speed of the train
    ;
    (define/public (set-train-speed! new-speed)
      (if unlocked?
          (begin
            (send connection set-loco-speed! id new-speed)
            (log-event "Method set-train-speed! called"
                       (string-append "changed speed to " (number->string new-speed))))
          (log-event "Method set-train-speed! called"
                     (string-append "ACCESS DENIED : train is locked to route"))))
    ;
    ; emergency-stop! :: always sets the train speed to 0, regardless of locks or modes
    ;
    (define/public (emergency-stop!)
      (send connection set-loco-speed! id 0)
      (log-event "Method emergency-stop! called"
                 (string-append "EMERGENCY STOP PRESSED")))

    ;
    ; follow-route :: change the speed of the train, only when it isn't locked by automatic mode
    ;
    ; @param new-position route :: the route to follow as a string with "->" between.
    ;
    (define/public (follow-route route)
      (send this lock!)
      (set! route route)
      (log-event "Method follow-route called" (string-append "moving along route '" route))
      (void))
    
    ))