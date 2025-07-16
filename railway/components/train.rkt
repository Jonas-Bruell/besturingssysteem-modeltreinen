;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> train.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
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
    (init-field add-to-log id connection previous current)

    (define unlocked? #t)
    (define route '())
    
    ; Adding train to track -- Only works on simulator, retuns false on hardware
    (send connection add-loco id previous current)

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

    ;
    ; get-location :: get the location on track of the train
    ;
    ; @returns symbol :: location of the train
    ;
    (define/public (get-location) current)

    ;
    ; get-next-location :: get the next location on track of the train
    ;
    ; @returns symbol :: next location of the train
    ;
    (define/public (get-next-location) current)

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