;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> track/interface.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "config.rkt"
         (prefix-in sim: "simulator/interface.rkt")
         (prefix-in hw:  "hardware-library/interface.rkt"))

(provide track%)

(define track%
  (class object%
    (init-field (architecture 'undefined))
    (init-field (version 'undefined))
    (super-new)

    ;
    ; Higher level concepts that track does not understand
    ;
    (define/public (get-state id) (void))
    (define/public (set-state! id new-state) (void))
    (define/public (get-position id) (void))
    
    ;
    ; Track info getters
    ;
    (define/public (get-architecture) architecture)
    (define/public (get-version) version)
    (define/public (get-versions)
      (map (λ (s) (let ((l (string-length s))) (substring s 0 (- l 4))))
           (filter (λ (str) (string-contains? str ".rkt"))
                   (map path->string (directory-list TRACKS_LIST)))))
    (define/public (get-track-info)
      (dynamic-require (string-append "track/routes/" version ".rkt") 'TRACK))
    (define/public (get-train-info) TRAINS_LIST)

    ;
    ; Configuration of track
    ;
    (define/public (config! architecture-setup version-setup)
      (set! architecture architecture-setup)
      (set! version version-setup)
      (case architecture-setup
        ('hw (void))
        ('sim (case (string->symbol version)
                ('hardware (sim:setup-hardware))
                ('straight (sim:setup-straight))
                ('straight-with-switch (sim:setup-straight-with-switch))
                ('loop (sim:setup-loop))
                ('loop-and-switches (sim:setup-loop-and-switches))))))

    ;
    ; translation layers to simulator / hardware
    ;
    (define/public (start)
      ((λ (method) method)
       (case architecture
         ('sim (sim:start))
         ('hw  (hw:start))
         )))

    (define/public (stop)
      ((λ (method) method)
       (case architecture
         ('sim (sim:stop))
         ('hw  (hw:stop))
         )))

    ; train
    (define/public (add-loco train-id previous-segment-id current-segment-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:add-loco train-id previous-segment-id current-segment-id))
         ('hw (hw:add-loco train-id previous-segment-id current-segment-id))
         )))

    (define/public (get-loco-speed train-id)
      ((λ (method) method)
       (case architecture
         ('sim (inexact->exact (/ (sim:get-loco-speed train-id)
                                  SIMULATOR_SPEED_SCALAR)))
         ('hw (hw:get-loco-speed train-id))
         )))

    (define/public (set-loco-speed! train-id speed)
      ((λ (method) method)
       (case architecture
         ('sim (sim:set-loco-speed! train-id (* speed SIMULATOR_SPEED_SCALAR)))
         ('hw (hw:set-loco-speed! train-id speed))
         )))

    ; detection blocks
    (define/public (get-detection-block-ids)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-detection-block-ids))
         ('hw (hw:get-detection-block-ids))
         )))

    (define/public (get-occupied-detection-blocks)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-occupied-detection-blocks))
         ('hw (hw:get-occupied-detection-blocks))
         )))

    ; switches
    (define (railway->track position)
      (case position
        ('left 1)
        ('right 2)))
    
    (define (track->railway position)
      (case position
        ('1 'left)
        ('2 'right)))
    
    (define/public (get-switch-ids)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-switch-ids))
         ('hw (hw:get-switch-ids))
         )))

    (define/public (get-switch-position switch-id)
      ((λ (method) method)
       (track->railway
        (case architecture
          ('sim (sim:get-switch-position switch-id))
          ('hw (hw:get-switch-position switch-id))
          ))))

    (define/public (set-switch-position! switch-id position)
      ((λ (method) method)
       (let ((pos (railway->track position)))
         (case architecture
           ('sim (sim:set-switch-position! switch-id pos))
           ('hw (hw:set-switch-position! switch-id pos))
           ))))

    ; crossings
    (define/public (set-crossing-position! crossing-id position)
      ((λ (method) method)
       (case position
         ('open (case architecture
                  ('sim (sim:open-crossing! crossing-id))
                  ('hw (hw:open-crossing! crossing-id))))
         ('closed (case architecture
                    ('sim (sim:close-crossing! crossing-id))
                    ('hw (hw:close-crossing! crossing-id))))
         )))

    ; lights
    (define/public (set-sign-code! sign-id code)
      ((λ (method) method)
       (case architecture
         ('sim (sim:set-sign-code! sign-id code))
         ('hw (hw:set-sign-code! sign-id code))
         )))
    ))