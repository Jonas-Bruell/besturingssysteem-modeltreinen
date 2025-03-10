;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                               >>> udp.rkt <<<                              ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "config.rkt"
         (prefix-in sim: "simulator/interface.rkt")
         (prefix-in hw:  "hardware-library/interface.rkt"))
(provide track%)

(define architecture 'sim)

(define track%
  (class object%
    (init-field (version "undefined"))
    (super-new)

    (define/public (set-state! new-state) (void))
    ;(define/public (get-position new-state) (void))

    (define/public (get-version) version)
    
    (define/public (get-versions)
      (map (λ (s) (let ((l (string-length s))) (substring s 0 (- l 4))))
           (filter (λ (str) (string-contains? str ".rkt"))
                   (map path->string (directory-list TRACKS_LIST)))))

    (define/public (set-version! new-version) (set! version new-version))

    (define/public (get-meta-data)
      (dynamic-require (string-append "track/routes/" version ".rkt") 'TRACK))
      
    (define/public (config architecture-setup version)
      (set! architecture architecture-setup)
      (case architecture-setup
        ('hw (void))
        ('sim (case version
                ('hardware (sim:setup-hardware))
                ('straight (sim:setup-straight))
                ('straight-with-switch (sim:setup-straight-with-switch))
                ('loop (sim:setup-loop))
                ('loop-and-switches (sim:setup-loop-and-switches))))))

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

    (define/public (add-loco train-id previous-segment-id current-segment-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:add-loco train-id previous-segment-id current-segment-id))
         ('hw (hw:add-loco train-id previous-segment-id current-segment-id))
         )))

    (define/public (get-loco-speed train-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-loco-speed train-id))
         ('hw (/ (hw:get-loco-speed train-id) HARDWARE_SPEED_SCALAR))
         )))

    (define/public (set-loco-speed! train-id speed)
      ((λ (method) method)
       (case architecture
         ('sim (sim:set-loco-speed! train-id speed))
         ('hw (hw:set-loco-speed! train-id (* speed HARDWARE_SPEED_SCALAR)))
         )))

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

    (define/public (get-switch-ids)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-switch-ids))
         ('hw (hw:get-switch-ids))
         )))

    (define/public (get-switch-position switch-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:get-switch-position switch-id))
         ('hw (hw:get-switch-position switch-id))
         )))

    (define/public (set-switch-position! switch-id position)
      ((λ (method) method)
       (case architecture
         ('sim (sim:set-switch-position! switch-id position))
         ('hw (hw:set-switch-position! switch-id position))
         )))

    (define/public (open-crossing! crossing-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:open-crossing! crossing-id))
         ('hw (hw:open-crossing! crossing-id))
         )))

    (define/public (close-crossing! crossing-id)
      ((λ (method) method)
       (case architecture
         ('sim (sim:close-crossing! crossing-id))
         ('hw (hw:close-crossing! crossing-id))
         )))

    (define/public (set-sign-code! sign-id code)
      ((λ (method) method)
       (case architecture
         ('sim (sim:set-sign-code! sign-id code))
         ('hw (hw:set-sign-code! sign-id code))
         )))
    ))