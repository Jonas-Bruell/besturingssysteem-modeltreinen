;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                         >>> detection-block.rkt <<<                        ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "segment.rkt")
(provide detection-block%) ; (object symbol boolean -> class)

(define detection-block%
  (class segment%
    (super-new)
    (inherit-field connection id state)
    (define/public (is-occupied?)
      (not (not (member id (send connection get-occupied-detection-blocks)))))
    ))