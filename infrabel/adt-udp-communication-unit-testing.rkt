#lang racket
(require "adt-udp-communication.rkt")

;; hardware-testing
(define hardware-testing
  (make-udp-communication 'sim 'hardware))
(hardware-testing 'start)
(thread ...)
(hardware-testing 'stop)

;(define straight-testing
;  (make-udp-communication 'sim 'straight))
;
;
;
;(define straight-with-switch-testing
;  (make-udp-communication 'sim 'straight-with-switch))
;
;
;
;(define loop-testing
;  (make-udp-communication 'sim 'loop))
;
;
;
;(define loop-and-switches-testing
;  (make-udp-communication 'sim 'loop-and-switches))
;
;
