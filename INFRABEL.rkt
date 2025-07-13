#lang racket
(require "infrabel/startup.rkt"
         "railway/gui-tab-panels/tab-crossings-lights.rkt"
         "railway/gui-tab-panels/tab-segments-detection-blocks.rkt"
         "railway/gui-tab-panels/tab-switches.rkt"
         "railway/gui-tab-panels/tab-trains.rkt"
         "track/interface.rkt"
         "track/simulator/graphics.rkt"
         "track/simulator/simulator.rkt")

(define tab-panels (list (cons " Signals " tab-crossings-lights)
                         (cons "  Rails  " tab-segments-detection-blocks)
                         (cons "Switches " tab-switches)
                         (cons " Trains  " tab-trains)))
(define sim-graphics (cons set-panel window%))

(define infrabel (start-infrabel track% tab-panels sim-graphics))

;; Run this file to startup Infrabel
