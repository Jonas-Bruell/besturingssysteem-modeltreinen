#lang racket
(require "railway/interface.rkt")
(require "track/interface.rkt")
(define track (new track%))
(send track set-version! "hardware")
(send track config 'sim 'hardware)
(send track start)
(define railway (new railway% (track track)))