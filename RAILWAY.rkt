#lang racket
(require "track/interface.rkt")
(require "railway/interface.rkt")
(define track (new track%))
(send track set-version! "hardware")
(define railway (new railway% (track track)))
;(send railway get-detection-block-state '1-7)