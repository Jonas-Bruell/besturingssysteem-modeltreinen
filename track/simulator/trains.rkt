#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;               - Youri Coppens: AI Lab: VUB - 2023
;
;       trains.rkt
;
;       Internal representation of trains
;

(require racket/class)

(require (only-in "railway.rkt" the-railway))

(provide add-train remove-train remove-all-trains get-train-speed set-train-speed! set-train-color! get-train-detection-blocks
         TRAINS)

(define TRAINS '())

(define point%
  (class object%
    (super-new)
    (init-field previous-segment current-segment distance)
    (field [p '(0 . 0)])
    (define/public (move delta)
      (set!-values (current-segment previous-segment distance p)
                   (send current-segment move-along-track previous-segment (+ distance delta))))
    (define/public (inverse-direction)
      (set! distance (- (send current-segment get-length previous-segment) distance))
      (set! previous-segment (send current-segment get-next-track previous-segment)))))

(define train%
  (class object%
    (super-new)
    (init-field id previous-segment current-segment speed pen-id)
    (define start-p (make-object point% previous-segment current-segment 100))
    (define end-p (make-object point% previous-segment current-segment 300))

    (define/public (move time)
      (let ((delta (* (abs speed) time)))
        (send start-p move delta)
        (send end-p move delta)))

    (define/public (inverse-direction)
      (send start-p inverse-direction)
      (send end-p inverse-direction))

    (define/public (get-segments)
      (remove-duplicates (map (lambda (p) (get-field current-segment p)) (list start-p end-p)); both points could be on the same segment
                         eq?))

    (define/public (draw window dc-id)
      (let* ([p1 (get-field p start-p)]
             [p2 (get-field p end-p)]
             [xm (/ (+ (car p1) (car p2)) 2)]
             [ym (/ (+ (cdr p1) (cdr p2)) 2)])
        (send window draw-line dc-id pen-id (car p1) (cdr p1) (car p2) (cdr p2))
        (send window draw-label (symbol->string id) dc-id 'mono-bold 'black xm ym)
        ))))

(define (add-train id previous-segment starting-segment)
  (set! TRAINS (cons (make-object train% id (send the-railway find-segment previous-segment) (send the-railway find-segment starting-segment) 0 'orange-fat) TRAINS)))

(define (remove-train id)
  (set! TRAINS (remf (lambda (train) (equal? (get-field id train) id)) TRAINS)))

(define (remove-all-trains)
  (set! TRAINS '()))

(define (find-train id)
  (let ((train
         (findf (lambda (train)
                  (equal? (get-field id train) id))
                TRAINS)))
    (if train
        train
        (error "Train not found: " id))))

(define (get-train-speed id)
  (get-field speed (find-train id)))

(define (set-train-speed! id speed)
  (let* ((train (find-train id))
         (old-speed (get-field speed train)))
    (when (xor (< old-speed 0) (< speed 0))
      (send train inverse-direction))
    (set-field! speed train speed)))

(define (set-train-color! id color)
  (set-field! color (find-train id) color))

(define (get-train-detection-blocks)
  (remove-duplicates (filter-map (lambda (segment)
                                   (and (send the-railway find-detection-block (get-field id segment))
                                        (get-field id segment)))
                                 (flatten (map (lambda (train)
                                                 (send train get-segments))
                                               TRAINS)))
                     eq?))
