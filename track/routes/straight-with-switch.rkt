#lang racket

(provide TRACK)

(define TRACK
  '((detection-block
    ;(name clockwise-in   clockwise-out )
     (D1   NIL            (switch  . S1))
     (D4   (switch  . S1) (dblock  . D5))
     (D5   (dblock  . D4) NIL           )
     (D6   (switch  . S1) (dblock  . D7))
     (D7   (dblock  . D6) NIL           ) 
     )
    (segment
    ;(name clockwise-in   clockwise-out )
     )
    (switch
    ;(name in            out1          out2         )
     (S1   (dblock . D1) (dblock . D4) (dblock . D6))
     )
    (switch*
     (threeway
     ;(name s1  s2  in  out1 out2 out3)
      )
     (cross
     ;(name s1  s2  cl-in1 cl-in2 cl-out1 cl-out2)
      )
     )
    (crossing
    ;(name)
     )
    (light
    ;(name)
     )
    ))