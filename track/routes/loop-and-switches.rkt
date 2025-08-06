#lang racket

(provide TRACK)

(define TRACK
  '((detection-block
    ;(name clockwise-in   clockwise-out )
     (D1   (segment . T2) (dblock  . D2))
     (D2   (dblock  . D1) (dblock  . D3))
     (D3   (dblock  . D2) (dblock  . D4))
     (D4   (dblock  . D3) (switch  . S1))
     (D5   (segment . T1) (dblock  . D6))
     (D6   (switch  . S2) (dblock  . D1))
     (D7   (switch  . S3) (segment . U4))
     (D8   (switch  . S3) (dblock  . D9))
     (D9   (dblock  . D8) NIL           )
     )
    (segment
    ;(name clockwise-in   clockwise-out )
     (U1   (switch  . S1) (segment . T1))
     (U2   (switch  . S1) (segment . T3))
     (U3   (dblock  . D6) (switch  . S2))
     (U4   (dblock  . D7) (switch  . S2))
     (T1   (segment . U1) (dblock  . D5))
     (T2   (switch  . S2) (dblock  . D1))
     (T3   (segment . U2) (switch  . S3))
     )
    (switch
    ;(name clockwise-in   clockwise-out )
     (S1   (dblock  . D4) (segment . U1) (segment . U2))
     (S2   (segment . T2) (segment . U3) (segment . U4))
     (S3   (segment . T3) (dblock  . D7) (dblock  . D8))
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