#lang racket

(provide TRACK)

(define TRACK
  '((detection-block
    ;(name cl-in cl-out)
     (D1   T2    D2    )
     (D2   D1    D3    )
     (D3   D2    D4    )
     (D4   D3    S1    )
     (D5   T1    D6    )
     (D6   S2    D1    )
     (D7   S3    U4    )
     (D8   S3    D9    )
     (D9   D8    NIL   )
     )
    (segment
    ;(name cl-in cl-out)
     (U1   S1    T1    )
     (U2   S1    T3    )
     (U3   D6    S2    )
     (U4   D7    S2    )
     (T1   U1    D5    )
     (T2   S2    D1    )
     (T3   U2    S3    )
     )
    (switch
    ;(name in out1 out2 )
     (S1   D4 U1   U2   )
     (S2   T2 U3   U4   )
     (S3   T3 D7   D8   )
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