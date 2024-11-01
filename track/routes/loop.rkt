#lang racket

(provide TRACK)

(define TRACK
  '((block
     (detection-block
     ;(name cl-in cl-out)
      (D1   T2    D2    )
      (D2   D1    D3    )
      (D3   D2    D4    )
      (D4   D3    D5    )
      (D5   D4    T1    )
      (D6   T1    D7    )
      (D7   D6    D8    )
      (D8   D7    T2    )
      )
     (segment
     ;(name cl-in cl-out)
      (T1   D5    D6    )
      (T2   D8    D1    )
      )
     )
    (switch
    ;(name in    out1 out2 )
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