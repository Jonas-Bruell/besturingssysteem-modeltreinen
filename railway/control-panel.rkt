;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       >>> railway/control-panel <<<                        ;;
;;                      programmeerproject 2,  2023-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 6 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require "config.rkt")
(provide control-panel%)

(define control-panel%
  (class frame%
    (init-field railway)
    (init-field track-info)
    (super-new (label "Railway Controle Panel")
               (width SERVER_A&D_WIDTH)
               (height SERVER_A&D_HEIGHT))

    ;
    ; update!
    ;
    (define callbacks-list '())
    (define (add-to-callbacks-list id callback)
      (append callbacks-list (list id callback)))
    (define/public (update! id)
      (let ((elements-tail (member id callbacks-list)))
        (if elements-tail
            ((cadr elements-tail))
            (error "railway/control-panel.rkt - update! - id not found: " id))))

    ;
    ; define all panels & panes
    ;
    (let* ((menu-bar (new menu-bar% (parent this)))
           (menu-bar-menu (new menu% (label "Menu") (parent menu-bar)))

           (trains-panel (new horizontal-pane% (parent this)))
           (emergency-stop-pane (new vertical-pane% (parent trains-panel)))
           (trains-pane (new vertical-pane% (parent trains-panel)))
           
           (tab-panel (new tab-panel% (parent this) (choices '("Switches" "Other"))))

           (left-column (new vertical-pane% (parent this) (min-width (/ SERVER_A&D_WIDTH 2))))
           (right-column (new vertical-pane% (parent this) (min-width (/ SERVER_A&D_WIDTH 2))))
           )
      
      (new menu-item% (label "stop") (parent menu-bar-menu) (callback (λ (t e) (send railway stop))))

      ;
      ; FILL TRAINS PANE
      ;
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-pane)))
             (message (new message% (label "T-3") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-3 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-3 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-pane)))
             (message (new message% (label "T-5") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-5 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-5 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-pane)))
             (message (new message% (label "T-7") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-7 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-7 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-pane)))
             (message (new message% (label "T-9") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-9 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-9 0) (send slider set-value 0)))))




      (void)
      )))
#|
      
    (define horizontal-group-box-panel%
      (class horizontal-pane%
        (init label parent)
        (super-new
         (parent (new group-box-panel% (label label) (parent parent)))
         (alignment '(center center)))))

    (define (wrap-callback callback change-colors)
      (λ (t e)
        (callback t e)
        (change-colors)))



    ;; defining and filling of panels
    (let* ((upper-columns
            (new horizontal-pane% (parent this)))

           (left-column
            (new vertical-pane% (parent upper-columns)))
           (right-column
            (new vertical-pane% (parent upper-columns)))

           (switches-group
            (new horizontal-group-box-panel%
                 (label "SWITCHES")
                 (parent left-column)
                 (min-width (/ SERVER_A&D_WIDTH 2))))
           (switches-panel-L
            (new vertical-panel% (parent switches-group)))
           (switches-panel-R
            (new vertical-panel% (parent switches-group)))
           
           (emergency-panel
            (new group-box-panel%
                 (label "EMERGENCY")
                 (parent right-column)
                 (min-width (/ SERVER_A&D_WIDTH 2))))

           (switches*-panel
            (new group-box-panel% (label "SPECIAL SWITCHES") (parent right-column)))
           (crossings-panel
            (new group-box-panel% (label "CROSSINGS") (parent right-column)))
           (lights-panel
            (new group-box-panel% (label "LIGHTS") (parent right-column)))

           
           (blocks-group
            (new group-box-panel% (label "SECTION BLOCKS") (parent this)))
           (blocks-panel-1
            (new horizontal-panel% (parent blocks-group)))
           (blocks-panel-2
            (new horizontal-panel% (parent blocks-group)))
           (blocks-panel-U
            (new horizontal-panel% (parent blocks-group)))
           (trains-panel
            (new group-box-panel% (label "TRAINS") (parent this)))
           

           )

      
      
      (define (fill-panel panel callbacks-all-objects)
        ; callbacks-all-objects
        ; = '((name (label . callback) (label . callback) ...)) ...)
        (let fill ((callbacks-all-objects callbacks-all-objects))
          (let ((local-panel (new horizontal-panel% (parent panel)))
                (callbacks-this-object (car callbacks-all-objects)))
            (new message%
                 (label (symbol->string (car callbacks-this-object)))
                 (parent local-panel))
            (let make-buttons ((callbacks (cdr callbacks-this-object)))
              (new button%
                   (label (caar callbacks))
                   (parent local-panel)
                   (callback (cdar callbacks)))
              (unless (null? (cdr callbacks))
                (make-buttons (cdr callbacks))))
            (unless (null? (cdr callbacks-all-objects))
              (fill (cdr callbacks-all-objects))))))

      ; Emergency
      (fill-panel emergency-panel
                  (list
                   (list 'stop (cons "stop sim" (λ (t e) (send railway stop))))
                   (list 'emergency
                         (cons "stop trains"
                               (λ (t e)
                                 (send railway set-train-speed! 'T-3 0)
                                 (send railway set-train-speed! 'T-5 0)
                                 (send railway set-train-speed! 'T-7 0)
                                 (send railway set-train-speed! 'T-9 0))))
                   ))
      
      ;;
      ;; FILL SWITCH
      ;;
      (define switch-panel%
        (class horizontal-group-box-panel%
          (init-field parent
                      name
                      in
                      check
                      button1 button2
                      out1 out2)
          (super-new (label name) (parent parent))
          ;; color active track
          (define (change-colors)
            (let ((postion (check)))
              (cond ((eq? postion 'left)
                     (send out-label1 set-color COLOR_FREE)
                     (send out-label2 set-color COLOR_OCCUPIED))
                    ((eq? postion 'right)
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_FREE)))))
          ;; panes
          (define in-pane
            (new pane% (parent this)))
          (define button-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          (define out-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          ;; content
          (define in-label
            (new message% (label in) (parent in-pane)))
          (define out-label1
            (new message% (label out1) (parent out-pane)))
          (define out-label2
            (new message% (label out2) (parent out-pane)))
          (define the-button1
            (new button% (label "pos-1") (parent button-pane)
                 (callback (wrap-callback button1 change-colors))))
          (define the-button2
            (new button% (label "pos-2") (parent button-pane)
                 (callback (wrap-callback button2 change-colors))))
          (send in-label set-color COLOR_FREE)
          (change-colors)))
      (let fill
        ((switch
          (map (λ (l) (list l
                            (λ () (send railway get-switch-position (car l)))
                            (λ (t e)
                              (send railway set-switch-position! (car l) 'left))
                            (λ (t e)
                              (send railway set-switch-position! (car l) 'right))))
               (cdr (assoc 'switch track-info))))
         (index 0))
        (new switch-panel%
             (parent (if (< index 10)
                         switches-panel-L
                         switches-panel-R))
             (name (symbol->string (caar (car switch))))
             (in (symbol->string (cadar (car switch))))
             (check (cadr (car switch)))
             (button1 (caddr (car switch)))
             (button2 (cadddr (car switch)))
             (out1 (symbol->string (caddr (caar switch))))
             (out2 (symbol->string (cadddr (caar switch)))))
        
        (unless (null? (cdr switch))
          (fill (cdr switch) (+ index 1))))

      ;;
      ;; FILL 3-WAY-SWITCH
      ;;
      (define three-way-switch-panel%
        (class horizontal-group-box-panel%
          (init-field parent
                      name
                      in
                      check1 check2
                      button1 button2 button3
                      out1 out2 out3)
          (super-new (label name) (parent parent))
          ;; color active track
          (define (change-colors)
            (let ((position1 (check1))
                  (position2 (check2)))
              (cond ((eq? position1 'left)
                     (send out-label1 set-color COLOR_FREE)
                     (send out-label2 set-color COLOR_OCCUPIED)
                     (send out-label3 set-color COLOR_OCCUPIED))
                    ((and (eq? position2 'right))
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_FREE)
                     (send out-label3 set-color COLOR_OCCUPIED))
                    ((and (eq? position2 'right))
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_OCCUPIED)
                     (send out-label3 set-color COLOR_FREE)))))
          ;; panes
          (define in-pane
            (new pane% (parent this)))
          (define button-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          (define out-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          ;; content
          (define in-label
            (new message% (label in) (parent in-pane)))
          (define out-label1
            (new message% (label out1) (parent out-pane)))
          (define out-label2
            (new message% (label out2) (parent out-pane)))
          (define out-label3
            (new message% (label out3) (parent out-pane)))
          (define the-button1
            (new button% (label "pos-1") (parent button-pane)
                 (callback (wrap-callback button1 change-colors))))
          (define the-button2
            (new button% (label "pos-2") (parent button-pane)
                 (callback (wrap-callback button2 change-colors))))
          (define the-button3
            (new button% (label "pos-3") (parent button-pane)
                 (callback (wrap-callback button3 change-colors))))
          (send in-label set-color COLOR_FREE)
          (change-colors)))
      (let ((switch
             (map (λ (l)
                    (list l
                          (λ () (send railway get-switch-position (cadr l)))
                          (λ () (send railway get-switch-position (caddr l)))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right)
                            (send railway set-switch-position! (caddr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right)
                            (send railway set-switch-position! (caddr l) 'right))))
                  (cdr (assoc 'threeway
                              (cdr (assoc 'switch*
                                          track-info)))))))
        (new three-way-switch-panel%
             (parent switches*-panel)
             (name (symbol->string (caar (car switch))))
             (in (symbol->string (car (cdddar (car switch)))))
             (check1 (cadr (car switch)))
             (check2 (cadr (cdar switch)))
             (button1 (caddr (cdar switch)))
             (button2 (cadddr (cdar switch)))
             (button3 (cadddr (cddar switch)))
             (out1 (symbol->string (cadr (cdddar (car switch)))))
             (out2 (symbol->string (caddr (cdddar (car switch)))))
             (out3 (symbol->string (cadddr (cdddar (car switch))))))
        )

      ;;
      ;; FILL CROSS-SWITCH
      ;;
      (define cross-switch-panel%
        (class horizontal-group-box-panel%
          (init-field parent
                      name
                      in1 in2
                      check1 check2
                      button1 button2 button3 button4
                      out1 out2)
          (super-new (label name) (parent parent))
          ;; color active track
          (define (change-colors)
            (let ((position1 (check1))
                  (position2 (check2)))
              (cond ((eq? position1 'left)
                     (send in-label1 set-color COLOR_FREE)
                     (send in-label2 set-color COLOR_OCCUPIED))
                    ((eq? position1 'right)
                     (send in-label1 set-color COLOR_OCCUPIED)
                     (send in-label2 set-color COLOR_FREE)))
              (cond ((eq? position2 'left)
                     (send out-label1 set-color COLOR_FREE)
                     (send out-label2 set-color COLOR_OCCUPIED))
                    ((eq? position2 'right)
                     (send out-label1 set-color COLOR_OCCUPIED)
                     (send out-label2 set-color COLOR_FREE)))))
          ;; panes
          (define in-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          (define button-pane
            (new horizontal-pane% (parent this) (alignment '(center center))))
          (define button-pane-L
            (new vertical-pane% (parent button-pane) (alignment '(center center))))
          (define button-pane-R
            (new vertical-pane% (parent button-pane) (alignment '(center center))))
          (define out-pane
            (new vertical-pane% (parent this) (alignment '(center center))))
          ;; content
          (define in-label1
            (new message% (label in1) (parent in-pane)))
          (define in-label2
            (new message% (label in2) (parent in-pane)))
          (define out-label1
            (new message% (label out1) (parent out-pane)))
          (define out-label2
            (new message% (label out2) (parent out-pane)))
          (define the-button1
            (new button% (label "pos-1") (parent button-pane-L)
                 (callback (wrap-callback button1 change-colors))))
          (define the-button2
            (new button% (label "pos-2") (parent button-pane-L)
                 (callback (wrap-callback button2 change-colors))))
          (define the-button3
            (new button% (label "pos-1") (parent button-pane-R)
                 (callback (wrap-callback button3 change-colors))))
          (define the-button4
            (new button% (label "pos-2") (parent button-pane-R)
                 (callback (wrap-callback button4 change-colors))))
          (change-colors)))
      (let ((switch
             (map (λ (l)
                    (list l
                          (λ () (send railway get-switch-position (cadr l)))
                          (λ () (send railway get-switch-position (caddr l)))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (cadr l) 'right))
                          (λ (t e)
                            (send railway set-switch-position! (caddr l) 'left))
                          (λ (t e)
                            (send railway set-switch-position! (caddr l) 'right))))
                  (cdr (assoc 'cross
                              (cdr (assoc 'switch*
                                          track-info)))))))
        (new cross-switch-panel%
             (parent switches*-panel)
             (name (symbol->string (caar (car switch))))
             (in1 (symbol->string (car (cdddar (car switch)))))
             (in2 (symbol->string (cadr (cdddar (car switch)))))
             (check1 (cadr (car switch)))
             (check2 (cadr (cdar switch)))
             (button1 (caddr (cdar switch)))
             (button2 (cadddr (cdar switch)))
             (button3 (cadddr (cddar switch)))
             (button4 (cadddr (cdddar switch)))
             (out1 (symbol->string (caddr (cdddar (car switch)))))
             (out2 (symbol->string (cadddr (cdddar (car switch)))))))

      ;;
      ;; SEGMENTS & SWITCHES
      ;;
      (define status%
        (class pane%
          (init-field parent
                      name
                      callback
                      )
          (super-new
           (parent parent)
           )
    
          (define group-box (new group-box-panel% (label name) (parent this)))
           
          (define message
            (new message% (label (callback)) (parent group-box) (auto-resize #t)))

          (define/public (set-label msg)
            (send message set-label msg))
          (define/public (set-color clr)
            (send message set-color clr))
          ))
      #|
      ; FILL D-BLOCKS
      (let fill
        ((block
          (map (λ (l)
                 (list l (λ () (format "~a" (send railway
                                                  get-detection-block-state
                                                  (car l))))))
               (cdr (assoc 'detection-block track-info))))
         (index 0))
        (let ((status (new status%
                           (parent (if (< index 10)
                                       blocks-panel-1
                                       blocks-panel-2))
                           (name (symbol->string (caaar block)))
                           (callback (cadar block)))))
          (set! callbacks-list
                (append callbacks-list
                        (list (cons status (cadar block))))))
        (unless (null? (cdr block))
          (fill (cdr block) (+ index 1))))

      ; FILL BLOCKS
      (let fill
        ((block (map (λ (l) (list l (λ () (format "~a" (send railway
                                                             get-segment-state
                                                             (car l))))))
                     (cdr (assoc 'segment track-info)))))
        (new status%
             (parent blocks-panel-U)
             (name (symbol->string (caaar block)))
             (callback (cadar block)))
        (unless (null? (cdr block))
          (fill (cdr block))))
|#
      ; FILL CROSSINGS
      (define crossing-panel%
        (class horizontal-group-box-panel%
          (init-field parent name)
          (super-new (label name) (parent parent))

          (new button% (label "open") (parent this))
          (new message% (label "opened") (parent this))
          (new button% (label "close") (parent this))
    
          ))
      (fill-panel crossings-panel
                  (list
                   (list 'C-1
                         (cons "open" (λ (t e) (send railway set-crossing-position! 'C-1 'open)))
                         (cons "close" (λ (t e) (send railway set-crossing-position! 'C-1 'closed))))
                   (list 'C-2
                         (cons "open" (λ (t e) (send railway set-crossing-position! 'C-2 'open)))
                         (cons "close" (λ (t e) (send railway set-crossing-position! 'C-2 'closed))))
                   ))

      ; FILL LIGHTS
      (define light-panel%
        (class group-box-panel%
          (init-field parent name)
          (super-new (label name) (parent parent))

          ))
      (fill-panel lights-panel
                  (list
                   (list 'L-1
                         (cons "Hp0" (λ (t e) (send railway set-light-signal! 'L-1 'Hp0)))
                         (cons "Hp1" (λ (t e) (send railway set-light-signal! 'L-1 'Hp1)))
                         (cons "Hp0+Sh0" (λ (t e) (send railway set-light-signal! 'L-1 'Hp0+Sh0)))
                         (cons "Ks1+Zs3" (λ (t e) (send railway set-light-signal! 'L-1 'Ks1+Zs3))))
                   (list 'L-1
                         (cons "Ks2" (λ (t e) (send railway set-light-signal! 'L-1 'Ks2)))
                         (cons "Ks2+Zs3" (λ (t e) (send railway set-light-signal! 'L-1 'Ks2+Zs3)))
                         (cons "Sh1" (λ (t e) (send railway set-light-signal! 'L-1 'Sh1)))
                         (cons "Ks1+Zs3+Zs3v" (λ (t e) (send railway set-light-signal! 'L-1 'Ks1+Zs3+Zs3v))))
                   (list 'L-2
                         (cons "Hp0" (λ (t e) (send railway set-light-signal! 'L-2 'Hp0)))
                         (cons "Hp1" (λ (t e) (send railway set-light-signal! 'L-2 'Hp1)))
                         (cons "Hp0+Sh0" (λ (t e) (send railway set-light-signal! 'L-2 'Hp0+Sh0)))
                         (cons "Ks1+Zs3" (λ (t e) (send railway set-light-signal! 'L-2 'Ks1+Zs3))))
                   (list 'L-2
                         (cons "Ks2" (λ (t e) (send railway set-light-signal! 'L-2 'Ks2)))
                         (cons "Ks2+Zs3" (λ (t e) (send railway set-light-signal! 'L-2 'Ks2+Zs3)))
                         (cons "Sh1" (λ (t e) (send railway set-light-signal! 'L-2 'Sh1)))
                         (cons "Ks1+Zs3+Zs3v" (λ (t e) (send railway set-light-signal! 'L-2 'Ks1+Zs3+Zs3v))))
                   ))
      (define Hp0          'Hp0)
      (define Hp1          'Hp1)
      (define Hp0+Sh0      'Hp0+Sh0)
      (define Ks1+Zs3      'Ks1+Zs3)
      (define Ks2          'Ks2)
      (define Ks2+Zs3      'Ks2+Zs3)
      (define Sh1          'Sh1)
      (define Ks1+Zs3+Zs3v 'Ks1+Zs3+Zs3v)

      ; FILL TRAINS
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-panel)))
             (message (new message% (label "T-3") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-3 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-3 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-panel)))
             (message (new message% (label "T-5") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-5 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-5 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-panel)))
             (message (new message% (label "T-7") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-7 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-7 0) (send slider set-value 0)))))
      (let* ((horizontal-pane (new horizontal-pane% (parent trains-panel)))
             (message (new message% (label "T-9") (parent horizontal-pane)))
             (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-9 (send t get-value)))))))
        (new button% (label "stop") (parent horizontal-pane) (callback (λ (t e) (send railway set-train-speed! 'T-9 0) (send slider set-value 0)))))
)
  |#    
