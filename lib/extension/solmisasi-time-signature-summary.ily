%%% solmisasi-time-signature-summary.ily
%%% A solmisasi-lily extension
%%% By Henri Yulianto, 2020

#(define (get-header-property-value sym)
   "Get key signature summary from header props."
   (assoc-ref (ly:module->alist $defaultheader) sym))

#(define (set-header-property-value sym val)
   "Set the value of key signature summary in header props."
   (module-define! $defaultheader sym val))

#(define updateTimeSignatureSummary
   (define-void-function (music sym) (ly:music? symbol?)
     "Update time signature summary in header."
     (let* ((time-list (list))
            (time-str #f)
            (time-signature-summary (get-header-property-value sym)))
       (music-map
        (lambda (m)
          (if (music-is-of-type? m 'time-signature-music)
              (let ((solmisasi-time-sig (ly:music-property m 'solmisasi-time-sig)))
                (set! time-str
                      (if (pair? solmisasi-time-sig)
                          (format #f "~a/~a"
                                  (car solmisasi-time-sig)
                                  (cdr solmisasi-time-sig))
                          #f
                          ))
                (if (and time-str
                         (not (member time-str time-list)))
                    (set! time-list
                          (append time-list (list time-str))))
                ))
          m)
        music)
       (if (positive? (length time-list))
           (if (or (not time-signature-summary)
                   (> (length time-list)
                      (length (string-split (time-signature-summary) #\;))))
               (set-header-property-value sym (string-join time-list "; "))))
       ))
   )

