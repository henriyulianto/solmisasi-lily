%%% solmisasi-key-signature-summary.ily
%%% A solmisasi-lily extension
%%% By Henri Yulianto, 2020


#(define (get-header-property-value sym)
   "Get key signature summary from header props."
   (assoc-ref (ly:module->alist $defaultheader) sym))

#(define (set-header-property-value sym val)
   "Set the value of key signature summary in header props."
   (module-define! $defaultheader sym val))

#(define updateKeySignatureSummary
   (define-void-function (music sym) (ly:music? symbol?)
     "Update key signature summary in header."
     (let* ((key-list (list))
            (key-str #f)
            (key-signature-summary (get-header-property-value sym)))
       (music-map
        (lambda (m)
          (if (music-is-of-type? m 'key-change-event)
              (let ((solmisasi-key-sig (ly:music-property m 'solmisasi-key-sig)))
                (set! key-str
                      (if (pair? solmisasi-key-sig)
                          (format #f "~a=~a"
                                  (car solmisasi-key-sig)
                                  (get-key-sig-string (cdr solmisasi-key-sig)))
                          #f
                          ))
                (if (and key-str
                         (not (member key-str key-list)))
                    (set! key-list
                          (append key-list (list key-str))))
                ))
          m)
        music)
       (if (positive? (length key-list))
           (if (or (not key-signature-summary)
                   (> (length key-list)
                      (length (string-split (key-signature-summary) #\;))))
               (set-header-property-value sym (string-join key-list "; "))))
       ))
   )

