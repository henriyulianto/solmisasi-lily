%% solmisasi-engraver.ily
%%
%% (Part of "solmisasi-lily" library for Lilypond)
%%
%% Copyright (C) 2016 - Henri Yulianto
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

#(define*-public key-is-changed #f)

#(define (male-vocal? instrName)
   (or (string-contains (string-downcase instrName) "laki")
       (string-contains (string-downcase instrName) "pria")
       (string-contains (string-downcase instrName) "tenor")
       (string-contains (string-downcase instrName) "bas")
       (string-contains (string-downcase instrName) "bariton")
       (string-contains (string-downcase instrName) "unisono tb")
       (string-contains (string-downcase instrName) "t.b.")))

#(define (vocal-instrument? instrName)
   (and (not (string-contains (string-downcase instrName) "iring"))
        (not (string-contains (string-downcase instrName) "piano"))))

#(define Solmisasi_note_head_engraver
   (make-engraver
    ((initialize trans)
     (solmisasi:log "- Engraving solmisasi notes in context ~a" (ly:translator-context trans)))
    (acknowledgers
     ((note-head-interface engraver grob source-engraver)
      ;(display (event-cause grob))(newline)
      ;; make sure \omit is not in effect (stencil is not #f)
      (if (ly:grob-property-data grob 'stencil)
          ;;(ly:message (_ "  [Solmisasi_*_engraver] NOTE HEAD ENGRAVING\n"))
          (let*
           ((staff-context (ly:translator-context engraver))
            (music-context (ly:translator-context source-engraver))
            (vocal-type-string
             (markup->string
              (ly:context-property staff-context 'shortInstrumentName "")))
            (instrument-name
             (markup->string
              (ly:context-property staff-context 'instrumentName "")))
            (event (event-cause grob))
            (solmisasi-dot-note? (ly:event-property event _DOT_NOTE_PROP #f))
            (solmisasi-rest? (ly:event-property event _REST_PROP #f))
            (cdr-chords (ly:event-property event 'cdr-chords #f))
            (dur-log (ly:grob-property grob 'duration-log))
            (base-markup
             (markup
              #:line
              (#:overlay
               (#:solmisasi
                (#:not-angka (ly:make-pitch 1 5) 0)
                #:solmisasi
                (#:not-angka (ly:make-pitch -1 5) 0)))))
            (stl-base (grob-interpret-markup grob base-markup))
            (stl-base-Y (ly:stencil-extent stl-base Y))
            (stl-base-height (interval-length stl-base-Y))
            (chord-gap 0.6)
            (chord-down-Y-pos (- 0 (* chord-gap 0.5) (* stl-base-height 0.4)))
            (chord-up-Y-pos (+ 0 (* chord-gap 0.5) (* stl-base-height 0.5)))
            (dot-circle-stencil (ly:stencil-translate
                                 (make-circle-stencil 0.22 0.001 #t)
                                 (cons 0.5 -0.25))))

           (if solmisasi-dot-note?
               (if (or (not cdr-chords) (null? cdr-chords))
                   (ly:grob-set-property! grob 'stencil dot-circle-stencil)
                   (let* ((dotstl dot-circle-stencil))
                     (for-each
                      (lambda (ch)
                        (set! dotstl
                              (ly:stencil-combine-at-edge
                               dotstl
                               1
                               1
                               dot-circle-stencil
                               (* 0.95 stl-base-height))
                              ) ; end set
                        ) ; end lambda
                      cdr-chords) ; end for-each
                     (ly:grob-set-property! grob 'stencil dotstl)
                     (ly:grob-set-property! grob 'Y-offset
                                            (- -0.2 (* 1.0 (+ (* 0.5 stl-base-height) chord-gap))))
                     ) ; end let
                   ) ; end if cdr-chords
               ;; else create number stencil based on the scale degree of the note
               ;; TODO: handle notes without pitches that aren't solmisasi-rests
               (let*
                ((tonic-pitch   				(ly:context-property staff-context 'tonic))
                 (tonic-number  				(ly:pitch-notename tonic-pitch))
                 (is-male-vocal?				(ly:context-property staff-context 'male-vocal))
                 (grob-pitch    				(ly:event-property event 'pitch))
                 (grob-pitch-solmisasi	(ly:event-property event 'pitch-solmisasi))
                 (grob-octave
                  (if (ly:pitch? grob-pitch-solmisasi)
                      (ly:pitch-octave grob-pitch-solmisasi) #f))
                 (grob-alter
                  (if (ly:pitch? grob-pitch-solmisasi)
                      (ly:pitch-alteration grob-pitch-solmisasi) #f))
                 (note-number
                  (if (ly:pitch? grob-pitch-solmisasi)
                      (ly:pitch-notename grob-pitch-solmisasi) #f))
                 (base-octave
                  (if (not solmisasi-rest?)
                      (if is-male-vocal? -1 0)
                      0))
                 (stl
                  (grob-interpret-markup grob
                                         (if solmisasi-rest?
                                             (markup
                                              #:line
                                              (#:lower
                                               0.5
                                               (#:solmisasi #:simple "0")))
                                             ; not rest
                                             (if (or (not cdr-chords) (null? cdr-chords))
                                                 ; single note
                                                 (markup
                                                  #:line
                                                  (#:lower
                                                   0.5
                                                   (#:solmisasi
                                                    (#:not-angka grob-pitch-solmisasi base-octave))))
                                                 ; chords
                                                 (let* ((cm
                                                         #{
                                                           \markup \fontsize #-0.4 {
                                                             \lower #0.5
                                                             \solmisasi {
                                                               %\with-dimensions-from #base-markup
                                                               %\hcenter-in
                                                               \not-angka #grob-pitch-solmisasi #base-octave
                                                             }
                                                         } #}))
                                                   (for-each
                                                    (lambda (ch)
                                                      (set! cm
                                                            #{
                                                              \markup {
                                                                \center-column {
                                                                  \vspace #chord-gap
                                                                  \overlay {
                                                                    \translate #(cons 0 chord-down-Y-pos) #cm
                                                                    \translate #(cons 0 chord-up-Y-pos)
                                                                    \with-dimensions-from \fontsize #-0.4 #base-markup
                                                                    \fontsize #-0.4 \lower #0.5 \solmisasi {
                                                                      \not-angka #(ly:music-property ch 'pitch-solmisasi) #base-octave
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            #})
                                                      ) ; end lambda
                                                    cdr-chords) ; end for-each
                                                   cm) ; end let
                                                 ) ; end if not cdr-chords
                                             )))
                 )
                ; set stencil
                (ly:grob-set-property! grob 'stencil stl)
                (ly:grob-set-property! grob 'Y-extent
                                       (ly:stencil-extent stl Y))
                (if cdr-chords
                    (begin
                     (ly:grob-set-property! grob 'stencil
                                            (ly:stencil-stack empty-stencil Y DOWN stl 0.5 0.3))
                     (ly:grob-set-property! grob 'Y-offset
                                            (* 0.5 (+ (* 0.5 stl-base-height) chord-gap)))
                     (ly:grob-set-property! grob 'Y-extent
                                            (cons
                                             (car (ly:stencil-extent stl Y))
                                             (- (cdr (ly:stencil-extent stl Y))
                                                (* 1.0 (+ (* 0.65 stl-base-height) chord-gap)))))
                     (ly:grob-set-property! grob 'springs-and-rods #t)
                     )
                    ) ; end if
                ))

           (if (eq? (ly:grob-property grob 'style) 'cross)
               (ly:grob-set-property! grob 'stencil
                                      (grob-interpret-markup grob
                                                             #{
                                                               \markup {
                                                                 \raise #0.2
                                                                 \musicglyph #"noteheads.s2cross"
                                                               }
                                                             #})))
           (if (eq? (ly:grob-property grob 'style) 'xcircle)
               (ly:grob-set-property! grob 'stencil
                                      (grob-interpret-markup grob
                                                             #{
                                                               \markup {
                                                                 \raise #0.2
                                                                 \musicglyph #"noteheads.s2xcircle"
                                                               }
                                                             #})))
           (if (eq? (ly:grob-property grob 'style) 'triangle)
               (ly:grob-set-property! grob 'stencil
                                      (grob-interpret-markup grob
                                                             #{
                                                               \markup {
                                                                 \raise #0.2
                                                                 \musicglyph #"noteheads.u1triangle"
                                                               }
                                                             #})))
           ))))))

#(define Solmisasi_rest_engraver
   (make-engraver
    ((initialize trans)
     (solmisasi:log "- Engraving solmisasi rests in context ~a" (ly:translator-context trans)))
    (acknowledgers
     ((rest-interface engraver grob source-engraver)
      ;; make sure \omit is not in effect (stencil is not #f)
      (if (ly:grob-property-data grob 'stencil)
          (ly:grob-set-property! grob 'stencil
                                 (grob-interpret-markup grob
                                                        #{ \markup \lower #0.5 "0" #}))))))
   )

#(define Solmisasi_key_engraver
   (let* ((current-key-sig (cons 1 (ly:make-pitch -6 0 0)))
          (last-event-mom (ly:make-moment 99999))
          (key-printed? #f))
     (make-engraver
      ((initialize trans)
       (solmisasi:log "- Engraving key-signatures in context ~a" (ly:translator-context trans)))
      (acknowledgers
       ((key-signature-interface engraver grob source-engraver)
        ;; make sure \omit is not in effect (stencil is not #f)
        (if (ly:grob-property-data grob 'stencil)
            ;;(ly:message (_ "  [Solmi;sasi_*_engraver] KEY SIGNATURE ENGRAVING\n"))
            (let*
             ((staff-context (ly:translator-context engraver))
              (context-id (ly:context-id staff-context))
              (context-name (ly:context-name staff-context))
              (event (event-cause grob))
              (current-moment (ly:context-current-moment staff-context))
              (solmisasi-key-sig
               (if event
                   (ly:event-property event 'solmisasi-key-sig)
                   current-key-sig))
              (last-pitch
               (if (and event
                        (vocal-instrument? context-id)
                        (not (equal? 'SolmisasiTimeAndKeySignature context-name)))
                   (ly:event-property event 'last-pitch-solmisasi #f)
                   #f))
              (print-ekuivalensi?
               (and last-pitch
                    (equal? 'SolmisasiStaff context-name)
                    (ly:moment<? ZERO-MOMENT current-moment)))
              (base-octave
               (if (male-vocal? context-id)
                   -1 0))
              (stl #f))
             ;--------------------------------------
             (set! key-is-changed
                   (or (equal? current-moment ZERO-MOMENT)
                       (not (equal? solmisasi-key-sig current-key-sig))))
             (if key-is-changed
                 (begin
                  (ly:grob-set-property! grob 'stencil
                                         (grob-interpret-markup grob
                                                                #{ \markup { \boxNadaDasar #solmisasi-key-sig } #}))
                  (ly:grob-set-property! grob 'Y-offset 0.7))
                 (ly:grob-set-property! grob 'stencil #f))
             (set! current-key-sig solmisasi-key-sig))))))))

#(define Solmisasi_equivalence_key_engraver
   (make-engraver
    (acknowledgers
     ((key-signature-interface engraver grob source-engraver)
      ;; make sure \omit is not in effect (stencil is not #f)
      (if (ly:grob-property-data grob 'stencil)
          ;;(ly:message (_ "  [Solmi;sasi_*_engraver] KEY SIGNATURE ENGRAVING\n"))
          (let*
           ((staff-context (ly:translator-context engraver))
            (context-id (ly:context-id staff-context))
            (context-name (ly:context-name staff-context))
            (event (event-cause grob))
            (current-moment (ly:context-current-moment staff-context))
            (is-male-vocal? (ly:context-property staff-context 'male-vocal))
            (transposed-up? (ly:context-property staff-context 'transposed-up))
            (last-pitch
             (if (and event
                      ;(vocal-instrument? context-id)
                      (not (equal? 'SolmisasiTimeAndKeySignature context-name)))
                 (ly:event-property event 'last-pitch-solmisasi #f)
                 #f))
            (last-pitch-pitch
             (if last-pitch
                 (ly:pitch-notename last-pitch)
                 #f))
            (last-pitch-alter
             (if last-pitch
                 (ly:pitch-alteration last-pitch)
                 #f))
            (last-pitch-octave
             (if last-pitch
                 (ly:pitch-octave last-pitch)
                 #f))
            (print-ekuivalensi?
             (and last-pitch
                  (equal? 'SolmisasiStaff context-name)
                  (ly:moment<? ZERO-MOMENT current-moment)))
            (base-octave (if is-male-vocal? -1 0))
            (stl #f))
           ;--------------------------------------
           (if (and key-is-changed
                    (ly:pitch? last-pitch))
               (begin
                (cond
                 ;; ces
                 ((and (= last-pitch-pitch 0)
                       (= last-pitch-alter -1/2))
                  (set! last-pitch (ly:make-pitch (- last-pitch-octave 1) 6 0)))
                 ;; TODO: bis, fes, eis
                 (transposed-up?
                  (set! last-pitch (ly:make-pitch
                                    (- last-pitch-octave 1)
                                    last-pitch-pitch
                                    last-pitch-alter)))
                 )
                (ly:grob-set-property! grob 'stencil
                                       (grob-interpret-markup grob
                                                              #{ \markup { \ekuivalensiNada #last-pitch #base-octave } #}))
                (ly:grob-set-property! grob 'whiteout #t)
                (ly:grob-set-property! grob 'Y-offset -1.15))
               (ly:grob-set-property! grob 'stencil #f))))))))

#(define Solmisasi_time_signature_engraver
   (let ((current-time-sig (cons 4 4)))
     (make-engraver
      ((initialize trans)
       (solmisasi:log "- Engraving time signatures in context ~a" (ly:translator-context trans)))
      (acknowledgers
       ( (time-signature-interface engraver grob source-engraver)
         ;; make sure \omit is not in effect (stencil is not #f)
         (if (ly:grob-property-data grob 'stencil)
             ;;(ly:message (_ "  [Solmisasi_*_engraver] TIME SIGNATURE ENGRAVING\n"))
             (let*
              ((event (event-cause grob))
               (solmisasi-time-sig
                (if event
                    (ly:event-property event 'solmisasi-time-sig)
                    current-time-sig))
               (stl
                (grob-interpret-markup grob
                                       #{
                                         \markup {
                                           \larger \bold \solmisasi
                                           \fraction
                                           #(string-append
                                             " "
                                             (number->string (car solmisasi-time-sig))
                                             " ")
                                           #(string-append
                                             " "
                                             (number->string (cdr solmisasi-time-sig))
                                             " ")
                                         }
                                       #}))
               (key-sig-stl
                (grob-interpret-markup grob
                                       #{\markup \center-align { \boxNadaDasar #'(1 . (ly:make-pitch 0 0 0)) }#}))
               (key-sig-X-extent
                (ly:stencil-extent key-sig-stl X))
               (extra-X
                (- (cdr key-sig-X-extent) (car key-sig-X-extent))))
              ;------------------------------------------
              (ly:grob-set-property! grob 'Y-offset 0.7)
              (if key-is-changed
                  (ly:grob-translate-axis! grob extra-X X))
              (ly:grob-set-property! grob 'stencil stl))))))))

#(define (solmisasi-beam-adjust grob)
   "Adjusts the width etc. of beams."
   ;; We calculate the amount to scale based on width of beam
   ;; TODO: improve this, maybe by using stems
   (let* ((x-ext (interval-length (ly:stencil-extent (ly:grob-property grob 'stencil) X)))
          (x-scale (+ 0.8 (/ 1.2 x-ext))))
     ;; (display x-ext)(newline)
     (ly:grob-set-property! grob 'stencil
                            (ly:stencil-translate
                             (ly:stencil-scale (ly:grob-property grob 'stencil) x-scale 1)
                             (cons 0.5 0))
                            )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_ENGRAVER_LOADED #t)
#(if (defined? 'LOGGING_LOADED)
     (solmisasi:log "* Solmisasi engraver module has been loaded."))