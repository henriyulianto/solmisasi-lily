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

#(define Solmisasi_note_heads_engraver
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
                                 (make-circle-stencil
                                  (if is-svg? 0.22 0.20)
                                  0.001 #t)
                                 (cons 0.5 -0.25)))
            (classname "notangka")
            (parenthesized? (ly:grob-property grob 'parenthesized))
            (attr-alist `((class . ,classname)))
            )

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
                  (grob-interpret-markup
                   grob
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
                                                \not-angka
                                                #(ly:pitch->solmisasi (ly:music-property ch 'pitch)) #base-octave
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

           (cond
            ((eq? (ly:grob-property grob 'style) 'cross)
             (ly:grob-set-property! grob 'stencil
                                    (grob-interpret-markup grob
                                                           #{
                                                             \markup {
                                                               \raise #0.2
                                                               \musicglyph #"noteheads.s2cross"
                                                             }
                                                           #})))
            ((eq? (ly:grob-property grob 'style) 'xcircle)
             (ly:grob-set-property! grob 'stencil
                                    (grob-interpret-markup grob
                                                           #{
                                                             \markup {
                                                               \raise #0.2
                                                               \musicglyph #"noteheads.s2xcircle"
                                                             }
                                                           #})))
            ((eq? (ly:grob-property grob 'style) 'slash)
             (ly:grob-set-property! grob 'stencil
                                    (grob-interpret-markup grob
                                                           #{
                                                             \markup {
                                                               \raise #0.2
                                                               \musicglyph #"noteheads.s2slash"
                                                             }
                                                           #})))
            ((eq? (ly:grob-property grob 'style) 'triangle)
             (ly:grob-set-property! grob 'stencil
                                    (grob-interpret-markup grob
                                                           #{
                                                             \markup {
                                                               \raise #0.2
                                                               \musicglyph #"noteheads.u1triangle"
                                                             }
                                                           #}))))
           (ly:grob-set-property! grob 'output-attributes attr-alist)
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
                                 (grob-interpret-markup
                                  grob
                                  #{ \markup \lower #0.5 "0" #}
                                  )))))))

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
                                         \markup \smaller {
                                           \override #'(font-features .("tnum" "cv47" "-kern"))
                                           \number
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

%% Additional engravers

%% Double barlines scheme engraver
DbBars =
#(lambda (context)
   (let ((time-signature '())
         (last-fraction #f))
     `((process-music
        . ,(lambda (trans)
             (let ((frac (ly:context-property context 'timeSignatureFraction)))
               (if (and (null? time-signature)
                        (not (equal? last-fraction frac))
                        (fraction? frac))
                   (begin
                    (ly:context-set-property! context 'whichBar "||")
                    (set! last-fraction frac))))))

       (stop-translation-timestep
        . ,(lambda (trans)
             (set! time-signature '()))))))

%% Unfold bar number across repeat structures
Unfold_bar_numbers_engraver =
#(lambda (ctx)
   (let* ((repeat-start #f)
          (alternative-starts '())
          (repeat-count #f))
     (make-engraver
      (listeners
       ((volta-repeat-start-event engraver event)
        ;; A \repeat volta <number> { ... } starts.
        ;; Get and store 'repeat-count and 'currentBarNumber.
        ;; At this point of time it's not yet know whether alternatives
        ;; will occurr.
        (set! repeat-count
              (ly:event-property event 'repeat-count))
        (set! repeat-start
              (ly:context-property ctx 'currentBarNumber)))

       ((volta-span-event engraver event)
        ;; If 'volta-span-event happens, alternatives are present.
        ;; Get and store (in a accumulates list) the bar numbers
        ;; when an alternative starts.
        (let ((volta-numbers (ly:event-property event 'volta-numbers)))
          (set! alternative-starts
                (cons
                 (ly:context-property ctx 'currentBarNumber)
                 alternative-starts))))

       ((volta-repeat-end-event engraver event)
        ;; 'volta-repeat-end-event is triggered at the end of the first
        ;; alternative or at the end of the repeat-setion, if no
        ;; alternatives are present.
        ;; For bar numbering this means we need to know the lengths of
        ;; the alternatives and the length of repeat-start to begin of
        ;; first alternative.
        (let* ((curr-bar-number
                (ly:context-property ctx 'currentBarNumber))
               ;; We call 'alternative-number in order to know whether
               ;; alternatives are present at all.
               (alternative-number
                (ly:event-property event 'alternative-number #f))
               ;; We call 'return-count to know how often a certain
               ;; alternative is repeated.
               (return-count (ly:event-property event 'return-count))
               ;; Drop bar-numbers lower than the start of current
               ;; repeat.
               (relevant-alternative-starts
                (filter
                 (lambda (x)
                   (> x repeat-start))
                 alternative-starts))
               ;; If we have alternatives, calculate their lengths.
               (relevant-alternative-lengths
                (let lp ((vals-list relevant-alternative-starts))
                  (if (or (null? vals-list) (odd? (length vals-list)))
                      '()
                      (cons
                       (- (car vals-list) (cadr vals-list))
                       (lp (drop vals-list 2))))))
               ;; Get the length of the repeat body, without first
               ;; alternative, if present.
               (body-length
                (-
                 (if alternative-number
                     (last relevant-alternative-starts)
                     curr-bar-number)
                 repeat-start)))

          (ly:context-set-property! ctx 'currentBarNumber
                                    (+
                                     curr-bar-number
                                     (* body-length return-count)
                                     (*
                                      (if (pair? relevant-alternative-lengths)
                                          (last relevant-alternative-lengths)
                                          0)
                                      (1- return-count))))))))))

#(define Bass_changes_equal_root_engraver
   (lambda (ctx)
     "For sequential @code{ChordNames} with same root, but different bass, the root
markup is dropped: D D/C D/B  -> D /C /B
The behaviour may be controlled by setting the @code{chordChanges}
context-property."
     (let ((chord-pitches '())
           (last-chord-pitches '())
           (bass-pitch #f))
       (make-engraver
        ((initialize this-engraver)
         (let ((chord-note-namer (ly:context-property ctx 'chordNoteNamer)))
           ;; Set 'chordNoteNamer, respect user setting if already done
           (ly:context-set-property! ctx 'chordNoteNamer
                                     (if (procedure? chord-note-namer)
                                         chord-note-namer
                                         note-name->markup))))
        (listeners
         ((note-event this-engraver event)
          (let* ((pitch (ly:event-property event 'pitch))
                 (pitch-name (ly:pitch-notename pitch))
                 (pitch-alt (ly:pitch-alteration pitch))
                 (bass (ly:event-property event 'bass #f))
                 (inversion (ly:event-property event 'inversion #f)))
            ;; Collect notes of the chord
            ;;  - to compare inversed chords we need to collect the bass note
            ;;    as usual member of the chord, whereas an added bass must be
            ;;    treated separate from the usual chord-notes
            ;;  - notes are stored as pairs containing their
            ;;    pitch-name (an integer), i.e. disregarding their octave and
            ;;    their alteration
            (cond (bass (set! bass-pitch pitch))
                  (inversion
                   (set! bass-pitch pitch)
                   (set! chord-pitches
                         (cons (cons pitch-name pitch-alt) chord-pitches)))
                  (else
                   (set! chord-pitches
                         (cons (cons pitch-name pitch-alt) chord-pitches)))))))
        (acknowledgers
         ((chord-name-interface this-engraver grob source-engraver)
          (let ((chord-changes (ly:context-property ctx 'chordChanges #f)))
            ;; If subsequent chords are equal apart from their bass,
            ;; reset the 'text-property.
            ;; Equality is done by comparing the sorted lists of this chord's
            ;; elements and the previous chord. Sorting is needed because
            ;; inverted chords may have a different order of pitches.
            ;; `chord-changes' needs to be true
            (if (and bass-pitch
                     chord-changes
                     (equal?
                      (sort chord-pitches car<)
                      (sort last-chord-pitches car<)))
                (ly:grob-set-property! grob 'text
                                       (make-line-markup
                                        (list
                                         (ly:context-property ctx 'slashChordSeparator)
                                         ((ly:context-property ctx 'chordNoteNamer)
                                          bass-pitch
                                          (ly:context-property ctx 'chordNameLowercaseMinor))))))
            (set! last-chord-pitches chord-pitches)
            (set! chord-pitches '())
            (set! bass-pitch #f))))
        ((finalize this-engraver)
         (set! last-chord-pitches '()))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_ENGRAVER_LOADED #t)
#(ly:message "* Solmisasi engravers module has been loaded.")
