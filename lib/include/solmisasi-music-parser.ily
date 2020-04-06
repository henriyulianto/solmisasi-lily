\version "2.20.0"
%% solmisasi-music-parser.ily
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

#(define (sol:message arg . rest)
   #f)

#(begin

  ;; Konstanta untuk solmisasiMusic
  (define _KEY_SIG_PROP 	'solmisasi-key-sig)
  (define _TIME_SIG_PROP 	'solmisasi-time-sig)
  (define _REST_PROP 			'solmisasi-rest)
  (define _DOT_NOTE_PROP 	'solmisasi-dot-note)
  (define _EXPERIMENTAL		#f)

  (define key-changes '())
  (define key-change-completed? #f)
  (define rest-pos (list))
  (define chord-pos (list))

  (define (note-event? m)
    (if (and (ly:music? m)
             (music-is-of-type? m 'note-event))
        #t
        #f))

  (define (rest-event? m)
    (if (ly:music? m)
        (if (or (music-is-of-type? m 'rest-event)
                (music-is-of-type? m 'multi-measure-rest))
            #t
            #f)
        #f))

  (define (event-chord? m)
    (and (ly:music? m)
         (music-is-of-type? m 'event-chord)
         (ly:music-property m 'elements #f)))

  (define (get-last-pitch-for-volta m)
    (let*
     ((last-elt (last (ly:music-property m 'elements))))
     (if (not (null? (ly:music-property last-elt 'elements)))
         (get-last-pitch-for-volta last-elt)
         (if (note-event? last-elt)
             (ly:music-property last-elt 'pitch)
             #f))
     ))

  (define (slur-start-note? m)
    (let* ((articulations (ly:music-property m 'articulations)))
      (not (null?
            (filter
             (lambda (a)
               (and (music-is-of-type? a 'slur-event)
                    (= -1 (ly:music-property a 'span-direction))))
             articulations)))))

  (define (slur-stop-note? m)
    (let* ((articulations (ly:music-property m 'articulations)))
      (not (null?
            (filter
             (lambda (a)
               (and (music-is-of-type? a 'slur-event)
                    (= 1 (ly:music-property a 'span-direction))))
             articulations)))))

  (define (tied-note? m)
    (let ((articulations (ly:music-property m 'articulations)))
      (not (null?
            (filter (lambda (a) (music-is-of-type? a 'tie-event)) articulations)))))

  (define (append-articulations m a) (ly:music-set-property! m 'articulations
                                                             (append
                                                              (ly:music-property m 'articulations)
                                                              (list a))))

  (define (delete-articulations m a) (ly:music-set-property! m 'articulations
                                                             (delete
                                                              a (ly:music-property m 'articulations))))

  (define (close-div-start) (make-music 'TextSpanEvent
                                        'tweaks (list (cons
                                                       (cons 'TextSpanner 'stencil)
                                                       close-divisi-stencil))
                                        'direction -1
                                        'span-direction -1))

  (define (open-div-start)	(make-music 'TextSpanEvent
                                       'tweaks (list (cons
                                                      (cons 'TextSpanner 'stencil)
                                                      open-divisi-stencil))
                                       'direction -1
                                       'span-direction -1))

  (define (div-span-stop)		(make-music 'TextSpanEvent
                                       ;'direction -1
                                       'span-direction 1))

  (define (is-music-chord? m) (and (note-event? m)
                                   (ly:music-property m 'cdr-chords #f)))

  (define (is-music-span-top? m) (ly:music-property m 'div-span-stop #f))

  (define (prepare-repeat-volta-last-pitch mus)
    (if (not (ly:music? mus))
        (ly:error "Parameter harus berupa ly:music?.")
        (let ((volta-has-alternatives? #f))
          (music-map
           (lambda (m)
             (if (equal? 'VoltaRepeatedMusic (ly:music-property m 'name))
                 (begin
                  (set! volta-has-alternatives?
                        (not (null? (ly:music-property m 'elements))))
                  (if volta-has-alternatives?
                      (music-map
                       (lambda (v)
                         (ly:music-set-property! v 'last-pitch-for-volta
                                                 (get-last-pitch-for-volta m))
                         v)
                       m))
                  ))
             m)
           mus)))
    mus)

  (define-public (my-retrograde-music music)
    "Returns @var{music} in retrograde (reversed) order."
    ;; Included here to allow this module to provide a complete set of
    ;; common formal operations on motives, i.e transposition,
    ;; inversion and retrograding.

    (define (reverse-span! m)
      ;; invert direction of two-sided spanners
      (let ((spd (ly:music-property m 'span-direction)))
        (if (ly:dir? spd)
            (begin
             (set! (ly:music-property m 'span-direction) (- spd))
             (case (ly:music-property m 'name)
               ((CrescendoEvent)
                (make-music 'DecrescendoEvent m))
               ((DecrescendoEvent)
                (make-music 'CrescendoEvent m))
               (else m)))
            m)))

    ;; carryover is a possible list of tie events, the loop returns any
    ;; such trailing list from the given expression
    (define (loop m carryover)
      (define (filter-ties! m carryover field)
        (let ((vals (ly:music-property m field)))
          (if (pair? vals)
              (call-with-values
               (lambda ()
                 (partition! (music-type-predicate
                              '(tie-event glissando-event)) vals))
               (lambda (ties no-ties)
                 (set! (ly:music-property m field)
                       (append! (map! reverse-span! no-ties) carryover))
                 ties))
              (begin
               (if (pair? carryover)
                   (set! (ly:music-property m field) carryover))
               '()))))

      ;; The reversal will let some prefatory material stay in front of
      ;; the following element.  Most prominently single
      ;; overrides/reverts/sets/unsets and applyContext.  This does not
      ;; change the position of a clef (which will generally be useless
      ;; after retrograding) but it does not jumble the clef change
      ;; command internals.  Also, stuff like \once\override stays at
      ;; the affected element.

      (define (prefatory? m)
        (or ((music-type-predicate
              '(apply-context apply-output-event layout-instruction-event)) m)
            (and
             (music-is-of-type? m 'music-wrapper-music)
             (prefatory? (ly:music-property m 'element)))))

      (define (musiclistreverse lst)
        (let loop ((lst lst) (res '()) (zeros '()))
          (cond ((null? lst) (reverse! zeros res))
                ; ((prefatory? (car lst))
                ;                  (loop (cdr lst) res (cons (car lst) zeros)))
                (else
                 (loop (cdr lst) (reverse! zeros (cons (car lst) res)) '())))))

      (cond ((music-is-of-type? m 'event-chord)
             (let* ((chord-ties
                     (append!
                      (filter-ties! m carryover 'elements)
                      ;; articulations on an event-chord do not occur
                      ;; "naturally" but are supported when user-generated
                      ;; elsewhere, so we treat them properly
                      (filter-ties! m '() 'articulations)))
                    ;; in-chord ties are converted to per-chord ties.
                    ;; This is less than optimal but pretty much the
                    ;; best we can hope to achieve with this approach.
                    (element-ties
                     (append-map!
                      (lambda (m) (filter-ties! m '() 'articulations))
                      (ly:music-property m 'elements))))
               (append! chord-ties element-ties)))

            ((music-is-of-type? m 'rhythmic-event)
             (filter-ties! m carryover 'articulations))

            ;; The following is hardly correct but tieing inside of
            ;; <<...>> is really beyond our pay grade.
            ((music-is-of-type? m 'simultaneous-music)
             (append-map! (lambda (m) (loop m (ly:music-deep-copy carryover)))
                          (ly:music-property m 'elements)))
            (else
             (let ((elt (ly:music-property m 'element))
                   (elts (ly:music-property m 'elements)))
               (let ((res
                      (fold loop
                            (if (ly:music? elt) (loop elt carryover) carryover)
                            elts)))
                 (if (ly:music? elt)
                     (set! (ly:music-property m 'element)
                           (reverse-span! elt)))
                 (if (pair? elts)
                     (set! (ly:music-property m 'elements)
                           (map! reverse-span! (musiclistreverse elts))))
                 (append! res (filter-ties! m '() 'articulations)))))))
    (let ((dangling (loop music '())))
      (for-each
       (lambda (t) (ly:music-warning t (_ "Dangling tie in \\retrograde")))
       dangling))
    music)

  (define (prepare-chords mus)
    (map-some-music
     (lambda (m)
       (and (and (event-chord? m)
                 (not (null?
                       (filter (lambda (x)
                                 (music-is-of-type? x 'note-event))
                               (ly:music-property m 'elements)))))
            (make-sequential-music
             (append
              (list #{
                \once\override Beam.stencil =
                #(lambda (grob)
                   (ly:stencil-combine-at-edge
                    (ly:stencil-translate-axis (ly:beam::print grob) 0.0 Y)
                    Y DOWN
                    (ly:beam::print grob)
                    3.2))
                    #})
              (list (make-music
                     'NoteEvent (car (ly:music-property m 'elements))
                     'cdr-chords (cdr (ly:music-property m 'elements)))))))
       ) ; end lambda
     mus))

  (define (prepare-chord-open-close mus)
    (let* ((reverselst #f)
           (rmus #f)
           (last-music (empty-music)))
      ;--> Forward
      (music-map
       (lambda (m)
         (cond
          ((or (and (note-event? m) (not (is-music-chord? m)))
               (rest-event? m))
           (if (is-music-chord? last-music)
               (ly:music-set-property! m 'div-span-stop #t)))
          ((and (note-event? m) (is-music-chord? m))
           (if (or (and (not (is-music-chord? last-music))
                        (not (equal? last-music (empty-music))))
                   (rest-event? m))
               (ly:music-set-property! m 'div-span-stop #t)))
          ) ; end cond
         (if (or (note-event? m) (rest-event? m))
             (set! last-music m))
         m) ; end lambda
       mus)
      ; <-- Backward
      (set! last-music (empty-music))
      (set! rmus (my-retrograde-music mus))
      (set! mus #f)
      (music-map
       (lambda (m)
         (cond
          ((and (or (note-event? m) (rest-event? m))
                (is-music-span-top? last-music))
           (if (is-music-chord? m)
               (ly:music-set-property! m 'close-div-start #t)
               (ly:music-set-property! m 'open-div-start #t)))
          ) ; end cond
         (if (or (note-event? m) (rest-event? m))
             (set! last-music m))
         m) ; end lambda
       rmus)
      (set! mus (my-retrograde-music rmus))
      mus))

  (define solmisasiMusic
    (define-music-function (mus) (ly:music?)
      "Memodifikasi mus agar siap di-engrave sebagai solmisasi/not angka."

      (define slur-started? #f)
      (define in-slur? #f)
      (define slur-stopped? #f)
      (define note-in-tie? #f)
      (define is-rest? #f)
      (define note-or-rest-iteration 0)
      (define chord-iteration 0)
      (define open-div-list (list))
      (define close-div-list (list))
      (define found-chord-after-single #f)
      (define found-single-after-chord #f)
      (define context-id #f)

      (define (beginMelismaList)
        (cond
         ((or (and (not slur-started?)
                   (not slur-stopped?)
                   (not in-slur?))
              slur-started?)
          (list #{ s1*0\melisma #}))
         (else
          (list))))

      (define (endMelismaList)
        (if note-in-tie?
            (list #{ s1*0\melisma #})
            (cond
             ((or (and (not slur-started?)
                       (not slur-stopped?)
                       (not in-slur?))
                  slur-stopped?)
              (list #{ s1*0\melismaEnd #}))
             (else
              (list)))))

      (define (melismaStart)
        (if is-rest?
            (list #{\melismaEnd#})
            (list #{\melismaEnd\melisma#})
            ))
      (define (melismaStop)
        (if is-rest?
            (list)
            (list #{\melismaEnd#})
            ))

      (sol:message (_ "\n  [solmisasiMusic] --------- BEGIN ---------\n"))
      (let* ( (muscopy (ly:music-deep-copy mus))
              (iter-num 0)
              (last-pitch (ly:make-pitch 0 0 0))
              (last-pitch-solmisasi last-pitch)
              (major-tonic-pitch (ly:make-pitch 0 0 0))
              (beat-structure-dur 1/4)
              (beat-structure-mom (ly:make-moment beat-structure-dur))
              (evaluated-moment ZERO-MOMENT)
              (evaluated-dur 0)
              (last-key-str "")
              (mus-key-changes '())
              (key-changes-str #f)
              (dur 0)
              (last-moment ZERO-MOMENT)
              (last-dur 0)
              (tempo-awal #f)
              (orig-m (empty-music))
              (current-time-sig 4/4)
              (last-note (empty-music))
              (is-last-note-end-of-slur? #f)
              (duradot-sum 0)
              )

        ;; Get last pitch untuk repeat-volta
        (set! muscopy (prepare-repeat-volta-last-pitch muscopy))
        ;; Initialize beam grouping with default 4/4
        (set! muscopy (make-sequential-music
                       (append
                        (list (beam_grouping_by_time_sig (cons 4 4)))
                        (list muscopy))))
        ;; Initialize rest-pos
        (set! rest-pos (list))
        ;; Initialize chord-pos
        (set! chord-pos (list))

        ;-----------------------------------------------------------
        ; Event: ChordEvent
        (set! chord-iteration 0)
        (set! muscopy (prepare-chords muscopy))
        (set! muscopy (prepare-chord-open-close muscopy))

        ;-----------------------------------------------------------
        ; Event: RestEvent
        ; Save a list of rest-event position in rest-pos
        (set! chord-iteration 0)
        (music-map
         (lambda (m)
           (cond
            ((rest-event? m)
             (set! note-or-rest-iteration (1+ note-or-rest-iteration))
             (if (not (memq note-or-rest-iteration rest-pos))
                 (set! rest-pos
                       (append rest-pos (list note-or-rest-iteration)))))
            ((or (note-event? m) (event-chord? m))
             (if (and (not (slur-stop-note? m))
                      (not (tied-note? m)))
                 (set! note-or-rest-iteration (1+ note-or-rest-iteration)))
             )
            ) ; end cond
           m) ; end lambda m
         muscopy)

        (music-map
         (lambda (m)
           (if (and (note-event? m) (not (slur-stop-note? m)))
               (set! note-or-rest-iteration (1+ note-or-rest-iteration)))

           (cond

            ;-----------------------------------------------------------
            ;; Event: PERUBAHAN TANDA SUKAT/BIRAMA/TIME SIGNATURE
            ;; Tambahkan/set properti musik baru: 'solmisasi-time-sig
            ((music-is-of-type? m 'time-signature-music)

             (let*
              ( (numerator-num (ly:music-property m 'numerator))
                (denominator-num (ly:music-property m 'denominator))
                (time-sig-fraction (cons numerator-num denominator-num)) )

              (set! evaluated-moment ZERO-MOMENT)
              (set! evaluated-dur 0)
              (set! beat-structure-dur
                    (if (and (= denominator-num 8)
                             (= (modulo numerator-num 3) 0))
                        1/8
                        ; else
                        1/4
                        ))
              (set! beat-structure-mom (ly:make-moment beat-structure-dur))
              (set! current-time-sig (/ numerator-num denominator-num))
              (sol:message (_ "  [solmisasiMusic] - Menemukan perubahan TANDA SUKAT: ~a/~a bs=~a\n")
                           numerator-num denominator-num beat-structure-dur)
              (if (defined? 'update-tanda-sukat-header)
                  (update-tanda-sukat-header numerator-num denominator-num))
              (ly:music-set-property! m _TIME_SIG_PROP (cons numerator-num denominator-num))
              (set! m (make-sequential-music
                       (append
                        (list (beam_grouping_by_time_sig time-sig-fraction))
                        (list m))))
              ))
            ;-----------------------------------------------------------

            ;-----------------------------------------------------------
            ;; Event: PERUBAHAN NADA DASAR/KEY
            ;; Tambahkan/set properti musik baru: 'solmisasi-key-sig
            ((music-is-of-type? m 'key-change-event)

             (let*
              ( (pitch-alist (ly:music-property m 'pitch-alist))
                (key-alts (filter (lambda (a) (not (= 0 (cdr a)))) pitch-alist))
                (keysig-alt-count (get-keysig-alt-count key-alts))
                (major-tonic-number (get-major-tonic keysig-alt-count))
                (tonic (ly:music-property m 'tonic))
                (tonic-num (ly:pitch-notename tonic))
                ;; mode 1-7, 1=major 6=minor
                (mode (+ 1 (modulo (- tonic-num major-tonic-number) 7)))
                (transposed-last-pitch #f)
                (last-major-tonic-pitch major-tonic-pitch)
                (key-sig-string ""))
              (set! major-tonic-pitch
                    (ly:make-pitch
                     0
                     major-tonic-number
                     (or (assoc-ref key-alts major-tonic-number) 0)
                     ))
              (set! key-sig-string
                    (format "~a = ~a"
                            (number->string mode)
                            (get-key-sig-string tonic)))
              (set! transposed-last-pitch
                    (if last-pitch
                        (ly:pitch-diff last-pitch major-tonic-pitch)
                        #f
                        ))
              (if (not (string=? key-sig-string last-key-str))
                  (begin
                   (set! last-key-str key-sig-string)
                   (set! mus-key-changes
                         (append mus-key-changes
                                 (list (cons evaluated-moment last-key-str))))
                   ))

              ;; simpan key aktual untuk digunakan dalam penulisan nada dasar sistem solmisasi
              (sol:message (_ "  [solmisasiMusic] - Menemukan perubahan NADA DASAR: mode=~a, tonic=~a\n") mode tonic)
              (ly:music-set-property! m 'solmisasi-key-sig (cons mode tonic))
              (ly:music-set-property! m 'last-pitch last-pitch)
              (ly:music-set-property! m 'last-pitch-solmisasi transposed-last-pitch)

              ;; konversi key sig ke c major, perlukah?
              ;(ly:music-set-property! m 'pitch-alist '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)))
              ;(ly:music-set-property! m 'tonic (ly:make-pitch 0 0 0))

              ))
            ;-----------------------------------------------------------

            ;-----------------------------------------------------------
            ;; Event: Skip event
            ((music-is-of-type? m 'skip-event)

             (let
              ((dur (ly:moment-main (ly:music-duration-length m))))
              (set! last-pitch #f)
              (set! last-dur dur)
              (set! last-moment (ly:make-moment last-dur))
              (set! evaluated-moment (ly:moment-add evaluated-moment last-moment))
              (set! evaluated-dur (+ evaluated-dur last-dur))))

            ;-----------------------------------------------------------
            ;; Event: Picthed NOTE atau REST
            ((or (note-event? m)
                 (rest-event? m))

             ;; Div spanner stop
             (if (ly:music-property m 'div-span-stop #f)
                 (append-articulations m (div-span-stop)))
             (if (ly:music-property m 'open-div-start #f)
                 (append-articulations m (open-div-start)))
             (if (ly:music-property m 'close-div-start #f)
                 (append-articulations m (close-div-start)))

             (if (note-event? m) (set! last-note m))
             (set! orig-m (ly:music-deep-copy m))

             (set! iter-num (+ 1 iter-num))
             (sol:message (_ "  [solmisasiMusic] ITERATION: ~a\n") iter-num)
             (if (equal? (list (make-music 'TieEvent)) (ly:music-property m 'articulations))
                 (sol:message (_ "  [solmisasiMusic] - Menemukan TIE\n")))

             (let*
              ( (dur (ly:moment-main (ly:music-duration-length m)))
                (dur4 (ly:make-duration 2 0 1))
                (dur8 (ly:make-duration 3 0 1))
                (dur16 (ly:make-duration 4 0 1))
                (dur32 (ly:make-duration 5 0 1))
                (dotpitch (if (tied-note? m)
                              (ly:make-pitch -6 0 0)
                              (ly:music-property m 'pitch)))
                (q4 (make-music   		'NoteEvent
                                    'duration dur4
                                    'origin (ly:music-property m 'origin)
                                    ;'pitch pitchProp
                                    'pitch dotpitch
                                    _DOT_NOTE_PROP #t))
                (q8 (make-music   		'NoteEvent
                                    'duration dur8
                                    'origin (ly:music-property m 'origin)
                                    ;'pitch pitchProp
                                    'pitch dotpitch
                                    _DOT_NOTE_PROP #t))
                (q16 (make-music  		'NoteEvent
                                    'duration dur16
                                    'origin (ly:music-property m 'origin)
                                    ;'pitch pitchProp
                                    'pitch dotpitch
                                    _DOT_NOTE_PROP #t))
                (q32 (make-music  		'NoteEvent
                                    'duration dur32
                                    'origin (ly:music-property m 'origin)
                                    ;'pitch pitchProp
                                    'pitch dotpitch
                                    _DOT_NOTE_PROP #t))
                (q4-rest
                 (if _EXPERIMENTAL
                     (make-music 'RestEvent
                                 'duration (ly:make-duration 2))
                     (make-music 'NoteEvent
                                 'duration dur4
                                 'origin (ly:music-property m 'origin)
                                 ;'tags (list (quote do-not-print))
                                 _REST_PROP #t)))
                (q8-rest
                 (if _EXPERIMENTAL
                     (make-music 'RestEvent
                                 'duration (ly:make-duration 3))
                     (make-music 'NoteEvent
                                 'duration dur8
                                 'origin (ly:music-property m 'origin)
                                 ;'tags (list (quote do-not-print))
                                 _REST_PROP #t)))
                (q16-rest
                 (if _EXPERIMENTAL
                     (make-music 'RestEvent
                                 'duration (ly:make-duration 4))
                     (make-music 'NoteEvent
                                 'duration dur16
                                 'origin (ly:music-property m 'origin)
                                 ;'tags (list (quote do-not-print))
                                 _REST_PROP #t)))
                (q32-rest
                 (if _EXPERIMENTAL
                     (make-music 'RestEvent
                                 'duration (ly:make-duration 5))
                     (make-music 'NoteEvent
                                 'duration dur32
                                 'origin (ly:music-property m 'origin)
                                 ;'tags (list (quote do-not-print))
                                 _REST_PROP #t)))
                (skipRest (make-music 'SkipEvent 'duration (ly:make-duration 2 0 1)))
                (cdr-chords (ly:music-property m 'cdr-chords #f))
                )

              ;; utk rest, set properti 'solmisasi-rest
              (if (rest-event? m)
                  (begin
                   (set! is-rest? #t)
                   (set! m (make-music 'RestEvent m))
                   (if _EXPERIMENTAL
                       (ly:music-set-property! m 'name 'RestEvent)
                       (ly:music-set-property! m 'name 'NoteEvent))
                   (ly:music-set-property! m 'pitch
                                           (if (not (null? (ly:music-property m 'pitch)))
                                               (ly:music-property m 'pitch)
                                               last-pitch))
                   (ly:music-set-property! m 'pitch-solmisasi last-pitch-solmisasi)
                   (ly:music-set-property! m _REST_PROP #t)
                   (set! skipRest (skip-of-length m))
                   (set! q4 q4-rest)
                   (set! q8 q8-rest)
                   (set! q16 q16-rest)
                   (set! q32 q32-rest)
                   )
                  ;; else: pitched note event
                  (begin
                   (if (ly:pitch? (ly:music-property m 'pitch))
                       (begin
                        (set! last-pitch (ly:music-property m 'pitch))
                        (set! last-pitch-solmisasi (ly:music-property m 'pitch-solmisasi)))
                       (begin
                        (ly:music-set-property! m 'pitch last-pitch)
                        (ly:music-set-property! m 'pitch-solmisasi last-pitch-solmisasi)))
                   (if (not (null? (ly:music-property m 'last-pitch-for-volta)))
                       (set! last-pitch (ly:music-property m 'last-pitch-for-volta)))
                   (sol:message (_ "  [solmisasiMusic] - Menemukan NADA: nada=~a, durasi=~a\n")
                                (ly:music-property m 'pitch) (ly:moment-main (ly:music-duration-length m)))
                   ;; transpose ke c major agar lebih mudah penanganannya
                   (ly:music-set-property! m 'pitch-solmisasi
                                           (ly:pitch-transpose (ly:music-property m 'pitch)
                                                               (ly:pitch-negate (ly:pitch-diff major-tonic-pitch
                                                                                               (ly:make-pitch 0 0 0)))))
                   (set! last-pitch-solmisasi (ly:music-property m 'pitch-solmisasi))
                   ; FOR CHORDS
                   (if (and cdr-chords
                            (not (null? cdr-chords)))
                       (begin
                        (map!
                         (lambda (e)
                           (ly:music-set-property! e 'pitch-solmisasi
                                                   (ly:pitch-transpose (ly:music-property e 'pitch)
                                                                       (ly:pitch-negate (ly:pitch-diff major-tonic-pitch
                                                                                                       (ly:make-pitch 0 0 0))))))
                         cdr-chords)
                        (ly:music-set-property! m 'cdr-chords cdr-chords)
                        (ly:music-set-property! q4 'cdr-chords cdr-chords)
                        (ly:music-set-property! q8 'cdr-chords cdr-chords)
                        (ly:music-set-property! q16 'cdr-chords cdr-chords)
                        (ly:music-set-property! q32 'cdr-chords cdr-chords)
                        ))
                   (sol:message (_ "                     in C: nada=~a, durasi=~a\n")
                                (ly:music-property m 'pitch) (ly:moment-main (ly:music-duration-length m)))
                   ))

              ;; untuk NOTE maupun REST

              (sol:message (_ "  [solmisasiMusic] EVAL: ~a ~a\n") evaluated-moment (ly:moment-main (ly:music-duration-length m)))

              ;; split durasi nada
              (let* ( (duradot4 (floor (ly:moment-main
                                        (ly:moment-div
                                         (ly:moment-sub (ly:make-moment dur) (ly:make-moment 1/4))
                                         (ly:make-moment 1/4)
                                         ))))
                      (duradot8 (floor (ly:moment-main
                                        (ly:moment-div
                                         (ly:moment-mod (ly:make-moment dur) (ly:make-moment 1/4))
                                         (ly:make-moment 1/8)
                                         ))))
                      (duradot16 (floor (ly:moment-main
                                         (ly:moment-div
                                          (ly:moment-mod (ly:make-moment dur) (ly:make-moment 1/8))
                                          (ly:make-moment 1/16)
                                          ))))
                      (duradot32 (floor (ly:moment-main
                                         (ly:moment-div
                                          (ly:moment-mod (ly:make-moment dur) (ly:make-moment 1/16))
                                          (ly:make-moment 1/32)
                                          ))))
                      (duradot4-extra 0)
                      (duradot8-extra 0)
                      (duradot16-extra 0)
                      (duradot32-extra 0)
                      (dots-list (empty-music))
                      ;; TODO: nilai nada/durasi lain?
                      (current-moment   (ly:make-moment dur))
                      (delta-moment     (ly:moment-mod evaluated-moment beat-structure-mom))
                      (now-sum-moment   (ly:moment-add delta-moment current-moment))
                      (remainder-moment (ly:moment-sub beat-structure-mom delta-moment))
                      (must-reverse
                       (and  ;(ly:moment<? last-moment beat-structure-mom)
                             (ly:moment<? ZERO-MOMENT delta-moment)
                             (or (ly:moment<? beat-structure-mom now-sum-moment)
                                 (equal? beat-structure-mom now-sum-moment))))
                      (still-remainder-moment ZERO-MOMENT)
                      ;; apakah ada extra durasi setelah reverse?
                      (has-extra-job #f)
                      (dots-list-car #f)
                      )

                (sol:message (_ "  [solmisasiMusic] eval=~a|reverse=~a|current=~a|delta=~a|now-sum=~a|remainder=~a\n")
                             evaluated-moment must-reverse current-moment delta-moment now-sum-moment remainder-moment)

                (if (not (ly:music-property m 'duration #f))
                    (begin

                     (display-scheme-music m))
                    )
                (if (equal? (cdr (ly:duration-factor (ly:music-property m 'duration))) 1)
                    (begin
                     (if (equal? beat-structure-dur 1/4)
                         (begin
                          (cond
                           ( ( >= dur beat-structure-dur) ;------------------------------------------------
                                                          (ly:music-set-property! m 'duration dur4)
                                                          (if (equal? must-reverse #t)
                                                              (cond
                                                               ((equal? (ly:moment-main-denominator remainder-moment) 8)
                                                                (ly:music-set-property! m 'duration dur8)
                                                                (set! current-moment (ly:moment-sub current-moment (ly:make-moment 1/8)))
                                                                (set! duradot32 0)
                                                                (set! duradot16 0)
                                                                (set! duradot8 0)
                                                                (set! duradot4 (floor
                                                                                (ly:moment-main
                                                                                 (ly:moment-div current-moment beat-structure-mom))))
                                                                (set! still-remainder-moment (ly:moment-sub
                                                                                              current-moment (ly:make-moment (* duradot4 1/4))))
                                                                (set! has-extra-job (ly:moment<? ZERO-MOMENT still-remainder-moment))
                                                                (sol:message (_ "  [solmisasiMusic] current-moment=~a | duradot4=~a | still=~a | has-extra-job=~a\n")
                                                                             current-moment duradot4 still-remainder-moment has-extra-job)

                                                                (if (equal? has-extra-job #t)
                                                                    (begin
                                                                     (set! duradot8-extra (floor
                                                                                           (ly:moment-main
                                                                                            (ly:moment-div still-remainder-moment (ly:make-moment 1/8)))))
                                                                     (set! duradot16-extra (* 16
                                                                                              (ly:moment-main
                                                                                               (ly:moment-sub still-remainder-moment (ly:make-moment (* duradot8-extra 1/8))))))
                                                                     (sol:message (_ "  [solmisasiMusic] duradot8-extra=~a | duradot16-extra=~a\n") duradot8-extra duradot16-extra)
                                                                     )
                                                                    )
                                                                )
                                                               ((equal? (ly:moment-main-denominator remainder-moment) 16)
                                                                (ly:music-set-property! m 'duration dur16)
                                                                (set! current-moment (ly:moment-sub current-moment (ly:make-moment 1/16)))
                                                                (set! duradot32 0)
                                                                (set! duradot16 0)
                                                                (set! duradot4 (floor
                                                                                (ly:moment-main
                                                                                 (ly:moment-div current-moment beat-structure-mom))))
                                                                (set! duradot8 (floor
                                                                                (ly:moment-main
                                                                                 (ly:moment-div remainder-moment (ly:make-moment 1/8)))))
                                                                (set! still-remainder-moment (ly:moment-sub
                                                                                              (ly:moment-sub current-moment (ly:make-moment (* duradot4 1/4)))
                                                                                              (ly:make-moment (* duradot8 1/8))))
                                                                (set! has-extra-job (ly:moment<? ZERO-MOMENT still-remainder-moment))
                                                                (sol:message (_ "  [solmisasiMusic] current-moment=~a | duradot4=~a | still=~a | has-extra-job=~a\n")
                                                                             current-moment duradot4 still-remainder-moment has-extra-job)

                                                                (if (equal? has-extra-job #t)
                                                                    (begin
                                                                     (set! duradot8-extra (floor
                                                                                           (ly:moment-main
                                                                                            (ly:moment-div still-remainder-moment (ly:make-moment 1/8)))))
                                                                     (set! duradot16-extra (* 16
                                                                                              (ly:moment-main
                                                                                               (ly:moment-sub still-remainder-moment (ly:make-moment (* duradot8-extra 1/8))))))
                                                                     (sol:message (_ "  [solmisasiMusic] duradot8-extra=~a | duradot16-extra=~a\n") duradot8-extra duradot16-extra)
                                                                     )
                                                                    )
                                                                )
                                                               )
                                                              )
                                                          )

                           ( (>= dur (* beat-structure-dur 1/2)) ;------------------------------------------------
                                                                 (set! duradot4 0)
                                                                 (set! duradot8 0)
                                                                 (sol:message (_ "  [solmisasiMusic] GREATER THAN 1/8: ~a ~a mod=~a\n") evaluated-moment dur delta-moment)
                                                                 (ly:music-set-property! m 'duration dur8)
                                                                 (if (equal? must-reverse #t)
                                                                     (if (equal? (ly:moment-main-denominator remainder-moment) 16)
                                                                         (begin
                                                                          (ly:music-set-property! m 'duration dur16)
                                                                          (set! current-moment (ly:moment-sub current-moment (ly:make-moment 1/16)))
                                                                          (set! duradot16 0)
                                                                          (set! duradot8 1)
                                                                          (set! duradot4 0)
                                                                          )
                                                                         )
                                                                     )
                                                                 )

                           ( (>= dur (* beat-structure-dur 1/4)) ;------------------------------------------------
                                                                 (set! duradot4 0)
                                                                 (set! duradot8 0)
                                                                 (set! duradot16 0)
                                                                 (ly:music-set-property! m 'duration dur16)
                                                                 )

                           ( (>= dur (* beat-structure-dur 1/8)) ;------------------------------------------------
                                                                 (set! duradot4 0)
                                                                 (set! duradot8 0)
                                                                 (set! duradot16 0)
                                                                 (set! duradot32 0)
                                                                 (ly:music-set-property! m 'duration dur32)
                                                                 )
                           ) ;; end cond dur
                          )
                         ;; else: untuk yang berbasis 3/8
                         (begin
                          ;; disable duradot4
                          (set! duradot4 0)
                          (cond
                           ( ( >= dur beat-structure-dur) ;------------------------------------------------
                                                          (ly:music-set-property! m 'duration dur8)
                                                          (if (equal? dur 3/8) ; 3/8
                                                              (begin
                                                               (set! duradot8 0)
                                                               (set! duradot16 0)
                                                               (set! duradot8-extra 0)
                                                               (set! duradot16-extra 0)
                                                               (set! must-reverse #f)
                                                               (set! m (make-sequential-music
                                                                        (append
                                                                         (list m)
                                                                         (make-list 2 q8))))
                                                               )
                                                              (begin
                                                               (set! current-moment (ly:moment-sub current-moment (ly:make-moment 1/8)))
                                                               (set! duradot8 (floor
                                                                               (ly:moment-main
                                                                                (ly:moment-div current-moment (ly:make-moment beat-structure-dur)))))
                                                               ))

                                                          (sol:message (_ "  [solmisasiMusic] GREATER THAN 1/8: ~a ~a mod=~a\n") evaluated-moment dur delta-moment)

                                                          (if (equal? must-reverse #t)
                                                              (if (equal? (ly:moment-main-denominator remainder-moment) 16)
                                                                  (begin
                                                                   (ly:music-set-property! m 'duration dur16)
                                                                   (set! current-moment (ly:moment-sub current-moment (ly:make-moment 1/16)))
                                                                   (set! duradot16 0)
                                                                   (set! duradot8 1)
                                                                   (set! duradot4 0)
                                                                   )
                                                                  )
                                                              )
                                                          )
                           ;; 1/16
                           ( ( >= dur (* beat-structure-dur 1/2)) ;------------------------------------------------
                                                                  (set! duradot8 0)
                                                                  (set! duradot16 0)
                                                                  (ly:music-set-property! m 'duration dur16)
                                                                  )
                           ) ;; end cond dur
                          ) ;; end begin
                         )

                     (set! note-in-tie? (or note-in-tie? (tied-note? m)))
                     (set! slur-stopped? (slur-stop-note? m))
                     ;(set! is-last-note-end-of-slur? (slur-stop-note? m))
                     (set! in-slur? (and slur-started? (not slur-stopped?)))
                     (set! slur-started? (slur-start-note? m))

                     (set! duradot-sum ;(1-
                           (+ duradot4
                              duradot8
                              duradot16
                              duradot32
                              duradot4-extra
                              duradot8-extra
                              duradot16-extra
                              duradot32-extra));)

                     (if slur-stopped?
                         ; end of slur, perhaps have dots
                         (if (positive? duradot-sum)
                             (begin
                              (set! dots-list (reverse (append
                                                        (make-list duradot4  q4)
                                                        (make-list duradot8  q8)
                                                        (make-list duradot16 q16)
                                                        (make-list duradot32 q32))))
                              (set! dots-list-car (car dots-list))
                              (if (ly:music-property m 'open-div-start #f)
                                  (append-articulations dots-list-car (open-div-start)))
                              (if (ly:music-property m 'close-div-start #f)
                                  (append-articulations dots-list-car (close-div-start)))
                              (set! dots-list (reverse
                                               (append (list dots-list-car)
                                                       (cdr dots-list))))

                              (set! dots-list (append
                                               (list (make-music
                                                      'ContextSpeccedMusic
                                                      'context-type
                                                      'Bottom
                                                      'element
                                                      (make-music
                                                       'PropertySet
                                                       'value
                                                       #t
                                                       'symbol
                                                       'melismaBusy)))
                                               dots-list
                                               (list (make-music
                                                      'ContextSpeccedMusic
                                                      'context-type
                                                      'Bottom
                                                      'element
                                                      (make-music
                                                       'PropertyUnset
                                                       'symbol
                                                       'melismaBusy)))
                                               ))
                              )
                             ; zero duradot-sum
                             (set! dots-list #f)

                             )

                         ; not end of slur
                         (if (positive? duradot-sum)
                             (begin
                              (set! dots-list (reverse (append
                                                        (make-list duradot4  q4)
                                                        (make-list duradot8  q8)
                                                        (make-list duradot16 q16)
                                                        (make-list duradot32 q32))))
                              (set! dots-list-car (car dots-list))
                              (if (ly:music-property m 'open-div-start #f)
                                  (append-articulations dots-list-car (open-div-start)))
                              (if (ly:music-property m 'close-div-start #f)
                                  (append-articulations dots-list-car (close-div-start)))
                              (set! dots-list (reverse
                                               (append (list dots-list-car)
                                                       (cdr dots-list))))

                              (set! dots-list (append
                                               (if (not (slur-stop-note? last-note))
                                                   (list (make-music
                                                          'ContextSpeccedMusic
                                                          'context-type
                                                          'Bottom
                                                          'element
                                                          (make-music
                                                           'PropertySet
                                                           'value
                                                           #t
                                                           'symbol
                                                           'melismaBusy)))
                                                   (list empty-music))
                                               dots-list
                                               (if (not (slur-stop-note? last-note))
                                                   (list (make-music
                                                          'ContextSpeccedMusic
                                                          'context-type
                                                          'Bottom
                                                          'element
                                                          (make-music
                                                           'PropertyUnset
                                                           'symbol
                                                           'melismaBusy)))
                                                   (list empty-music))
                                               )))
                             ; zero duradot-sum
                             (set! dots-list #f)
                             )
                         ) ; end if (slur-stopped?)

                     (if (not must-reverse)
                         (if (list? dots-list)
                             (begin
                              (if (ly:music-property m 'open-div-start #f)
                                  (begin
                                   (delete-articulations m (open-div-start))
                                   ; (set! dots-list
                                   ;                                          (append dots-list
                                   ;                                            (list (make-music 'SkipEvent
                                   ;                                                    'duration (ly:make-duration 0 0 0)
                                   ;                                                    'articulations
                                   ;                                                    (list (open-div-start))))))

                                   )
                                  )
                              (if (ly:music-property m 'close-div-start #f)
                                  (begin
                                   (delete-articulations m (close-div-start))
                                   ; (set! dots-list
                                   ;                                          (append dots-list
                                   ;                                            (list (make-music 'SkipEvent
                                   ;                                                    'duration (ly:make-duration 0 0 0)
                                   ;                                                    'articulations
                                   ;                                                    (list (close-div-start))))))
                                   )
                                  )
                              (set! m (make-sequential-music
                                       (append
                                        (list m)
                                        dots-list
                                        ))))
                             (begin
                              (set! m (make-sequential-music (list m))))
                             ) ; end if
                         ;; else must-reverse = #t
                         (set! m (make-sequential-music
                                  (append
                                   (list m)
                                   ; (list (make-music
                                   ;                                           'ContextSpeccedMusic
                                   ;                                           'context-type
                                   ;                                           'Bottom
                                   ;                                           'element
                                   ;                                           (make-music
                                   ;                                            'PropertySet
                                   ;                                            'value
                                   ;                                            #t
                                   ;                                            'symbol
                                   ;                                            'melismaBusy)))
                                   (make-list duradot32 			q32)
                                   (make-list duradot16 			q16)
                                   (make-list duradot8  			q8)
                                   (make-list duradot4  			q4)
                                   (make-list duradot4-extra  q4)
                                   (make-list duradot8-extra  q8)
                                   (make-list duradot16-extra q16)
                                   (make-list duradot32-extra q32)
                                   ; (list (make-music
                                   ;                                           'ContextSpeccedMusic
                                   ;                                           'context-type
                                   ;                                           'Bottom
                                   ;                                           'element
                                   ;                                           (make-music
                                   ;                                            'PropertyUnset
                                   ;                                            'symbol
                                   ;                                            'melismaBusy)))
                                   )))
                         ) ; end (if (equal? must-reverse #f)

                     ;; RESET
                     (if slur-stopped?
                         (begin
                          (set! duradot-sum 0)
                          (set! slur-started? #f)
                          (set! slur-stopped? #f)
                          (set! in-slur? #f)))
                     (set! note-in-tie? (tied-note? m))
                     )
                    )

                (set! last-dur dur)
                (set! last-moment (ly:make-moment last-dur))
                (set! evaluated-moment (ly:moment-add evaluated-moment last-moment))
                (set! evaluated-dur (+ evaluated-dur last-dur))
                )
              )
             )
            ;-----------------------------------------------------------
            ) ;; end cond
           m) ;; end lambda (m)
         muscopy) ;; end music-map
        (set! key-changes (append key-changes (list mus-key-changes)))
        muscopy) ;; end let
      ) ;; end define-music-function
    ) ;; end define

  (define solmisasiLyric
    (define-music-function (mus) (ly:music?)
      (let* ((newmus (empty-music))
             (elems (music-flatten (ly:music-property mus 'elements '())))
             (newelems (list))
             (rp rest-pos)
             (ri 0))
        (while (not (null? rp))
               (set! ri (- (car rp) 1))
               (set! rp (cdr rp))
               (if (> (length elems) ri)
                   (set! elems
                         (append (drop-right elems (- (length elems) ri))
                                 (list (make-music 'LyricEvent
                                                   'text ""
                                                   'duration (ly:make-duration 2)))
                                 (take-right elems (- (length elems) ri)))))
               )
        (set! newmus (make-sequential-music elems))
        newmus))
    ) ; end solmisasiLyric

  (define solmisasiLyrics solmisasiLyric)

  ) % end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_MUSIC_PARSER_LOADED #t)
#(if (defined? 'LOGGING_LOADED)
     (solmisasi:log "* Solmisasi music parser module has been loaded.\n"))