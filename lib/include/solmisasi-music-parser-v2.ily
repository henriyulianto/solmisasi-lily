%% solmisasi-music-parser-v2.ily
%%
%% (Part of "solmisasi-lily" library for Lilypond)
%%
%% Copyright (C) 2016-2024 - Henri Yulianto
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

#(if
  (ly:version? < (list 2 21 0))
  (use-modules (scm song-util))
  (use-modules (lily song-util))
  )

#(define (sol:message arg . rest)
   #f)

#(begin
  ;; Konstanta untuk solmisasiMusic
  (define _KEY_SIG_PROP 	'solmisasi-key-sig)
  (define _TIME_SIG_PROP 	'solmisasi-time-sig)
  (define _REST_PROP 		'solmisasi-rest)
  (define _DOT_NOTE_PROP 	'solmisasi-dot-note)
  (define _EXPERIMENTAL		#t)

  ;; Variabel
  (define now 					ZERO-MOMENT)
  (define elapsed				ZERO-MOMENT)
  (define unit					(ly:make-moment 1/32))
  (define units-in-beat			8)
  (define beam-group-length		(ly:make-moment 1/4))
  (define measure-length 		(ly:make-moment 4/4))
  (define current-time-sig		#f)
  (define last-pitch			#f)
  (define last-pitch-solmisasi 	#f)
  (define last-note				#f)
  (define major-tonic-pitch 	(ly:make-pitch 0 0 0))
  (define key-changes			'())
  )

#(define melismaBegin
   (context-spec-music
    (make-property-set 'melismaBusy #t)
    'Bottom))

#(define melismaEnd
   (context-spec-music
    (make-property-unset 'melismaBusy)
    'Bottom))

#(define (is-dot-note?	n)
   (equal? #t (ly:music-property n 'solmisasi-dot-note #f)))

#(define (make-tie-event trans)
   (make-music 'TieEvent
               'tweaks (list (cons 'transparent trans))))

#(define (make-slur-event dir)
   (make-music 'SlurEvent
               'span-direction dir))

#(define (make-beam-event dir)
   (if _AUTO_BEAMING
       (make-music 'BeamEvent
                   'span-direction dir)
       (empty-music)
       ))

#(define (make-textscript-event dir txt)
   (make-music 'TextScriptEvent
               'direction dir
               'text txt))

#(define (make-solmisasi-dot-note music dur)
   (set! last-pitch
         (cond
          ((note-event? music)
           (ly:music-property music 'pitch))
          ((rest-event? music)
           (if (ly:pitch? last-pitch)
               last-pitch
               (if (and (defined? last-note)
                        (ly:music-property last-note 'pitch #f))
                   (ly:music-property last-note 'pitch)
                   (ly:make-pitch 0 0))))
          (else
           (ly:make-pitch 0 0))))
   (make-music ; type
               (cond
                ((note-event? music)
                 'NoteEvent)
                ((rest-event? music)
                 'RestEvent)
                (else
                 'EventChord))
               ; pitch
               'pitch
               (cond
                ((note-event? music)
                 (ly:music-property music 'pitch))
                ((rest-event? music)
                 (if (ly:pitch? last-pitch)
                     last-pitch
                     (if (and (defined? last-note)
                              (ly:music-property last-note 'pitch #f))
                         (ly:music-property last-note 'pitch)
                         (ly:make-pitch 0 0))))
                (else
                 (ly:make-pitch 0 0)))
               ; solmisasi-dot-note
               'solmisasi-dot-note #t
               'duration dur))

#(define (clean-music-list lst)
   (cond
    ((ly:music? lst)
     lst)
    ((list? lst)
     (filter (compose not unspecified?)
             (filter (lambda (x)
                       (not (equal? x (empty-music))))
                     lst)))
    (else
     (ly:error "Unknown error."))))

#(define (break-length-and-dots note/rest)
   (let* ((dur (ly:music-property note/rest 'duration))
          (durlog (ly:duration-log dur))
          (durdots	(make-list (ly:duration-dot-count dur) 1))
          (arti (ly:music-property note/rest 'articulations #f))
          (scale (ly:duration-scale dur))
          (event-type (cond
                       ((note-event? note/rest)
                        'NoteEvent)
                       ((rest-event? note/rest)
                        'RestEvent))))
     (display (format #f "durlog = ~a" durlog))(newline)
     (if (null? durdots)
         (begin
          (if (and (ly:music-property note/rest 'div-span-stop #f)
                   (not (is-dot-note? note/rest)))
              (set! note/rest (append-articulations! note/rest (div-span-stop))))
          (if (ly:music-property note/rest 'open-div-start  #f)
              (set! note/rest (append-articulations! note/rest (open-div-start))))
          (if (ly:music-property note/rest 'close-div-start  #f)
              (set! note/rest (append-articulations! note/rest (close-div-start))))
          note/rest)
         ; else
         (begin
          (if arti
              (ly:music-set-property! note/rest 'articulations arti))
          (clean-music-list
           (append
            (list (make-music event-type
                              note/rest
                              'duration (ly:make-duration durlog 0 scale)))
            (if (note-event? note/rest)
                (list melismaBegin)
                (list (empty-music)))
            (append-map
             (lambda (x)
               (set! durlog (1+ durlog))
               (list (make-solmisasi-dot-note note/rest (ly:make-duration durlog 0))))
             durdots)
            (if (note-event? note/rest)
                (list melismaEnd)
                (list (empty-music)))
            ))
          ))))

#(define (solmisasi-completion note/rest)
   (let* ((beat-unit							(ly:moment-mul unit (ly:make-moment units-in-beat)))
          (is-onbeat?							(equal? ZERO-MOMENT
                                    (ly:moment-mod now beat-unit)))
          (note/rest-dur					(ly:music-property note/rest 'duration))
          (note/rest-len					(ly:duration-length note/rest-dur))
          (note/rest-upbeat 			(ly:moment-sub beat-unit
                                              (ly:moment-mod note/rest-len beat-unit)))
          (main-num								0)
          (dots-num								0)
          (event-type (cond
                       ((note-event? note/rest)
                        'NoteEvent)
                       ((rest-event? note/rest)
                        'RestEvent)))
          (note/rest-first				#f)
          (note/rest-rest					#f)
          (note/rest-list					(list)))
     (set! note/rest (make-music event-type note/rest))
     (if (note-event? note/rest)
         (begin
          (display (format #f "Beat unit = ~a" beat-unit))(newline)
          (display (if is-onbeat? " onbeat" ""))
          (set! last-pitch (ly:music-property note/rest 'pitch))
          (set! last-note note/rest)))
     ;; If we're on beat:
     ;;   If length <= beat-unit
     ;;     then no change
     ;;   else
     ;;     process beat-unit . rest
     ;; else (not on beat):
     ;;   If length + now <= beat-unit
     ;;     then no change
     ;;   else
     ;;     process upbeat  . rest
     ;(if (not (is-dot-note? note/rest))
     ;(display (format "Music:~a; now:~a; is-onbeat?:~a; note/rest-len:~a; beat-unit:~a;\n"
     ;(music->lily-string note/rest) now is-onbeat? note/rest-len beat-unit)))
     (cond
      ((not (memq (ly:moment-main-denominator note/rest-len) '(1 2 4 8 16 32)))
       (set! note/rest-list note/rest)
       (set! now (ly:moment-add now (ly:duration-length note/rest-dur)))
       )
      (else
       (if is-onbeat?
           (if (ly:moment<=? note/rest-len beat-unit)
               (begin
                (set! note/rest-list (break-length-and-dots note/rest))
                (if (ly:moment<? note/rest-len beat-unit);beam-group-length)
                    (cond
                     ((or (note-event? note/rest-list)
                          (rest-event? note/rest-list))
                      (append-articulations! note/rest-list (make-beam-event START)))
                     ((list? note/rest-list)
                      (set-car! note/rest-list
                                (append-articulations! (car note/rest-list)
                                                       (make-beam-event START))))
                     (else
                      (display "ERROR"))))
                (set! now (ly:moment-add now (ly:music-length note/rest)))
                )
               ; else
               (begin
                (set! note/rest-first (ly:music-deep-copy note/rest))
                (if (and (ly:music-property note/rest-first 'div-span-stop #f)
                         (not (is-dot-note? note/rest-first)))
                    (set! note/rest-first
                          (append-articulations! note/rest-first (div-span-stop))))
                (set! note/rest-rest (ly:music-deep-copy note/rest))
                (for-each
                 (lambda (x)
                   (ly:music-set-property! note/rest-rest x '()))
                 '(tweaks articulations))
                (set! (ly:music-property note/rest-first 'duration)
                      (moment->duration beat-unit))
                (set! (ly:music-property note/rest-rest 'duration)
                      (moment->duration (ly:moment-sub note/rest-len beat-unit)))
                (set! now (ly:moment-add now (ly:music-length note/rest-first)))
                ))
           ; else (not onbeat)
           (begin
            (if (ly:moment<? (ly:moment-mod now beat-unit) (ly:make-moment 1/8))
                (set! beat-unit (ly:make-moment 1/16)))
            (if (ly:moment<=? (ly:moment-add (ly:moment-mod now beat-unit) note/rest-len)
                              beat-unit)
                (begin
                 (set! note/rest-list (break-length-and-dots note/rest))
                 (if (equal? (ly:moment-add (ly:moment-mod now beat-unit) note/rest-len)
                             beat-unit);beam-group-length)
                     (cond
                      ((or (note-event? note/rest-list)
                           (rest-event? note/rest-list))
                       (append-articulations! note/rest-list (make-beam-event STOP)))
                      ((list? note/rest-list)
                       (list-set! note/rest-list (1- (length note/rest-list))
                                  (append-articulations! (last note/rest-list)
                                                         (make-beam-event STOP))))
                      (else
                       (display "ERROR"))))
                 (set! now (ly:moment-add now (ly:music-length note/rest)))
                 )
                ; else
                (begin
                 (set! note/rest-first (ly:music-deep-copy note/rest))
                 (if (and (ly:music-property note/rest-first 'div-span-stop #f)
                          (not (is-dot-note? note/rest-first)))
                     (set! note/rest-first
                           (append-articulations! note/rest-first (div-span-stop))))
                 (set! note/rest-rest (ly:music-deep-copy note/rest))
                 (for-each
                  (lambda (x)
                    (ly:music-set-property! note/rest-rest x '()))
                  '(tweaks articulations))
                 (set! (ly:music-property note/rest-first 'duration)
                       (moment->duration (ly:moment-sub beat-unit
                                                        (ly:moment-mod now beat-unit))))
                 (if (equal? (ly:moment-add (ly:moment-mod now beat-unit)
                                            (ly:music-length note/rest-first))
                             beat-unit);beam-group-length)
                     (append-articulations! note/rest-first (make-beam-event STOP)))
                 (set! (ly:music-property note/rest-rest 'duration)
                       (moment->duration (ly:moment-sub note/rest-len
                                                        (ly:music-length note/rest-first))))
                 (set! now (ly:moment-add now (ly:music-length note/rest-first)))
                 ))))
       ))
     ;; If there are duration splits
     (if (ly:music? note/rest-rest)
         (begin
          ;; dots for the rest
          (set! (ly:music-property note/rest-rest 'pitch)
                (ly:make-pitch -6 0 0))
          (set! (ly:music-property note/rest-rest 'solmisasi-dot-note) #t)
          ;(display (is-dot-note? note/rest-first))
          ;(if (and (not (is-dot-note? note/rest-first))
          ;         (slur-stop-note? note/rest-first))
          (delete-articulations! note/rest-rest (make-slur-event START))
          (delete-articulations! note/rest-rest (make-slur-event STOP))
          ;(append-articulations! note/rest-first (make-tie-event #f))
          ;(append-tweaks! note/rest-rest (list (quote color) 0.5 0.5 0.5))
          ;)
          (set! note/rest-list
                (flatten-list
                 (list note/rest-first
                       (if (and (not (is-dot-note? note/rest-first))
                                (not (slur-start-note? note/rest-first))
                                (is-dot-note? note/rest-rest)
                                (not (rest-event? note/rest-first)))
                           melismaBegin
                           (empty-music))
                       (solmisasi-completion note/rest-rest)
                       (if (and (not (is-dot-note? note/rest-first))
                                (not (slur-start-note? note/rest-first))
                                (is-dot-note? note/rest-rest)
                                (not (rest-event? note/rest-first)))
                           melismaEnd
                           (empty-music))
                       )))
          ))

     ; (display current-time-sig)(newline)
     ;      (display-scheme-music major-tonic-pitch)
     ;      (display key-changes)(newline)
     (clean-music-list note/rest-list)) ; end let
   ) % end func

#(define (parse-time-signature-music m)
   (let* ((numerator-num (ly:music-property m 'numerator))
          (denominator-num (ly:music-property m 'denominator))
          (time-sig-fraction (cons numerator-num denominator-num)) )
     (set! now ZERO-MOMENT)
     (set! measure-length (fraction->moment time-sig-fraction))
     (set! current-time-sig (/ numerator-num denominator-num))
     (set! units-in-beat
           (if (and (= (modulo numerator-num 3) 0)
                    (or (= denominator-num 8) (= denominator-num 16)))
               12
               8))
     (ly:music-set-property! m _TIME_SIG_PROP (cons numerator-num denominator-num))
     (make-sequential-music
      (append
       (list (beam_grouping_by_time_sig time-sig-fraction))
       (list m)))))

#(define (parse-key-change-events m)
   (let* ((pitch-alist (ly:music-property m 'pitch-alist))
          (key-alts (filter (lambda (a) (not (= 0 (cdr a)))) pitch-alist))
          (keysig-alt-count (get-keysig-alt-count key-alts))
          (major-tonic-number (get-major-tonic keysig-alt-count))
          (tonic (ly:music-property m 'tonic))
          (tonic-num (ly:pitch-notename tonic))
          ;; mode 1-7, 1=major 6=minor
          (mode (+ 1 (modulo (- tonic-num major-tonic-number) 7)))
          (modeStr '("do" "re" "mi" "fa" "sol" "la" "si"))
          (transposed-last-pitch #f)
          (last-major-tonic-pitch major-tonic-pitch)
          (key-sig-string "")
          (last-key-str ""))
     (set! major-tonic-pitch (ly:make-pitch
                              0 major-tonic-number
                              (or (assoc-ref key-alts major-tonic-number) 0)
                              ))
     (set! key-sig-string
           (format #f "~a ＝ ~a"
                   (list-ref modeStr (1- mode))
                   (get-key-sig-string tonic)))
     (set! transposed-last-pitch
           (if last-pitch
               (ly:pitch-diff last-pitch major-tonic-pitch)
               #f
               ))
     (if (not (string=? key-sig-string last-key-str))
         (begin
          (set! last-key-str key-sig-string)
          (if (or (not key-changes)
                  (null? key-changes))
              (set! key-changes (list (list now last-key-str)))
              (if (not (member (list now last-key-str) key-changes))
                  (append key-changes
                          (list (list now last-key-str)))))
          ))
     ;; simpan key aktual untuk digunakan dalam penulisan nada dasar sistem solmisasi
     (ly:music-set-property! m 'solmisasi-key-sig (cons mode tonic))
     (ly:music-set-property! m 'last-pitch last-pitch)
     (ly:music-set-property! m 'last-pitch-solmisasi transposed-last-pitch)

     ;; konversi key sig ke c major, perlukah?
     ;(ly:music-set-property! m 'pitch-alist '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)))
     ;(ly:music-set-property! m 'tonic (ly:make-pitch 0 0 0))

     m))

#(define (parse-note-and-rest-events m)
   (let* ((pitch (ly:music-property m 'pitch #f))
          (pitch-solmisasi (if (ly:pitch? pitch)
                               (ly:pitch-transpose pitch
                                                   (ly:pitch-negate
                                                    (ly:pitch-diff major-tonic-pitch
                                                                   (ly:make-pitch 0 0 0))))
                               last-pitch-solmisasi))
          (volta-pitch (ly:music-property m 'last-pitch-for-volta #f))
          (to-solmisasi (lambda (x)
                          (let ((solm (solmisasi-completion x)))
                            (if (list? solm)
                                (set! solm (make-sequential-music solm)))
                            solm))))
     (if (ly:pitch? pitch)
         (begin
          (set! last-pitch pitch)
          ;; transpose ke c major agar lebih mudah penanganannya
          (ly:music-set-property! m 'pitch-solmisasi pitch-solmisasi)
          (set! last-pitch-solmisasi pitch-solmisasi))
         (begin
          (ly:music-set-property! m 'pitch last-pitch)
          (ly:music-set-property! m 'pitch-solmisasi last-pitch-solmisasi)))
     (if volta-pitch
         (set! last-pitch volta-pitch))
     (display now)(newline)
     (to-solmisasi m)))

#(define (process-skip-events m)
   (set! last-pitch #f)
   (set! last-pitch-solmisasi #f)
   (set! now (ly:moment-add
              now
              (ly:duration-length (ly:music-property m 'duration))))
   m)

#(define (process-partial-music m)
   (let* ((partialset-element (find-child-named m 'PartialSet))
          (partial-moment
           (if (ly:music? partialset-element)
               (ly:duration-length
                (ly:music-property partialset-element 'duration))
               #f)))
     (if (ly:moment? partial-moment)
         (set! now (ly:moment-add now (ly:moment-sub measure-length partial-moment))))
     (display "------- PARTIAL -------")(newline)
     (display now)(newline)
     m))

solmisasiMusic =
#(define-music-function (music) (ly:music?)
   (let* ((solmisasified-music (ly:music-deep-copy music)))
     (set! now ZERO-MOMENT)
     ;-----------------------------------------------------------
     ; Initialize music with 4/4 time signature
     (set! solmisasified-music
           (make-sequential-music
            (append
             ;(list (beam_grouping_by_time_sig (cons 4 4)))
             (list solmisasified-music))))
     ;-----------------------------------------------------------
     ; Event: ChordEvent
     (set! solmisasified-music
           (prepare-chord-open-close
            (prepare-chords solmisasified-music)))
     ;-----------------------------------------------------------
     ; Process the whole music
     (map-some-music
      (lambda (m)
        (or
         ;; ---- TIME SIGNATURE
         (and (music-is-of-type? m 'time-signature-music)
              (parse-time-signature-music m))
         ;; ---- KEY SIGNATURE
         (and (music-is-of-type? m 'key-change-event)
              (parse-key-change-events m))
         ;; ---- PARTIALSET
         (and (and (music-name? m 'ContextSpeccedMusic)
                   ;(music-property-value? m 'context-type 'Score)
                   ;(music-property-value? m 'descend-only #t)
                   (music-has-property? m 'element)
                   (ly:music? (find-child-named m 'PartialSet)))
              (process-partial-music m))
         ;; ---- SKIPS
         (and (music-is-of-type? m 'skip-event)
              (process-skip-events m))
         ;; ---- NOTE or REST
         (and (or (note-event? m) (rest-event? m))
              (parse-note-and-rest-events m))
         ) ; end or
        ) ; end lambda
      solmisasified-music) ; end map-some-music
     solmisasified-music) ; end let
   )
%%% END music function solmisasiMusic

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_MUSIC_PARSER_LOADED #t)
#(if (defined? 'LOGGING_LOADED)
     (solmisasi:log "* Solmisasi music parser (experimental v2) module has been loaded."))