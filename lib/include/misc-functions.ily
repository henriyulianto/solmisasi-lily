%% misc-functions.ily
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

%% MACROS

#(define music-flatten
   (lambda (mus)
     "Convert nested sequential musics into single sequential musics"
     (fold
      (lambda (e prv)
        (if (not (music-is-of-type? e 'sequential-music))
            (append prv (list e))
            (append prv (music-flatten (ly:music-property e 'elements)))))
      '() mus)))

#(define (get-keysig-alt-count alt-alist)
   "Return the number of alterations in a key signature,
    (+) for sharps, and (-) for flats."
   (if (null? alt-alist)
       0
       ; else
       (* (length alt-alist) 2 (cdr (car alt-alist)))))

#(define (get-key-alts context)
   "This function is needed because 2.19 and 2.20 define 'keyAlterations, 
    while the same context property was defined as 'keySignature in 2.18."
   (define key-alts (ly:context-property context 'keyAlterations '()))
   (if (equal? key-alts '())
       (ly:context-property context 'keySignature '())
       ; else
       key-alts))

#(define (get-major-tonic alt-count)
   "Return the tonic number of 0-6, as if major scale is used."
   ;; tonik: 0 1 2 3 4 5 6
   ;;        C D E F G A B
   ;;
   ;; alt-count GANJIL
   ;; (alt-count major-tonic)
   ;; (-7 0)  (-5 1)  (-3 2)  (-1 3)  (1 4)  (3 5)  (5 6)  (7 0)
   ;;  7b=Cb   5b=Db   3b=Eb   1b=F    1#=G   3#=A   5#=B  7#=C#
   ;;
   ;; alt-count GENAP
   ;; (alt-count major-tonic)
   ;; (-6 4)  (-4 5)  (-2 6)  (0 0)   (2 1)  (4 2)  (6 3)
   ;;  6b=Gb   4b=Ab   2b=Bb   0=C     2#=D   4#=E  6#=F#
   (if (odd? alt-count)
       (modulo (- (/ (+ alt-count 1) 2) 4) 7)
       ; else
       (modulo (/ alt-count 2) 7)))

#(define (get-key-sig-string pitch)
   "Return the text/string name of the given pitch of key signature."
   (if (ly:pitch? pitch)
       (string-append
        (case (ly:pitch-notename pitch)
          ((0) "C")
          ((1) "D")
          ((2) "E")
          ((3) "F")
          ((4) "G")
          ((5) "A")
          ((6) "B")
          (else "?"))
        (case (ly:pitch-alteration pitch)
          ((1/2) 	"is")
          ((-1/2) (if (member (ly:pitch-notename pitch) '(2 5))
                      "s" "es"))
          (else "")))
       ; else
       "?"))

#(define (have-music? m)
   (and (not (null? m))
        (ly:music? m)
        (ly:moment<? ZERO-MOMENT (ly:music-length m))))

%% For chords open and close lines

#(define (span-point side common dir)
   (let ((iv
          ; compatible with 2.19 onwards -vv
          (ly:generic-bound-extent side common)))
     ;(ly:grob-robust-relative-extent side common X)))
     (if (interval-empty? iv)
         (ly:grob-relative-coordinate side common X)
         (interval-bound iv dir))))

#(define (close-divisi-stencil grob)
   (let* ((left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (common (ly:grob-common-refpoint left-bound right-bound X))
          (left-span (span-point left-bound common RIGHT))
          (right-span (span-point right-bound common LEFT))
          (span-length (- right-span left-span))
          (usable-length
           (- span-length
              (if (zero? (ly:item-break-dir left-bound)) 0.4 0)
              (if (zero? (ly:item-break-dir right-bound)) 0.4 0)))
          (base-markup
           ;(grob-interpret-markup grob
           #{
             \markup {
               \overlay {
                 \solmisasi \not-angka #(ly:make-pitch 1 5 0) #0
                 \solmisasi \not-angka #(ly:make-pitch -1 5 0) #0
               }
             }
           #})
          (note-height
           (* 1.85
              (interval-length
               (ly:stencil-extent (grob-interpret-markup grob base-markup) Y))))
          (bottom-y 1.2)
          (top-y (- note-height bottom-y))
          (middle-y (* 0.5 (+ bottom-y top-y)))
          (inc-y 0.25)
          (inc-x 0.0)
          (left-x 1.5)
          (pscmd (format #f "~a setlinewidth 1 setlinecap [0.5 0.5] 0 setdash ~a ~a moveto ~a ~a lineto ~a ~a moveto ~a ~a lineto stroke"
                         0.15
                         (+ left-x usable-length (* -1 inc-x)) 	(+ inc-y middle-y)
                         (+ left-x inc-x) 											(+ inc-y bottom-y)
                         (+ left-x usable-length (* -1 inc-x))	(+ inc-y middle-y)
                         (+ left-x inc-x)											(+ inc-y top-y)
                         ))
          )
     (grob-interpret-markup grob
                            #{
                              \markup {
                                \with-dimensions #'(0.0001 . 0.0001) #'(0.0001 . 0.0001)
                                \postscript #pscmd
                              }
                            #})
     ))

#(define (open-divisi-stencil grob)
   (let* ((left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (common (ly:grob-common-refpoint left-bound right-bound X))
          (left-span (span-point left-bound common RIGHT))
          (right-span (span-point right-bound common LEFT))
          (span-length (- right-span left-span))
          (usable-length
           (- span-length
              (if (zero? (ly:item-break-dir left-bound)) 0.4 0)
              (if (zero? (ly:item-break-dir right-bound)) 0.4 0)))
          (base-markup
           ;(grob-interpret-markup grob
           #{
             \markup {
               \overlay {
                 \solmisasi \not-angka #(ly:make-pitch 1 5 0) #0
                 \solmisasi \not-angka #(ly:make-pitch -1 5 0) #0
               }
             }
           #})
          (note-height
           (* 1.85
              (interval-length
               (ly:stencil-extent (grob-interpret-markup grob base-markup) Y))))
          (bottom-y 1.2)
          (top-y (- note-height bottom-y))
          (middle-y (* 0.5 (+ bottom-y top-y)))
          (inc-y 0.1)
          (inc-x 0.2)
          (left-x 1.5)
          (pscmd (format #f "~a setlinewidth 1 setlinecap [0.5 0.5] 0 setdash ~a ~a moveto ~a ~a lineto ~a ~a moveto ~a ~a lineto stroke"
                         0.15
                         (+ left-x inc-x) 									  		(+ inc-y middle-y)
                         (+ left-x usable-length (* -1 inc-x))	(+ inc-y bottom-y)
                         (+ left-x inc-x) 									  		(+ inc-y middle-y)
                         (+ left-x usable-length (* -1 inc-x))	(+ inc-y top-y)
                         ))
          )
     (grob-interpret-markup grob
                            #{
                              \markup {
                                \with-dimensions #'(0.0001 . 0.0001) #'(0.0001 . 0.0001)
                                \postscript #pscmd
                              }
                            #})
     ))

#(define (get-list-index l el)
   (if (null? l)
       -1
       (if (= (car l) el)
           0
           (let ((result (get-list-index (cdr l) el)))
             (if (= result -1)
                 -1
                 (1+ result))))))

#(define (count-element-less-than-or-equal el l)
   (cond
    ((null? l) 0)
    (else
     (+ (if (<= (car l) el) 1 0) (count-element-less-than-or-equal el (cdr l))))))

#(define (get-first-context-id! mus)
   "Find the name of a ContextSpeccedMusic, possibly naming it"
   (let ((id (ly:music-property mus 'context-id)))
     (if (eq? (ly:music-property mus 'name) 'ContextSpeccedMusic)
         (if (and (string? id)
                  (not (string-null? id)))
             id
             ;; We may reliably give a new context a unique name, but
             ;; not an existing one
             (if (ly:music-property mus 'create-new #f)
                 (let ((id (get-next-unique-voice-name)))
                   (set! (ly:music-property mus 'context-id) id)
                   id)
                 '()))
         '())))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MUSIC/SCHEME/VOID FUNCTIONS

withExtensions =
#(define-void-function (exts) (list?)
   "Load one or some extensions of this library."
   (for-each
    (lambda (o)
      (ly:parser-parse-string
       (ly:parser-clone)
       (format #f "\\include \"~a/extension/solmisasi-~a.ily\"" _SOLMISASI_LIB_DIR o))
      (solmisasi:log (format #f "* Extension \"~a\" has been loaded." o))
      )
    exts))

at =
#(define-music-function (t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})

music-car =
#(define-music-function (mus) (ly:music?)
   (let
    ((elem 	(ly:music-property mus 'element '()))
     (elems (ly:music-property mus 'elements '()))
     (sim 	(music-is-of-type? mus 'simultaneous-music))
     (rel 	(music-is-of-type? mus 'relative-octave-music))
     (seq 	(music-is-of-type? mus 'sequential-music)))
    (cond
     (rel (make-music 'RelativeOctaveMusic 'element (music-car elem)))
     (sim (make-music 'SimultaneousMusic 'elements (map music-car elems)))
     (seq (make-music 'SequentialMusic 'elements (list (car elems))))
     (else mus))))

music-cdr =
#(define-music-function (mus) (ly:music?)
   (let
    ((elem 	(ly:music-property mus 'element '()))
     (elems (ly:music-property mus 'elements '()))
     (sim 	(music-is-of-type? mus 'simultaneous-music))
     (rel 	(music-is-of-type? mus 'relative-octave-music))
     (seq 	(music-is-of-type? mus 'sequential-music)))
    (cond
     (rel (make-music 'RelativeOctaveMusic 'element (music-cdr elem)))
     (sim (make-music 'SimultaneousMusic 'elements (map music-cdr elems)))
     (seq (make-music 'SequentialMusic 'elements (cdr elems)))
     (else mus))))

music-cons =
#(define-music-function (el mus) (ly:music? ly:music?)
   (let*
    ((elem 		(ly:music-property mus 'element '()))
     (elems 	(ly:music-property mus 'elements '()))
     (sim 		(music-is-of-type? mus 'simultaneous-music))
     (rel 		(music-is-of-type? mus 'relative-octave-music))
     (seq 		(music-is-of-type? mus 'sequential-music))
     (elsim		(music-is-of-type? el  'simultaneous-music))
     (elseq 	(music-is-of-type? el  'sequential-music))
     (elelems (ly:music-property el  'elements '()))
     (rem 		(and seq
                 (not (null? elems))
                 (or (music-is-of-type? (car elems) 'simultaneous-music)
                     (music-is-of-type? (car elems) 'relative-octave-music)
                     (music-is-of-type? (car elems) 'sequential-music)))))
    (cond
     (rem (make-music 'SequentialMusic 'elements
                      (cons (music-cons el (car elems)) (cdr elems))))
     (rel (make-music 'RelativeOctaveMusic 'element
                      (music-cons el elem)))
     (sim (make-music 'SimultaneousMusic 'elements
                      (if elsim
                          (append elelems elems)
                          (cons (music-cons el (car elems)) (cdr elems)))))
     (seq (make-music 'SequentialMusic 'elements
                      (if elseq
                          (append elelems elems)
                          (cons el elems))))
     (else (ly:error "Not a type of music to cons")))))

silence =
#(define-music-function (arg) (ly:music?)
   "Convert note events to skip events."
   (map-some-music
    (lambda (m)
      (and (or (music-is-of-type? m 'note-event)
               (music-is-of-type? m 'rest-event)
               (music-is-of-type? m 'multi-measure-rest))
           (make-sequential-music
            (list (make-music 'SkipEvent m)))))
    arg))

silenceNoMarkEvent =
#(define-music-function (arg) (ly:music?)
   "Convert note events to skip events, and remove all mark events."
   (let ((argCopy (ly:music-deep-copy arg)))
     (map-some-music
      (lambda (m)
        (and (music-is-of-type? m 'mark-event)
             (make-music 'SequentialMusic 'void #t)))
      (silence argCopy))))

flexibleDo =
#(define-music-function (k m) (ly:pitch? ly:music?)
   "Implement a flexible-do notation system."
   #{ \key $k \major
      \transpose do $k { \relative $k { \absolute $m } }
   #})

flexibleLa =
#(define-music-function (k m) (ly:pitch? ly:music?)
   "Implement a flexible-la based music for minor scale."
   (let*
    ((diff-pitch (ly:make-pitch 0 2 FLAT))
     (do-pitch (ly:pitch-transpose k diff-pitch)))
    #{ \key $k \minor
       \transpose la, $k { \relative $do-pitch { \absolute $m } }
    #}))

transposeDownOneOctave =
#(define-music-function (m) (ly:music?)
   "Transpose music down for one octave."
   #{ \transpose c c, { $m } #})

transposeUpOneOctave =
#(define-music-function (m) (ly:music?)
   "Transpose music up for one octave."
   #{ \transpose c c' { $m } #})

beam_grouping_by_time_sig  =
#(define-music-function (ts) (fraction?)
   (cond
    ((equal? ts (cons 2 4)) ; 2/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 3 4)) ; 3/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 ) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 4 4)) ; 4/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 5 4)) ; 5/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 6 4)) ; 6/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 2 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 7 4)) ; 7/4
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 2 2 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 2 3)) ; 2/2
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (2 2 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 3 8)) ; 3/8
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (1) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 5 8)) ; 6/8
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (3 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 6 8)) ; 6/8
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (3 3) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 7 8)) ; 6/8
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (3 2 2) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 9 8)) ; 9/8
                            #{
                              \set beamExceptions = #'( (end .
                                                             ( ( (1 . 8) . (3 3 3) ) )
                                                             ) )
                            #})
    ((equal? ts (cons 12 8)) ; 12/8
                             #{
                               \set beamExceptions = #'( (end .
                                                              ( ( (1 . 8) . (3 3 3 3) ) )
                                                              ) )
                             #})
    (else
     #{ {} #})
    ))


%% ALIASES

#(define movableDo                flexibleDo)
#(define doIs                     flexibleDo)
#(define doEqualsTo               flexibleDo)
#(define doSamaDengan             flexibleDo) % in Bahasa Indonesia
#(define movableLa                flexibleLa)
#(define laIs                     flexibleLa)
#(define laEqualsTo               flexibleLa)
#(define laSamaDengan             flexibleLa) % in Bahasa Indonesia
#(define transposeTurunSatuOktaf  transposeDownOneOctave) % in Bahasa Indonesia
#(define transposeNaikSatuOktaf	 	 transposeUpOneOctave) % in Bahasa Indonesia

%% EXTERNAL LIBRARIES
\include "imported/shapeII.ily"
\include "imported/extract-chords.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define MISC_FUNCTIONS_LOADED #t)
#(if (defined? 'LOGGING_LOADED)
     (solmisasi:log "* Misc functions module has been loaded."))