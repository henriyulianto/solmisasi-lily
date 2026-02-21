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

% EXTRA CONFIG
#(define-public _AUTO_BEAMING #t)

% Due to 2.25 changes
#(define-public (empty-music) (make-music 'Music))

%% From: song-util.scm (2.24 and below)

#(define-public (music-property-value? music property value)
   "Return @code{#t} iff @var{music}'s @var{property} is equal to
@var{value}."
   (equal? (ly:music-property music property) value))

#(define-public (music-name? music name)
   "Return @code{#t} iff @var{music}'s name is @var{name}."
   (if (list? name)
       (member (ly:music-property music 'name) name)
       (music-property-value? music 'name name)))

#(define-public (music-property? music property)
   "Return @code{#t} iff @var{music} is a property setter
and sets or unsets @var{property}."
   (and (music-name? music '(PropertySet PropertyUnset))
        (music-property-value? music 'symbol property)))

#(define-public (music-has-property? music property)
   "Return @code{#t} iff @var{music} contains @var{property}."
   (not (eq? (ly:music-property music property) '())))

#(define-public (property-value music)
   "Return value of a property setter @var{music}.
If it unsets the property, return @code{#f}."
   (if (music-name? music 'PropertyUnset)
       #f
       (ly:music-property music 'value)))

#(define-public (music-elements music)
   "Return list of all @var{music}'s top-level children."
   (let ((elt (ly:music-property music 'element))
         (elts (ly:music-property music 'elements))
         (arts (ly:music-property music 'articulations)))
     (if (pair? arts)
         (set! elts (append elts arts)))
     (if (null? elt)
         elts
         (cons elt elts))))

#(define-public (find-child music predicate)
   "Find the first node in @var{music} that satisfies @var{predicate}."
   (define (find-child queue)
     (if (null? queue)
         #f
         (let ((elt (car queue)))
           (if (predicate elt)
               elt
               (find-child (append (music-elements elt) (cdr queue)))))))
   (find-child (list music)))

#(define-public (find-child-named music name)
   "Return the first child in @var{music} that is named @var{name}."
   (find-child music (lambda (elt) (music-name? elt name))))

#(define-public (process-music music function)
   "Process all nodes of @var{music} (including @var{music}) in the DFS order.
Apply @var{function} on each of the nodes.  If @var{function} applied on a
node returns @code{#t}, don't process the node's subtree.

If a non-boolean is returned, it is considered the material to recurse."
   (define (process-music queue)
     (if (not (null? queue))
         (let* ((elt (car queue))
                (stop (function elt)))
           (process-music (if (boolean? stop)
                              (if stop
                                  (cdr queue)
                                  (append (music-elements elt) (cdr queue)))
                              ((if (cheap-list? stop) append cons)
                               stop (cdr queue)))))))
   (process-music (list music)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (ly:moment<=? momI momII)
   "Returns a boolean whether momI is less than or equal to momII."
   (or (ly:moment<? momI momII) (equal? momI momII)))

#(define (moment->number moment)
   "Returns rational number from moment."
   (/ (ly:moment-main-numerator moment) (ly:moment-main-denominator moment)))

#(define (moment->duration moment)
   "Try to convert moment to a duration suitable for displaying a note or a rest,
so in the following form : (ly:make-duration k dots 1 1)
Note that, if moment=5/8, for example, no duration of this form is possible."
   (let* ((p (ly:moment-main-numerator moment))
          (q (ly:moment-main-denominator moment))
          (k (- (ly:intlog2 q) (ly:intlog2 p))))
     (if (< (ash p k) q) (set! k (1+ k))) ; (ash p k) = p * 2^k
     (if (> k 6)
         (ly:make-duration 6 0) ; 6 means 64th (max value).
         (let loop ((p1 (- (ash p k) q))
                    (dots 0))
           (let ((p2 (ash p1 1)))
             ;; (format #t "p1 = ~a ; p2 = ~a\n" p1 p2)
             (if (>= p2 q)
                 (loop (- p2 q) (1+ dots))
                 ;; it seems that (not documented) :
                 ;;    (ly:duration->moment (ly:make-duration a b c d)) =
                 ;;    (ly:moment-mul (ly:duration->moment (ly:make-duration a b))
                 ;;                   (ly:make-moment c d))
                 (let* ((dur (ly:make-duration k dots))        ; duration for displaying the note
                                                               (dur-len (ly:duration->moment dur))     ; his display length.
                                                               (frac (ly:moment-div moment dur-len))) ; to adjust to the real length
                   (ly:make-duration k dots
                                     (ly:moment-main-numerator frac) ; frac = 1/1 for moment = 3/4, 7/8 etc ..
                                     (ly:moment-main-denominator frac)))))))))

#(define-public (compose . functions)
   (let ((functions* (drop-right functions 1))
         (last-function (last functions)))
     (letrec ((reduce (lambda (x functions)
                        (if (null? functions)
                            x
                            (reduce ((car functions) x) (cdr functions))))))
       (lambda args (reduce (apply (last functions) args) (reverse functions*))))))

#(define (insert-at new k l)
   (cond ((null? l)
          (list new))
         ((zero? k)
          (append (list new) l))
         (else
          (append (car l)
                  (insert-at new (1- k) (cdr l))))))

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
       (ly:format "\\include \"~a/extension/solmisasi-~a.ily\"" SOLMISASI_LIB_DIR o))
      (ly:message (ly:format"* Extension \"~a\" has been loaded." o))
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
#(define transposeNaikSatuOktaf	  transposeUpOneOctave) % in Bahasa Indonesia

%% EXTERNAL LIBRARIES
\include "imported/shapeII.ily"
\include "imported/extract-chords.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FUNCTIONS FOR SOLMISASI MUSIC PARSER
#(begin

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

  (define (append-tweaks! m a)
    (ly:music-set-property! m 'tweaks
                            (append
                             (ly:music-property m 'tweaks)
                             (list a)))
    m)

  (define (append-articulations! m a)
    (ly:music-set-property! m 'articulations
                            (append
                             (ly:music-property m 'articulations)
                             (list a)))
    m)

  (define (delete-articulations! m a)
    (ly:music-set-property! m 'articulations
                            (delete
                             a (ly:music-property m 'articulations)))
    m)

  (define (close-div-start)
    (make-music 'TextSpanEvent
                'tweaks (list (cons
                               (cons 'TextSpanner 'stencil)
                               close-divisi-stencil))
                'direction -1
                'span-direction -1))

  (define (open-div-start)
    (make-music 'TextSpanEvent
                'tweaks (list (cons
                               (cons 'TextSpanner 'stencil)
                               open-divisi-stencil))
                'direction -1
                'span-direction -1))

  (define (div-span-stop)
    (make-music 'TextSpanEvent
                ;'direction -1
                'span-direction 1))

  (define (is-music-chord? m)
    (and (note-event? m)
         (ly:music-property m 'cdr-chords #f)))

  (define (is-music-span-top? m)
    (ly:music-property m 'div-span-stop #f))

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
                   (ly:stencil-stack
                    (ly:stencil-translate-axis (ly:beam::print grob) 0.0 Y)
                    Y DOWN
                    (ly:beam::print grob)
                    0 3.5))
                    #})
              (list (make-music
                     'NoteEvent (car (ly:music-property m 'elements))
                     'cdr-chords (cdr (ly:music-property m 'elements)))))))
       ) ; end lambda
     mus))

  (define (prepare-chord-open-close mus)
    (let* ((stoplst (list))
           (chord-iteration 0)
           ;(rmus #f)
           (last-music (empty-music)))
      ;--> Forward
      (set! chord-iteration 0)
      (music-map
       (lambda (m)
         (cond
          ((or (and (note-event? m) (not (is-music-chord? m)))
               (rest-event? m))
           (if (is-music-chord? last-music)
               (begin
                (set! stoplst (append stoplst (list chord-iteration)))
                (ly:music-set-property! m 'div-span-stop #t))))
          ((and (note-event? m) (is-music-chord? m))
           (if (or (and (not (is-music-chord? last-music))
                        (not (equal? last-music (empty-music))))
                   (rest-event? m))
               (begin
                (set! stoplst (append stoplst (list chord-iteration)))
                (ly:music-set-property! m 'div-span-stop #t))))
          ) ; end cond
         (if (or (note-event? m) (rest-event? m))
             (begin
              (set! chord-iteration (1+ chord-iteration))
              (set! last-music m)))
         m) ; end lambda
       mus)
      ; <-- Backward
      (set! last-music (empty-music))
      (set! chord-iteration 0)
      (music-map
       (lambda (m)
         (if (or (note-event? m) (rest-event? m))
             (begin
              (if (and (not (null? stoplst))
                       (eq? chord-iteration (1- (car stoplst))))
                  (begin
                   (set! stoplst (cdr stoplst))
                   (if (is-music-chord? m)
                       (ly:music-set-property! m 'close-div-start #t)
                       (ly:music-set-property! m 'open-div-start #t))))
              (set! chord-iteration (1+ chord-iteration))
              (set! last-music m)))
         m) ; end lambda
       mus)
      ;(set! mus (my-retrograde-music rmus))
      mus))

  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILITIES

#(define (get-header-property-value sym)
   "Get key signature summary from header props."
   (assoc-ref (ly:module->alist $defaultheader) sym))

#(define (set-header-property-value sym val)
   "Set the value of key signature summary in header props."
   (module-define! $defaultheader sym val))

%% For tuplets
dot =
#(define-music-function (p d) (ly:pitch? ly:duration?)
   (make-music 'NoteEvent
               'pitch p
               'duration d
               'solmisasi-dot-note #t))

satuDot =
#(define-music-function (p d) (ly:pitch? ly:duration?)
   #{
     \melisma \dot $p $d \melismaEnd
   #}
   )

duaDot =
#(define-music-function (p d) (ly:pitch? ly:duration?)
   #{
     \melisma \dot $p $d \melisma \dot $p $d \melismaEnd
   #}
   )

%% Lyrics
alignSyllable =
#(define-music-function (align) (number?)
   #{ \once\override LyricText.self-alignment-X = #align #})
syairDiKiri = \alignSyllable #LEFT
syairDiKanan = \alignSyllable #RIGHT
leftAlignedSyllable = \alignSyllable #LEFT
rightAlignedSyllable = \alignSyllable #RIGHT

syairOff =
{
  %\override VerticalAlignment.no-alignment = ##t
  \omit LyricText
  \omit LyricExtender
  \omit LyricHyphen
  \omit LyricSpace
  \omit VerticalAxisGroup
  \omit StanzaNumber
  \omit InstrumentName
  \omit BarLine
}
syairOn = \undo \syairOff
lyricsOff = \syairOff
lyricsOn = \undo \syairOff

%% Coloring
bgcolor =
#(define-music-function
  (color toppad bottompad)
  (
    string?
    (number? 0)
    (number? 0)
    )
  #{
    \override Staff.StaffSymbol.transparent = ##f
    \override Staff.StaffSymbol.color = #(eval-string color)
    \override Staff.StaffSymbol.layer = #-10
    \override Staff.StaffSymbol.stencil =
    #(grob-transformer 'stencil
                       (lambda (grob orig)
                         (let* ((X-ext (ly:stencil-extent orig X))
                                (Y-ext (ly:stencil-extent orig Y)))
                           (set! Y-ext (cons
                                        (- (car Y-ext) bottompad)
                                        (+ (cdr Y-ext) toppad)))
                           (ly:grob-set-property! grob 'layer -10)
                           (ly:stencil-add
                            (stencil-with-color
                             (ly:round-filled-box X-ext Y-ext 0)
                             (eval-string color))
                            orig))))
  #})

%% Rehearsal Marks
boxedAlphabetMark =
#(define-music-function (self-alignment-X info)
   ((number? LEFT) (markup? empty-markup))
   #{
     \once \set Score.rehearsalMarkFormatter = #format-mark-alphabet
     \once \override Score.RehearsalMark.stencil =
     #(lambda (grob)
        (if (and (grob::is-live? grob)
                 (ly:grob-property-data grob 'stencil))
            (grob-interpret-markup grob
                                   #{
                                     \markup \pad-to-box #'(0 . 0) #'(0 . 1) \line {
                                       \override #'(thickness . 1.3)
                                       \override #'(box-padding . 0.5)
                                       \box #(ly:grob-property grob 'text)
                                       \hspace #0.25 \smaller\smaller \bold \smallCaps $info
                                     }
                                   #})
            (empty-stencil)))
     \tweak self-alignment-X $self-alignment-X \mark \default
   #})

boxedNumberMark =
#(define-music-function (self-alignment-X info)
   ((number? LEFT) (markup? empty-markup))
   #{
     \once \set Score.rehearsalMarkFormatter = #format-mark-numbers
     \once \override Score.RehearsalMark.stencil =
     #(lambda (grob)
        (if (and (grob::is-live? grob)
                 (ly:grob-property-data grob 'stencil))
            (grob-interpret-markup grob
                                   #{
                                     \markup \pad-to-box #'(0 . 0) #'(0 . 1) \line {
                                       \override #'(thickness . 1.3)
                                       \override #'(box-padding . 0.5)
                                       \box #(ly:grob-property grob 'text)
                                       \hspace #0.25 \smaller\smaller \bold \smallCaps $info
                                     }
                                   #})
            (empty-stencil)))
     \tweak self-alignment-X $self-alignment-X \mark \default
   #})

disallowLineBreak = \override Score.NonMusicalPaperColumn.line-break-permission = ##f
allowLineBreak = \override Score.NonMusicalPaperColumn.line-break-permission = ##t
disallowPageBreak = \override Score.NonMusicalPaperColumn.page-break-permission = ##f
allowLineBreak = \override Score.NonMusicalPaperColumn.page-break-permission = ##t

dynamicsOff = {
  \omit DynamicText
  \omit DynamicTextSpanner
  \omit DynamicLineSpanner
  \omit Hairpin
  \omit TextScript
}
dynamicsOn = \undo \dynamicsOff

spanBarOn = \override Staff.BarLine.allow-span-bar = ##t
spanBarOff = \override Staff.BarLine.allow-span-bar = ##f

dynamicText =
#(define-event-function (mkup) (markup?)
   (make-dynamic-script (markup #:normal-text (#:italic mkup))))

extenderOnSolmisasiOnly =
#(define-music-function (syl) (ly:music?)
   (if (music-is-of-type? syl 'lyric-event)
       #{ \tag #'notangka { \lyricmode { $syl __ } }
          \tag #'notbalok { \lyricmode { $syl } }
       #}
       (empty-music)))

leftAlignedOnSolmisasiOnly =
#(define-music-function (syl) (ly:music?)
   (if (music-is-of-type? syl 'lyric-event)
       #{ \tag #'notangka { \lyricmode { \syairDiKiri $syl } }
          \tag #'notbalok { \lyricmode { $syl } }
       #}
       (empty-music)))

conditional =
#(define-scheme-function (cond what) (boolean? scheme?)
   (if cond what))

%% Printing
#(define is-svg?
   (eq? 'svg (ly:get-option 'backend)))

#(define-markup-command (musical-structure layout props structure)
   (markup?)
   (interpret-markup layout props
                     #{
                       \markup {
                         \pad-markup #0.3
                         \box \pad-markup #0.3
                         \bold \caps \upright #structure
                       }
                     #}))

setStaffShortInstrumentName =
#(define-music-function (new-inst-name) (markup?)
   #{
     \context Staff
     \applyContext
     #(lambda (context)
        (let ((old-inst-name (ly:context-property context 'shortInstrumentName)))
          (ly:context-set-property! context 'savedInstrumentName old-inst-name)
          (ly:context-set-property! context 'shortInstrumentName new-inst-name)))
   #})

resetStaffShortInstrumentName =
#(define-music-function () ()
   #{
     \context Staff
     \applyContext
     #(lambda (context)
        (let ((saved-inst-name (ly:context-property context 'savedInstrumentName)))
          (ly:context-set-property! context 'shortInstrumentName saved-inst-name)))
   #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define MISC_FUNCTIONS_LOADED #t)
#(ly:message "* Miscellaneous functions module has been loaded.")
