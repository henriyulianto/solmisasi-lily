#(define (noteEvent? music)
   (eq? (ly:music-property music 'name) 'NoteEvent))

#(define (no-duration? music)
   (not (ly:duration? (ly:music-property music 'duration))))

#(define (expand-q-chords music); for q chords : see chord-repetition-init.ly
   (expand-repeat-chords! (list 'rhythmic-event) music))

%%%%%%%%%%%%%%%%%%%%%%%%%%  extractNote  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define tagNotExtractNote (gensym))
#(use-modules (ice-9 receive)) %% for the use of receive

#(define (chord->note chord n . args)
   "Return either the note n of chord chord, keeping articulations or if other
numbers are specified in args, a chord with the matching notes."
   (receive (notes others)
     (partition noteEvent? (ly:music-property chord 'elements))
     (if (null? notes)
         chord
         (let* ((len (length notes))
                (res (filter-map
                      (lambda(i)
                        (and (integer? i)
                             (<= i len)
                             (> i 0)
                             (list-ref notes (1- i)))) ; list-ref is zero-based
                      (cons n args)))
                (one-note (cond
                           ((null? res) (list-ref notes (1- len)))
                           ((null? (cdr res))(car res))
                           (else #f))))
           (if one-note
               (begin
                (ly:music-set-property! one-note 'articulations
                                        (append (ly:music-property one-note 'articulations) others))
                one-note)
               (make-event-chord (append res others)))))))

#(define (extract-note music n . args)
   "Extract the note n of each chords in music, keeping articulations.
If other numbers are given in args, the function returns a chord build with all
matching notes. If no note matches, returns the last note of the chord."
   (map-some-music
    (lambda (evt)
      (cond
       ((eq? 'EventChord (ly:music-property evt 'name))
        (let ((tags (ly:music-property evt 'tags)))
          (if (memq tagNotExtractNote tags)
              (ly:music-set-property! evt 'tags ; only remove the tag
                                      (delq tagNotExtractNote tags))
              (set! evt (apply chord->note evt n args)))
          evt))
       (else (and (ly:music-property evt 'duration #f) evt))))
    (expand-q-chords music)))

extractNote =
#(define-music-function (n music )
   (number? ly:music?)
   (extract-note music n))

% useful for notExtractNote
tagify =
#(define-music-function (tag music) (symbol? ly:music?)
   "Add `tag in the tags property of all chords"
   (music-map
    (lambda (evt)
      (if (eq? 'EventChord (ly:music-property evt 'name))
          (ly:music-set-property! evt 'tags
                                  (cons tag (ly:music-property evt 'tags))))
      evt)
    music))

notExtractNote =
#(define-music-function (music) (ly:music?)
   "Avoids music to be extracted by \\extractNote."
   #{
     \tagify #tagNotExtractNote $music
   #})

%%%%%%%%%%%%%%%%%%%%%%%%%%  extractVoice  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define tagNotExtractVoice (gensym))

#(define (extract-voice music d deeper-level?) ;; d as a decimal number
   (define (nth-voice voices n)
     (let ((len (length voices)))
       (list-ref voices (1-   ; list-ref is zero-based !
                              (if (<= n len) n len)))))

   (define (not-extract? x)
     (let ((tags (ly:music-property x 'tags)))
       (and (memq tagNotExtractVoice tags)    ; don't extract anything
            (begin
             (ly:music-set-property! x 'tags
                                     (delq tagNotExtractVoice tags)) ; only remove the tag
             x))))

   (define (myfilter x)
     (or (not-extract? x)
         (case (ly:music-property x 'name #f)
           ((SimultaneousMusic EventChord) x)
           ((OverrideProperty PropertySet VoiceSeparator) #f)
           ((ContextSpeccedMusic)
            (if (eq? (ly:music-property x 'context-type) 'Voice)
                (set! x (myfilter (ly:music-property x 'element #f))))
            x)
           (else (if (ly:music-property x 'duration #f)
                     x
                     (let ((e (ly:music-property x 'element #f))
                           (es (ly:music-property x 'elements #f)))
                       (if e (ly:music-set-property! x 'element (myfilter e)))
                       (if (pair? es)(ly:music-set-property! x 'elements
                                                             (filter myfilter es)))
                       x))))))
   (map-some-music
    (lambda(evt)
      (case (ly:music-property evt 'name)
        ((EventChord) evt)
        ((SimultaneousMusic)
         (or (not-extract? evt)
             (let* ((save-d d)
                    ; if d = 4.321, we'll get :
                    (n (truncate d))                ; n = 4 (the integer part)
                    (next-d (- (* 10  d)(* 10 n)))  ; next-d = 43.21 - 40 = 3.21
                    (voices (filter myfilter (ly:music-property evt 'elements))))
               (set! evt (if (or (null? voices)(< n 1))
                             '()
                             (nth-voice voices (inexact->exact n))))
               (if deeper-level?
                   (begin
                    (set! d (if (< next-d 1) n next-d))        ; keep n if (truncate next-d) = 0
                    (set! evt (extract-voice evt d deeper-level?)))) ; SimultaneousMusic inside?
               (set! d save-d)
               evt)))
        (else (and (ly:music-property evt 'duration #f)
                   evt))))
    music))

extractVoice =
#(define-music-function (n music )
   (integer? ly:music?)
   "Extract in music, the n-th voice of simultaneous music of the same level, keeping 
only basic music events (no more \\Voicexxx or \\new Voice). A Voice separator
doesn't count as a voice."
   (extract-voice music n #f))

deepExtractVoice =
#(define-music-function (x music )
   (number? ly:music?)
   "Behaves like extractVoice, taking first the integer part of x as n argument, but
goes deeper in each simultaneous music, extracting voice of other potential
simultaneous music, taking now as n argument the first digit of the decimal part
of x, then continues always deeper with second digit and so on.
Notes that a digit of 0, means taking previous value of n, so 2 is equivalent to 2,222...
and 2,3 to 2,333..."
   (extract-voice music x #t))

notExtractVoice =
#(define-music-function (music)(ly:music?)
   "Inside an \\extractVoice section, avoids that a part of this section (called
here `music) to be extracted."
   #{ \tag #tagNotExtractVoice $music #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% derivated functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% If you have enter << <c e g> \\ <ais cis fis> >>, the first function will
%% give you c, the second fis
extractPartUpper = #(define-music-function (music )(ly:music?)
                      #{ \extractNote #1000 \extractVoice #1 $music #})

extractPartLower = #(define-music-function (music )(ly:music?)
                      #{ \extractNote #1 \extractVoice #1000 $music #})
%%%%%% shortcuts %%%%%%%
#(define ePU extractPartUpper)
#(define ePL extractPartLower)


%%%%%%%%%%%%%%%%%%%%% addNote

#(define (add-note music notes-to-add)                ; music and notes-to-add as music
   (define (note->chords-arti note)                    ; note as a NoteEvent
     (receive (note-arti chord-arti)
       (partition      ; separates arti for NoteEvent from arti for EventChord
                       (lambda (evt)(memq (ly:music-property evt 'name)
                                          (list 'StringNumberEvent 'StrokeFingerEvent 'FingeringEvent)))
                       (ly:music-property note 'articulations))
       (ly:music-set-property! note 'articulations note-arti)
       chord-arti))
   (let* ((alist      ; a list of pairs of 2 lists : '(notes . articulations)
                      (reverse (let loop ((m (expand-q-chords notes-to-add)) ; q to chords
                                                                             (p '())) ; m = music, p previous value of the list
                                 (case (ly:music-property m 'name)
                                   ((or SkipEvent SkipMusic) ; a skip in notes-to-add means : add nothing
                                                             (cons #f p))           ; add #f to the list
                                   ((NoteEvent)
                                    (acons (list m) (note->chords-arti m) p))
                                   ((EventChord)
                                    (receive (notes arti) ; separates notes from scripts, dynamics etc
                                      (partition noteEvent? (ly:music-property m 'elements))
                                      (if (pair? notes)(acons notes arti p) p)))
                                   (else (let ((e (ly:music-property m 'element)))
                                           (fold loop
                                                 (if (ly:music? e)(loop e p) p)
                                                 (ly:music-property m 'elements))))))))
          (entry #f)  ; will be (car alist)
          (entry? (lambda() (and
                             (pair? alist)
                             (begin (set! entry (car alist))
                               (set! alist (cdr alist))
                               entry))))
          (do-add (lambda (notes arti)
                    (let* ((dur (ly:music-property (car notes) 'duration))
                           (new-notes (map            ; fix all durations to dur
                                                      (lambda(evt)(ly:music-set-property! evt 'duration dur)
                                                        evt)
                                                      (car entry)))            ; the list of new notes
                           (new-arti (cdr entry)))    ; the articulations
                      (append new-notes notes new-arti arti)))))
     ;; combine in chords, each element of alist with notes of music
     (map-some-music
      (lambda(x)
        (case (ly:music-property x 'name)
          ((NoteEvent)(if (entry?)
                          (make-event-chord (do-add (list x) (note->chords-arti x)))
                          x))
          ((EventChord)
           (if (entry?)
               (receive (notes arti) ; separates notes from scripts, dynamics etc
                 (partition noteEvent? (ly:music-property x 'elements))
                 (if (pair? notes)(ly:music-set-property! x 'elements (do-add notes arti)))))
           x)
          (else (and (ly:music-property x 'duration #f) x)))) ; #f means : go deeper
      (expand-q-chords music))))


addNote =
#(define-music-function (music notes)
   (ly:music? ly:music?)
   "Merges in a chord, the first note or chord in `music, with the first note or chord
in `notes, including articulations, then continues to the second one, and so on.
The duration of notes are taken from `music.
In `notes, only note or chord events are kept."
   (add-note #{\relative c' $music  #}   ; the 2 music-parameters will
             #{\relative c' $notes  #})) % be seen in \relative mode


%%%%%%%%%%%%%%%%%%%% addVoice
%% Traditionnal way
addVoice =
#(define-music-function (music newVoice)
   (ly:music? ly:music?)
   #{ << $music \\ $newVoice >> #})


%% Alternate way
addVoiceAlt =
#(define-music-function (music newVoice)
   (ly:music? ly:music?)
   #{ << { \voiceOne $music } \new Voice { \voiceTwo $newVoice } >>
      \oneVoice #})

%%%%%%%%%%%%%%%%%%%%
deleteDynamics =
#(define-music-function (music) (ly:music?)
   (music-filter (lambda (evt)
                   (not (memq (ly:music-property evt 'name)
                              (list
                               'AbsoluteDynamicEvent 'CrescendoEvent 'DecrescendoEvent))))
                 music))

%%%%%%%%%%%%%%%%%%%%%%%%
absolute =
#(define-music-function (music) (ly:music?)
   "A \\relative command will have no effect in the resulting music."
   (make-music 'UnrelativableMusic 'element music))

doubleNote =
#(define-music-function (music) (ly:music?)
   "Double each note with the note an octave higher."
   #{ \addNote \transpose c c' \relative c' { $music } $music #})