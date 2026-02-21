%% solmisasi-markups.ily
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

#(use-modules (lily))

#(define (not-angka-internal layout props num alter diff-octave thickness)
   (let* ((font-size (or (chain-assoc-get 'font-size props) 0))
          (to-be-slashed (> (abs alter) 0))
          (to-be-dotted (> (abs diff-octave) 0))
          (forward
           (case alter
             ((1/2) #t)
             ((-1/2) #f)))
          (mag (magstep font-size))
          (mag-diff (- mag 1.0))
          (thickness (* mag
                        (ly:output-def-lookup layout 'line-thickness)
                        thickness))
          ;; backward slashes might use slope and point in the other direction!
          (dy (* mag (if forward 0.65 -0.65)))
          (number-stencil (interpret-markup
                           layout props
                           #{
                             \markup \fontsize #font-size
                             #(number->string num)
                           #}
                           ))
          (octave-dot-radius (+ 0.18 (* mag-diff 0.25)))
          (octave-dot-padding (+ 0.2 (* mag-diff 0.6)))
          (octave-dot-stencil
           (case (abs diff-octave)
             ((0) #f)
             ((1) (ly:stencil-aligned-to
                   (make-circle-stencil octave-dot-radius 0.001 #t)
                   Y -0))
             ((2) (ly:stencil-aligned-to
                   (ly:stencil-combine-at-edge
                    (make-circle-stencil octave-dot-radius 0.001 #t)
                    Y
                    -1
                    (make-circle-stencil octave-dot-radius 0.001 #t)
                    octave-dot-padding)
                   Y (if (positive? diff-octave) -0.6 0.6)))
             ((3) (ly:stencil-aligned-to
                   (ly:stencil-combine-at-edge
                    (ly:stencil-combine-at-edge
                     (make-circle-stencil octave-dot-radius 0.001 #t)
                     Y
                     -1
                     (make-circle-stencil octave-dot-radius 0.001 #t)
                     octave-dot-padding)
                    Y
                    -1
                    (make-circle-stencil octave-dot-radius 0.001 #t)
                    octave-dot-padding)
                   Y (if (positive? diff-octave) -0.7 0.7)))
             ))
          (num-x (horizontal-slash-interval num forward (ly:stencil-extent number-stencil X) mag))
          (center-y (interval-center (ly:stencil-extent number-stencil Y)))
          (center-x (interval-center (ly:stencil-extent number-stencil X)))
          (start-y (interval-start (ly:stencil-extent number-stencil Y)))
          (end-y (interval-end (ly:stencil-extent number-stencil Y)))
          ;; Use the real extents of the slash, not the whole number,
          ;; because we might translate the slash later on!
          (num-y (interval-widen (cons center-y center-y) (abs dy)))
          (is-sane (and (interval-sane? num-x) (interval-sane? num-y)))
          (slash-stencil (if is-sane
                             (make-line-stencil (if is-svg? (* 1.25 thickness) thickness)
                                                (car num-x) (- (interval-center num-y) dy)
                                                (cdr num-x) (+ (interval-center num-y) dy))
                             #f)))
     ;; chromatic
     (if (and to-be-slashed (ly:stencil? slash-stencil))
         (begin
          ;; for some numbers we need to shift the slash/backslash up or
          ;; down to make the slashed digit look better
          (set! slash-stencil (adjust-slash-stencil num forward slash-stencil mag))
          (set! number-stencil
                (ly:stencil-add number-stencil slash-stencil))))
     ;; octave
     (if (and to-be-dotted
              (ly:stencil? octave-dot-stencil))
         (set! number-stencil
               (ly:stencil-add number-stencil
                               (ly:stencil-translate
                                octave-dot-stencil
                                (if (> diff-octave 0)
                                    (cons center-x (+ end-y (* mag 0.45)))
                                    (cons center-x (- start-y (* mag 0.45))))
                                ))))
     ;; return
     number-stencil))

#(define-public (sol:pitch? x)
   (or (string? x) (list? x) (pair? x) (ly:pitch? x)))

#(define-markup-command (not-angka layout props pitch base-octave)
   (sol:pitch? number?)
   #:properties ((font-size 0)
                 (thickness 1.5))
   (let* ((pit (if (ly:pitch? pitch)
                   pitch
                   (ly:make-pitch 0 0 0)))
          (pitch-name 		(ly:pitch-notename pit))
          (pitch-alter 		(ly:pitch-alteration pit))
          (pitch-octave 	(ly:pitch-octave pit))
          (note-number 		(+ 1 pitch-name))
          (diff-octave 		(- pitch-octave base-octave))
          (not-angka-stencil
           (not-angka-internal layout props
                               note-number pitch-alter diff-octave thickness)))
     not-angka-stencil))

#(define-markup-command (infoNadaBaru layout props nada oktafDasar)
   (ly:pitch? number?)
   (interpret-markup layout props
                     #{
                       \markup {
                         \pad-around #0.1
                         \center-align \concat {
                           \hspace #0.2
                           "="
                           \hspace #0.2
                           \solmisasi \not-angka #nada #oktafDasar
                           \hspace #0.2
                         }
                       }
                     #}))

#(define-markup-command (ekuivalensiNada layout props nada oktafDasar)
   (ly:pitch? number?)
   (interpret-markup layout props
                     #{
                       \markup {
                         \override #'(thickness . 1.5)
                         \box
                         \with-dimensions-from
                         \overlay {
                           \infoNadaBaru #(ly:make-pitch 1 5 1/2) #0
                           \infoNadaBaru #(ly:make-pitch -1 5 -1/2) #0
                         }
                         \bold \infoNadaBaru #nada #oktafDasar
                       }
                     #}))

#(define-markup-command (boxNadaDasar layout props keySigPair)
   (pair?)
   (define key-sig-number-string
     (ly:number->string (car keySigPair)))
   (define key-sig-string (get-key-sig-string (cdr keySigPair)))
   (interpret-markup layout props
                     #{
                       \markup {
                         \override #'(thickness . 1.3)
                         \override #'(box-padding . 0.75)
                         \box
                         \center-align \concat \bold \larger {
                           #key-sig-number-string "=" #key-sig-string
                         }
                       }
                     #}))

#(if (not (defined? 'make-solmisasi-markup))
     (define-markup-command (solmisasi layout props arg)
       (markup?)
       (interpret-markup layout props
                         (markup arg))))

#(if (not (defined? 'make-ekspresi-markup))
     (define-markup-command (ekspresi layout props arg)
       (markup?)
       (interpret-markup layout props
                         (markup arg))))

%% Override "with-url" markup
#(define-markup-command (with-URL layout props url arg)
   (string? markup?)
   #:category graphic
   #:as-string (format #f "~a [~a]"
                       (markup->string arg #:layout layout #:props props)
                       url)
   (interpret-markup layout props
                     (if (not (or (eq? 'ps (ly:get-option 'backend))
                                  (eq? 'cairo (ly:get-option 'backend))))
                         #{ \markup #arg #}
                         #{ \markup \with-url #url #arg #})))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_MARKUPS_LOADED #t)
#(ly:message "* Solmisasi markups module has been loaded.")
