\version "2.19.82"

%% solmisasi-markups.ily
%%
%% (Part of "solmisasi" library for Lilypond)
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
          (number-stencil (interpret-markup layout props
                            (markup
                             (make-fontsize-markup font-size
                               (number->string num)))))
          (octave-dot-radius (+ 0.18 (* mag-diff 0.25)))
          (octave-dot-padding (+ 0.2 (* mag-diff 0.6)))
          (octave-dot-stencil
           (case (abs diff-octave)
             ((0) #f)
             ((1) (ly:stencil-aligned-to
                   (make-circle-stencil octave-dot-radius 0.001 #t)
                   0 0))
             ((2) (ly:stencil-aligned-to
                   (ly:stencil-combine-at-edge
                    (make-circle-stencil octave-dot-radius 0.001 #t)
                    0
                    -1
                    (make-circle-stencil octave-dot-radius 0.001 #t)
                    octave-dot-padding)
                   0 0))
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
                             (make-line-stencil thickness
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
                      (cons center-x (+ end-y (* mag 0.5)))
                      (cons center-x (- start-y (* mag 0.5))))
                  ))))
     ;; return
     number-stencil))

#(define-markup-command (not-angka layout props pitch base-octave)
   (ly:pitch? number?)
   #:properties ((font-size 0)
                 (thickness 1.5))
   (let*
    ((pitch-name
      (and (ly:pitch? pitch)
           (ly:pitch-notename pitch)))
     (pitch-alter
      (and (ly:pitch? pitch)
           (ly:pitch-alteration pitch)))
     (pitch-octave
      (and (ly:pitch? pitch)
           (ly:pitch-octave pitch)))
     (note-number (+ 1 pitch-name))
     (diff-octave (- pitch-octave base-octave))
     (not-angka-stencil empty-stencil))
    (set! not-angka-stencil
          (not-angka-internal layout props
            note-number pitch-alter diff-octave thickness))
    not-angka-stencil))

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_MARKUPS_LOADED #t)
#(if (defined? 'LOGGING_LOADED)
  (solmisasi:log "* Solmisasi markups module has been loaded.\n"))