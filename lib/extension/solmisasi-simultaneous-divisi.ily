%%% solmisasi-simultaneous-divisi.ily
%%% A solmisasi-lily extension (separated from release bundle)
%%% By Henri Yulianto, 2020
%%% NOTES:
%%% 1. This only applies best to 2 voices divisi at maximum. So, be warned!
%%% 2. From now on, a dedicated tag is used. The tag's name symbol is 'solmisasi.
%%%    If you also want to engrave the original standard music, please remove
%%%    this tagging by calling: \removeWithTag #'solmisasi

%#(if (and (defined? '_SOLMISASI_LILY_LOADED) _SOLMISASI_LILY_LOADED)

#(define (close-divisi-stencil-vII grob)
   (let* ((left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (bound-details (ly:grob-property grob 'bound-details))
          (left-padding (+ (assoc-get 'padding
                                      (assoc-get 'left bound-details)) 0.0))
          (right-padding (- (assoc-get 'padding
                                       (assoc-get 'right bound-details)) 0.0))
          (common (ly:grob-common-refpoint left-bound right-bound X))
          (left-span (span-point left-bound common RIGHT))
          (right-span (span-point right-bound common LEFT))
          (span-length (- right-span left-span))
          (usable-length
           (- span-length
              (if (zero? (ly:item-break-dir left-bound)) 0.4 0)
              (if (zero? (ly:item-break-dir right-bound)) 0.4 0)
              left-padding
              right-padding))
          (base-markup
           #{
             \markup {
               \overlay {
                 \solmisasi \not-angka #(ly:make-pitch 1 5 0) #0
                 \solmisasi \not-angka #(ly:make-pitch -1 5 0) #0
               }
             }
           #})
          (style (ly:grob-property grob 'style 'dashed-line))
          (note-height
           (* 2.4
              (interval-length
               (ly:stencil-extent (grob-interpret-markup grob base-markup) Y))))
          (bottom-y 1.2)
          (top-y (- note-height bottom-y))
          (middle-y (* 0.5 (+ bottom-y top-y)))
          (inc-y 0.3)
          (inc-x 0.0)
          (left-x (+ 1.7 left-padding))
          (pscmd (format #f "~a setlinewidth 1 setlinecap ~a ~a ~a moveto ~a ~a lineto ~a ~a moveto ~a ~a lineto stroke"
                         0.15
                         (if (equal? style 'dashed-line) "[0.5 0.5] 0 setdash" "")
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

#(define (open-divisi-stencil-vII grob)
   (let* ((left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (bound-details (ly:grob-property grob 'bound-details))
          (left-padding (+ (assoc-get 'padding
                                      (assoc-get 'left bound-details)) 0.0))
          (right-padding (+ (assoc-get 'padding
                                       (assoc-get 'right bound-details)) 0.0))
          (common (ly:grob-common-refpoint left-bound right-bound X))
          (left-span (span-point left-bound common RIGHT))
          (right-span (span-point right-bound common LEFT))
          (span-length (- right-span left-span))
          (usable-length
           (- span-length
              (if (zero? (ly:item-break-dir left-bound)) 0.4 0)
              (if (zero? (ly:item-break-dir right-bound)) 0.4 0)
              left-padding
              right-padding))
          (base-markup
           #{
             \markup {
               \overlay {
                 \solmisasi \not-angka #(ly:make-pitch 1 5 0) #0
                 \solmisasi \not-angka #(ly:make-pitch -1 5 0) #0
               }
             }
           #})
          (style (ly:grob-property grob 'style 'dashed-line))
          (note-height
           (* 2.4
              (interval-length
               (ly:stencil-extent (grob-interpret-markup grob base-markup) Y))))
          (bottom-y 1.2)
          (top-y (- note-height bottom-y))
          (middle-y (* 0.5 (+ bottom-y top-y)))
          (inc-y -0.6)
          (inc-x 0.0)
          (left-x (+ 1.7 left-padding))
          (pscmd (format "~a setlinewidth 1 setlinecap ~a ~a ~a moveto ~a ~a lineto ~a ~a moveto ~a ~a lineto stroke"
                         0.15
                         (if (equal? style 'dashed-line) "[0.5 0.5] 0 setdash" "")
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

solmisasiVoiceOne = {
  \shiftOff
  \temporary\override Beam.extra-offset = #'(0 . 1.0)
  \temporary\override DynamicLineSpanner.staff-padding = #4.1
  \temporary\override NoteHead.Y-offset = #1.7
  \temporary\override NoteHead.font-size = #-0.5
  \slurDown
  \stemUp
}

solmisasiVoiceTwo = {
  \shiftOff
  \temporary\override Beam.extra-offset = #'(0 . -0.6)
  \temporary\override NoteHead.Y-offset = #-2.8
  \temporary\override NoteHead.font-size = #-0.5
  \slurDown
  \stemUp
}

%% Redefine \voiceOne, \voiceTwo, and \oneVoice without interferring their functions

voiceOne = {
  #(context-spec-music (make-voice-props-set 0)  'Voice)
  \tag#'solmisasi \solmisasiVoiceOne
}

voiceTwo = {
  #(context-spec-music (make-voice-props-set 1)  'Voice)
  \tag#'solmisasi \solmisasiVoiceTwo
}

oneVoice = {
  \tag#'solmisasi \undo \solmisasiVoiceOne
  \tag#'solmisasi \undo \solmisasiVoiceTwo
  #(context-spec-music (make-voice-props-revert) 'Voice)
}

startCloseDivisiSpan = -\tag #'solmisasi \tweak stencil #close-divisi-stencil-vII _\startTextSpan
startOpenDivisiSpan = -\tag #'solmisasi \tweak stencil #open-divisi-stencil-vII _\startTextSpan
stopDivisiSpan = {
  \tag #'solmisasi <>\stopTextSpan
  \oneVoice
}
