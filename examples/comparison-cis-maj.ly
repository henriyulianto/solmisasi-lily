\version "2.20.0"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example 1
%% Basic/simple usage of solmisasi-lily library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\include "solmisasi.ily"

\header {
  tagline = ##f
}

\paper {
  bookTitleMarkup = ##f
  paper-width = 210\mm
  paper-height = 180\mm
  top-margin = 5\mm
  right-margin = 5\mm
  bottom-margin = 5\mm
  left-margin = 5\mm
  #(define fonts
     (set-global-fonts
      #:music "emmentaler"
      #:brace "emmentaler"
      #:typewriter "Consolas"
      #:factor (/ staff-height pt 20)
      ))
  indent = 25\mm
  %short-indent = 0\mm
  ragged-right = ##t
}

\layout {
}

%%% Defining a custom strait brace with a variable length:
#(define-markup-command (long-curly-bracket layout props arg-height)
   (number?)
   "Draw a curly bracket with a variable length."
   (interpret-markup layout props
     (markup
      #:translate (cons 0 (* arg-height -1))
      (#:with-dimensions (cons -0.001 0) (cons 0.001 0)
        (#:override (cons (quote filled) #t)
          (#:path 0.01 `(
                          (moveto   -0.8   0.0)
                          (curveto  -0.2   0.4  -0.3  0.7 -0.3  1.5)
                          (lineto   -0.3   ,arg-height)
                          (curveto  -0.4   ,(+ arg-height 1.3) 0.5 ,(+ arg-height 2.8) 0.7 ,(+ arg-height 2.8))
                          (curveto   0.6   ,(+ arg-height 2.5) 0.1 ,(+ arg-height 2)   0.1 ,arg-height)
                          (lineto    0.1   1.5)
                          (curveto   0.1   0.7   0.1   0.0  -0.8   0.0)
                          (closepath)
                          (curveto  -0.2  -0.4  -0.3  -0.7  -0.3  -1.5)
                          (lineto   -0.3   ,(* arg-height -1))
                          (curveto  -0.4   ,(* (+ arg-height 1.3) -1) 0.5 ,(* (+ arg-height 2.8) -1) 0.7 ,(* (+ arg-height 2.8) -1))
                          (curveto   0.6   ,(* (+ arg-height 2.5) -1) 0.1 ,(* (+ arg-height 2) -1)   0.1 ,(* arg-height -1))
                          (lineto    0.1  -1.5)
                          (curveto   0.1  -0.7   0.1   0.0  -0.8   0.0)
                          (closepath))))))))

redColored = \override NoteHead.color = #red
resetColor = \revert NoteHead.color

bass_color			=	#'(1.0 0.0 0.0)
baritone_color	= #'(1.0 0.5 0.0)
tenor_color			= #'(1.0 0.0 0.5)
alto_color			= #'(0.0 1.0 1.0)
mezzosop_color	= #'(0.0 0.5 1.0)
sopran_color		= #'(0.0 0.0 1.0)

colorify_note =
#(define-music-function (clr) (color?)
   #{ \override NoteHead.color = #clr #})

bass_notes = \relative c, {
  \key cis \major
  \cadenzaOn
  \clef bass
  disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 \clef "treble_8" bis4 cis4 cisis4 dis4 disis4
}

baritone_notes = \relative c, {
  \key cis \major
  \cadenzaOn
  \clef bass
  fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 \clef "treble_8" bis4 cis4 cisis4 dis4 disis4 eis4 s2
}

tenor_notes = \relative bis, {
  \key cis \major
  \cadenzaOn
  \clef bass
  aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 \clef "treble_8" bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 s2
}

alto_notes = \relative bis, {
  \key cis \major
  \cadenzaOn
  \clef treble eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 s4
}

mezzosop_notes = \relative c' {
  \key cis \major
  \cadenzaOn
  \clef treble gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4
}

soprano_notes = \relative c' {
  \key cis \major
  \cadenzaOn
  \clef treble bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 cis4 cisis4 dis4 disis4 eis4 fis4 fisis4 gis4 gisis4 ais4 aisis4 bis4 \bar "|."
}

score_c_maj = \score {
  <<
    {
      %\omit Score.BarLine
      %\omit Score.SpanBar
      \override Score.SpanBar.allow-span-bar = ##f
      \omit Score.TimeSignature
    }
    \new StaffGroup \with {
      \override SpanBar.allow-span-bar = ##f
    } {
      <<
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-0.8
          instrumentName = "Soprano"
          shortInstrumentName = "S."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \soprano_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \soprano_notes
          }
        }
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-0.8
          instrumentName = \markup {
            \right-column {
              "Mezzo-"
              "Soprano"
            }
          }
          shortInstrumentName = "Mz."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \mezzosop_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \mezzosop_notes
          }
        }
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-0.8
          instrumentName = "Alto"
          shortInstrumentName = "A."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \alto_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \alto_notes
          }
        }
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
          instrumentName = "Tenor"
          shortInstrumentName = "T."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \tenor_notes
          }
        }
        \new SolmisasiStaff \with {
          instrumentName = \markup {
            \with-color #grey "(Actual)"
          }
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
        } {
          \new SolmisasiVoice {
            \override NoteHead.color = #grey
            \solmisasiMusic \tenor_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
          instrumentName = "(Real World)"
          male-vocal = ##t
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \tenor_notes
          }
        }
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
          instrumentName = "Baritone"
          shortInstrumentName = "Br."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \baritone_notes
          }
        }
        \new SolmisasiStaff \with {
          instrumentName = \markup {
            \with-color #grey "(Actual)"
          }
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
        } {
          \new SolmisasiVoice {
            \override NoteHead.color = #grey
            \solmisasiMusic \baritone_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
          instrumentName = "(Real World)"
          male-vocal = ##t
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \baritone_notes
          }
        }
        \new Staff \with {
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
          instrumentName = "Bass"
          shortInstrumentName = "B."
        } {
          \new Voice \with {
            \remove "Stem_engraver"
          } {
            \bass_notes
          }
        }
        \new SolmisasiStaff \with {
          instrumentName = \markup {
            \with-color #grey "(Actual)"
          }
          \override VerticalAxisGroup.staff-staff-spacing.padding = #-1
        } {
          \new SolmisasiVoice {
            \override NoteHead.color = #grey
            \solmisasiMusic \bass_notes
          }
        }
        \new SolmisasiStaff \with {
          \override BarLine.allow-span-bar = ##f
          instrumentName = "(Real World)"
          male-vocal = ##t
        } {
          \new SolmisasiVoice {
            \solmisasiMusic \bass_notes
          }
        }
      >>
    }
    % \new SolmisasiStaff {
    %       \new Voice {
    %         \solmisasiMusic \music_solmisasi
    %       }
    %     }
  >>
  \layout {
    \context {
      \Score
      \remove "Instrument_name_engraver"
      %\remove "System_start_delimiter_engraver"
      proportionalNotationDuration = #(ly:make-moment 1/4)
    }
    \context {
      \SolmisasiStaff
    }
  }
}

\score {
  \score_c_maj
}