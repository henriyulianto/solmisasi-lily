%% solmisasi-layout-definition.ily
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

#(if (not (defined? 'SOLMISASI_SCRIPTALIST_LOADED))
     #{ \include "solmisasi-script-alist.ily" #})

#(if (not (defined? 'SOLMISASI_ENGRAVER_LOADED))
     #{ \include "solmisasi-engraver.ily" #})

%% Variables
lyricTextLayerNum = #-3
lyricHyphenLayerNum = #-4
lyricExtenderLayerNum = #-4
spanBarLayerNum = #-5
startBarLayerNum = #-5

#(define (get-staff-symbol grob)
   "Return the staff symbol corresponding to Grob @var{grob}."
   (if (grob::has-interface grob 'staff-symbol-interface)
       grob
       (ly:grob-object grob 'staff-symbol)))

#(define (staff-symbol-line-count staff)
   "Get or compute the number of lines of staff @var{staff}."
   (let ((line-count 0))
     (if (ly:grob? staff)
         (let ((line-pos (ly:grob-property staff 'line-positions '())))
           (set! line-count (if (pair? line-pos)
                                (length line-pos)
                                (ly:grob-property staff 'line-count 0)))))
     line-count))

#(define (staff-symbol-transparent staff)
   (let ((transparent #f))
     (if (ly:grob? staff)
         (set! transparent (ly:grob-property staff 'transparent)))
     transparent))

#(define ((make-custom-dot-bar-line dot-positions) grob extent)
   "Draw dots (repeat sign dots) at @var{dot-positions}. The 
coordinates of @var{dot-positions} are equivalent to the 
coordinates of @code{StaffSymbol.line-positions}, a dot-position 
of X and a line-position of X indicate the same vertical position."
   (let* ((staff-space (ly:staff-symbol-staff-space grob))
          (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
          (transparent (staff-symbol-transparent (get-staff-symbol grob)))
          (dy 0)
          (stencil empty-stencil))
     (for-each
      (lambda (dp)
        (if (equal? #t transparent) (set! dy -0.35))
        (set! stencil (ly:stencil-add stencil
                                      (ly:stencil-translate-axis dot
                                                                 (+ (* dp (/ staff-space 2)) dy)
                                                                 Y))))
      dot-positions)
     stencil))

% #(add-bar-glyph-print-procedure ":" (make-custom-dot-bar-line '(-1 1)))

#(define-public (my-lyric-text::print grob)
   (let ((text (ly:grob-property grob 'text))
         (textStl
          (if (string? text)
              (make-tied-lyric-markup text)
              text))
         (stl (stencil-whiteout-box (ly:text-interface::print grob) 1.5)))
     stl))

%% copied from define-context-properties.scm
#(define (translator-property-description symbol type? description)
   (if (not (and
             (symbol? symbol)
             (procedure? type?)
             (string? description)))
       (throw 'init-format-error))
   (if (not (equal? #f (object-property symbol 'translation-doc)))
       (ly:error ("symbol ~S redefined") symbol))
   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc description)
   (set! all-translation-properties (cons symbol all-translation-properties))
   ; return
   symbol)

#(define-public solmisasi-translation-properties
   (map
    (lambda (x)
      (apply translator-property-description x))
    `(
       (savedInstrumentName ,markup? "Original instrument name.")
       (male-vocal ,boolean? "Is this context for male vocals?")
       (transposed-up ,boolean? "Is this context transposed up one octave?"))))

#(define-public all-translation-properties
   (append
    all-user-translation-properties
    all-internal-translation-properties
    solmisasi-translation-properties))

forceShowBracket = \override Score.SystemStartBracket.collapse-height = #4

solmisasiStaffContextMods = \with {
  \remove "Ledger_line_engraver"
  \consists #Solmisasi_equivalence_key_engraver

  \omit Accidental
  \omit Clef
  \omit ClefModifier
  \omit TimeSignature
  \omit Flag
  \omit KeyCancellation

  %% Inisialisasi property male-vocal
  male-vocal = ##f
  transposed-up = ##f

  explicitKeySignatureVisibility = #begin-of-line-invisible

  \override Stem.thickness = #14
  \override Stem.X-offset = #0.65
  \override Stem.length = #2
  \override Stem.length-fraction = #0.5
  \override Stem.color = #blue
  \override Stem.direction = #UP
  \override Stem.transparent = ##t
  \override NoteHead.Y-offset = #-0.65
  \override NoteHead.layer = #90
  \override Rest.Y-offset = #-0.65
  \override Tie.details.height-limit = #1.3
  %\override Slur.details.height-limit = #1.3
  \override TextScript.direction = #UP
  \override TextSpanner.direction = #UP
  \slurDown
  \tupletUp

  \override StaffSymbol.line-count = #5
  \override StaffSymbol.transparent = ##t
  \override BarLine.bar-extent = #'(-2 . 2)
  \override BarLine.gap = #0.8
  \override BarLine.space-alist.next-note = #'(semi-fixed-space . 1.5)
  \override InstrumentName.extra-offset = #'(0 . -0.35)
  \override Beam.transparent = ##f
  \override Beam.beam-thickness = #0.15
  \override Beam.length-fraction = #0.5
  \override Beam.extra-offset = #'(0 . 0.75)
  \override Beam.layer = #100
  \override TupletBracket.bracket-visibility = ##t
  \override Dots.staff-position = #2
  \override VerticalAxisGroup.default-staff-staff-spacing =
  #'((basic-distance . 0)
     (padding . 0.5))

  \shapeII #'((0.05 0.25)(0 0.3)(0 0.3)(-0.05 0.25)) Slur

  \accepts SolmisasiVoice
}

solmisasiVoiceContextMods = \with {
  \consists Pitch_squash_engraver
  \consists #Solmisasi_note_heads_engraver
  \consists #Solmisasi_rest_engraver

  squashedPosition = #0

  \override DynamicLineSpanner.staff-padding = #1.75
  \override DynamicLineSpanner.Y-extent = #'(1.5 . -1.5)
  \override DynamicText.extra-offset = #'(0 . -0.5)
  \override Hairpin.extra-offset = #'(0 . -0.5)
  %\override DynamicText.Y-offset = #-1.0
  \override Hairpin.whiteout = #1.5
  \override TextScript.Y-extent = #'(1.5 . -1.5)
  \override TupletBracket.shorten-pair = #'(0 . 0)
  \override TupletNumber.font-size = #0
  \override Glissando.bound-details.left.Y = #-1.2
  \override Glissando.bound-details.right.Y = #0.8
  \override Glissando.bound-details.left.padding = #0.2
  \override Glissando.bound-details.right.padding = #0.2
  \override TieColumn.tie-configuration = #'((-2.65 . -1))
  \override BreathingSign.text = \markup {
    \translate #'(0 . -1.5)
    \musicglyph #"comma"
  }
  \override Rest.staff-position = #0
  %\override Rest.Y-offset = #-0.65
  %\override Rest.font-size = #0.5
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

\layout {

  \context {
    \Score

    \consists \DbBars
    \consists "Span_arpeggio_engraver"

    \override SpanBar.layer= #-5
    \override BarNumber.font-size = #-0.5
    \override BarNumber.padding = #1
    \override BarNumber.font-shape = #'italic
    \override BarNumber.after-line-breaking = ##f
    \override BarNumber.extra-offset = #'(-0.3 . -0.1)
    \override VoltaBracket.font-size = #-1.5
    \override SystemStartBar.collapse-height = #4
    \override SystemStartBar.thickness = #1.9 % sama dengan BarLine.hair-thickness
    \override TimeSignature.style = #'numbered
    \override TextSpanner.dash-fraction = #0.35
    \override TextSpanner.dash-period = #2
    majorSevenSymbol = \markup {
      \fontsize #3 {
        \lower #1 {
          "M" \super "7"
        }
      }
    }
    barNumberVisibility = #all-bar-numbers-visible
    tieWaitForNote = ##t
    noChordSymbol = \markup\with-dimensions-from\null \smaller { (tacet) }
    % scriptDefinitions = #solmisasi-script-alist
    \forceShowBracket
  }

  \context {
    \Lyrics
    \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = #2.25
  }

  \context {
    \Lyrics
    \name "SolmisasiLyrics"
    \alias Lyrics

    \consists "Bar_engraver"
    \consists "Separating_line_group_engraver"

    \override BarLine.bar-extent = #'(0 . 1.5)
    \override BarLine.transparent = ##t
    \override BarLine.gap = #1.0
    \override LyricText.layer = #-2
    \override LyricText.whiteout = #2
    \override LyricText.whiteout-style = #'outline
    \override LyricText.word-space = #1
    \override LyricExtender.layer = -4
    \override LyricHyphen.layer = #-3
    \override LyricHyphen.minimum-distance = #0.4
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.75
    \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = #2.5
  }

  \context {
    \Voice

    \override DynamicLineSpanner.staff-padding = #3.0
    \override DynamicLineSpanner.Y-extent = #'(2.5 . -2.5)
    \override TextScript.Y-extent = #'(2.5 . -2.5)
    \override TupletNumber.font-size = #-1
    \override Script.direction = #UP

    \dynamicUp
  }

  \context {
    \Voice
    \name "SolmisasiVoice"
    \alias Voice

    \with \solmisasiVoiceContextMods
  }

  \context {
    \PianoStaff
    \override InstrumentName.self-alignment-X = #RIGHT
    \override InstrumentName.padding = #0.8
  }

  \context {
    \Staff
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
    \override InstrumentName.self-alignment-X = #RIGHT
    \override InstrumentName.padding = #0.8
  }

  \context {
    \Staff
    \name SolmisasiStaff
    \alias Staff

    \with \solmisasiStaffContextMods
  }

  \context {
    \type "Engraver_group"
    \name "GlobalTempo"

    \consists "Bar_engraver"
    \consists "Grace_spacing_engraver"
    \consists "Time_signature_engraver"
    \consists "Text_spanner_engraver"
    \consists "Text_engraver"
    \consists "Axis_group_engraver"

    keepAliveInterfaces =
    #'( metronome-mark-interface
        text-script-interface
        line-spanner-interface
        ;time-signature-interface
        )

    \override BarLine.transparent = ##t
    \override BarLine.bar-extent = #'(-0.5 . 0.5)
    \override TimeSignature.stencil = ##f
    \override TimeSignature.break-align-symbol = ##f
    \override MetronomeMark.Y-offset = #(/ 12 11)
    \override MetronomeMark.break-align-symbols = ##f
    \override MetronomeMark.direction = #UP
    \override TextSpanner.direction = #UP
    \override TextSpanner.Y-offset = #0
    \override VerticalAxisGroup.staff-affinity = #DOWN
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
    \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##f
    \override MetronomeMark.extra-spacing-width = #'(+inf.0 . -inf.0)
    \revert MetronomeMark.extra-spacing-height
  }

  \context {
    \type "Engraver_group"
    \name "SolmisasiTimeAndKeySignature"

    \consists "Bar_engraver"
    \consists "Pure_from_neighbor_engraver"
    \consists "Font_size_engraver"
    \consists "Separating_line_group_engraver"
    \consists "Axis_group_engraver"
    \consists "Break_align_engraver"
    \consists "Time_signature_engraver"
    \consists "Key_engraver"
    \consists Text_spanner_engraver
    \consists #Solmisasi_time_signature_engraver
    \consists #Solmisasi_key_engraver

    \omit NoteHead
    \omit Stem
    \omit Clef
    \omit ClefModifier
    \omit KeyCancellation

    explicitKeySignatureVisibility = #end-of-line-invisible
    keepAliveInterfaces =
    #'( key-signature-interface
        time-signature-interface
        )

    \override BarLine.transparent = ##t
    \override BarLine.bar-extent = #'(-0.5 . 0.5)
    \override TimeSignature.self-alignment-X = #CENTER
    \override KeySignature.self-alignment-X = #CENTER
    \override TimeSignature.break-visibility = #end-of-line-invisible
    \override VerticalAxisGroup.staff-affinity = #DOWN
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
    \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##f
  }

  \context {
    \ChoirStaff
  }

  \context {
    \ChoirStaff
    \name "SolmisasiChoirStaff"

    \override StaffGrouper.staff-staff-spacing =
    #`((basic-distance . 0.5)
       (minimum-distance . 0.25)
       (padding . 0.75))

    \denies Staff
    \accepts SolmisasiStaff
    \accepts SolmisasiVoice
    \accepts SolmisasiLyrics
  }

  \context {
    \Score
    \accepts GlobalTempo
    \accepts SolmisasiChoirStaff
    \accepts SolmisasiStaff
    \accepts SolmisasiTimeAndKeySignature
    \accepts SolmisasiLyrics
    \accepts SolmisasiVoice
  }

  \context {
    \ChoirStaff
    \accepts GlobalTempo
    \accepts SolmisasiStaff
  }

  \context {
    \GrandStaff
    \accepts SolmisasiChoirStaff
    \accepts SolmisasiStaff
    \accepts SolmisasiTimeAndKeySignature
    \accepts SolmisasiVoice
    \accepts SolmisasiLyrics
  }

  \context {
    \PianoStaff
    \accepts SolmisasiStaff
    \accepts SolmisasiTimeAndKeySignature
    \accepts SolmisasiVoice
    \accepts SolmisasiLyrics
  }

  \context {
    \StaffGroup
    \accepts SolmisasiStaff
    \accepts SolmisasiTimeAndKeySignature
    \accepts SolmisasiVoice
    \accepts SolmisasiLyrics
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define SOLMISASI_LAYOUT_DEFINITION_LOADED #t)
#(ly:message "* Solmisasi layout module has been loaded.")
