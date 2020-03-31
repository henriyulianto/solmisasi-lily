\version "2.20.0"
\include "solmisasi.ily"

\header {
  title = "Vocal Excersice Example"
  subtitle = "Major Scales"
  composer = "Not Me"
  tagline = ##f
}

#(set-default-paper-size "a4")

\paper {
  indent = 0\mm
  system-system-spacing.padding = #4
  ragged-last = ##t
  markup-system-spacing.padding = #4
}

music = {
  \time 4/4
  \key a \major
  a4 b cis' d' |
  e' fis' gis' a' |
  \bar "||"
  \key d \major
  a' g' fis' e' |
  d' cis' b a~ |
  \time 2/4
  2 |
  \break

  \time 4/4
  \bar "||"
  \key bes \major
  bes4 c' d' es' |
  f' g' a' bes' |
  \bar "||"
  \key es \major
  bes' as' g' f' |
  es' d' c' bes~ |
  \time 2/4
  2 |
  \break

  \time 4/4
  \set Staff.male-vocal = ##t
  \bar "||"
  \key b \major
  b4 cis' dis' e' |
  fis' gis' ais' b' |
  \set Staff.male-vocal = ##f
  \bar "||"
  \key e \major
  b' a' gis' fis' |
  e' dis' cis' b~ |
  \time 3/4
  2 r4 |
  \break

  \time 6/8
  \bar "||"
  \key c \major
  c'8 d' e' d' e' f' |
  g' a' b' a' b' c'' |
  \bar "||"
  \key f \major
  c'' bes' a' bes' a' g' |
  f' e' d' e' d' c'~ |
  \time 3/4
  2 r4 |
  \break

  \time 6/8
  \bar "||"
  \key des \major
  des'8 es' f' es' f' ges' |
  as' bes' c'' bes' c'' des'' |
  \bar "||"
  \key ges \major
  des'' ces'' bes' ces'' bes' as' |
  ges' f' es' f' es' des'~ |
  \time 3/4
  2 r4 |
  \break

  \time 6/8
  \bar "||"
  \key d \major
  d'8 e' fis' e' fis' g' |
  a' b' cis'' b' cis'' d'' |
  \bar "||"
  \key g \major
  d'' c'' b' c'' b' a' |
  g' fis' e' fis' e' d'~ |
  \time 3/4
  2 r4 |
  \break

  \time 6/8
  \bar "||"
  \key es \major
  es'8 f' g' f' g' as' |
  bes' c'' d'' c'' d'' es'' |
  \set Staff.male-vocal = ##t
  \bar "||"
  \key as \major
  es'' des'' c'' des'' c'' bes' |
  as' g' f' g' f' es'~ |
  \time 3/4
  2 r4 |
  \bar "|."

}

lyric = \lyricmode {
  ma ma ma ma ma ma ma ma
  ma ma ma ma ma ma ma moo __
  ma ma ma ma ma ma ma ma
  ma ma ma ma ma ma ma moo __
  ma ma ma ma ma ma ma ma
  ma ma ma ma ma ma ma moo __
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma moo __
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma moo __
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma moo __
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma ma
  ma ma ma ma ma moo __
}

\score {
  <<
    \new SolmisasiTimeAndKeySignature {
      \solmisasiMusic \silence \music
    }
    \new SolmisasiStaff {
      \new SolmisasiVoice {
        \solmisasiMusic \music
      }
      \addlyrics {
        \solmisasiLyric \lyric
      }
    }
  >>
  \layout {
    \context {
      \Score
      \remove \DbBars
      \override NonMusicalPaperColumn.line-break-permission = ##f
    }
    \context {
      \SolmisasiTimeAndKeySignature
      \omit TimeSignature
    }
    \context {
      \SolmisasiStaff
      \revert TimeSignature.stencil
      \override TimeSignature.font-size = #-1
      \override TimeSignature.break-visibility = #end-of-line-invisible
    }
    \context {
      \Lyrics
      \override LyricText.font-family = #'sans
      \override LyricText.font-size = #-0.5
    }
  }
}