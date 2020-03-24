---
layout: default
title: "Combining Both Notation Systems"
nav_order: 1
grand_parent: Examples of Usage
parent: Advanced Examples
---

# Combining Both Standard and _Solmisasi_ Notation in One Score
{: .no_toc .fs-7 }

From the previous examples, we already had standard and solmisasi notation version of the song "Tantum Ergo Sacramentum". Occasionally, for some specific reasons, you may want those music representations combined, or engraved altogether in one score. It can be achieved quite easily. I'll give you a shot, please check this out, guys!
{: .fs-5}

```
%%% tantum-ergo-haydn-combined.ly
\version "2.20.0"
\include "english.ly"
\include "solmisasi.ily"

#(set-default-paper-size "a4")
#(set-global-staff-size 15)

\paper {
  indent = 2.4\mm
  short-indent = 2.4\mm
  system-system-spacing.padding = #5
  score-markup-spacing.padding = #5
}

\header {
  title = "Tantum Ergo Sacramentum"
  poet = "Tommaso d'Aquino"
  composer = "F.J. Haydn (1732 - 1809)"
  enteredby = "Giorgio Vicario"
  maintainer = "Gorgio Vicario, Henri Yulianto (2020)"
  maintainerEmail = "katobleto@yahoo.com"
  lastupdated = "2008/2/4"
  copyright = "Public Domain"
  mutopiacomposer = "HaydnFJ"
  mutopiainstrument = "Voice (SATB)"
  mutopiastyle = "Classical"
  source = "Transcription"
  footer = "Mutopia-2008/02/19-1322"
  tagline = \markup {
    \override #'(box-padding . 1.0)
    \override #'(baseline-skip . 2.7)
    \box \center-column {
      \small {
        \line {
          Sheet music from \with-url "http://www.MutopiaProject.org"
          \concat { \tiny www. MutopiaProject \tiny .org }
          • \italic Free to download, with the \italic freedom to distribute,
          modify and perform.
        }
        \line {
          Typeset using \with-url "http://www.LilyPond.org"
          \concat { \tiny www. LilyPond \tiny .org }
          by \maintainer • Reference: \footer
        }
        \line {
          \teeny {
            This sheet music has been placed in the public domain by the typesetter,
            for details see: \with-url
            "http://creativecommons.org/licenses/publicdomain"
            http://creativecommons.org/licenses/publicdomain
          }
        }
      }
    }
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

global = {
  \tempo "Andante"
  \key c\major \time 3/4
}

soprani = {
  \relative c'' {
    \clef treble
    \repeat volta 2 {
      e2\mf e4\< | e8 ( f8 ) f4. ( g8 ) \! |
      a4 ( g4 ) \> f4 | f8 ( e8 ) e4 \! r4 |
      g2 g4 | a2 f4 | e2 d4 | e2 r4 |
      g2 g4 | g4 g2 | a2 a4 | a8 ( g8 ) g2\fermata | \break
      g2 g4 | g2 a4 | g2 fs4 | g2 r4 |
      e2 -\tweak extra-offset #'(0 . -1.0)\pp e4 | e8 ( f8 ) f4. ( g8 ) | a4 ( g4 ) f4 | f8 ( e8 ) e4 r4 |
      g2 -\tweak extra-offset #'(-1 . -1.0)\f g4 | a2 f4 | e2 -\tweak extra-offset #'(0 . -1.0)\pp d4 | e2 r4
    }
    a2. | g2.
    \bar "|."
  }
}

contralti = {
  \relative c'' {
    \clef treble
    \repeat volta 2 {
      c2 -\tweak extra-offset #'(0 . -1.2)\mf c4 -\tweak extra-offset #'(0 . -1.2)\< | c4 c2 <>\! |
      c2 -\tweak extra-offset #'(0 . -1.2)\> c4 | c4 c4 \! r4 |
      e4 ( d4 ) c4 | c4 ( f4 ) d4 | c2 b4 | c2 r4 |
      e2 e4 | e8 ( d8 ) d2 | e2 e4 | e4 d2\fermata |
      d2 d4 | e4 ( d4 ) e4 | d2 c4 | b2 r4 |
      c2 -\tweak extra-offset #'(0 . -1.0)\pp c4 | c4 c2 | c2 c4 | c4 c4 r4 |
      e4 -\tweak extra-offset #'(-1 . -1.0)\f ( d4 ) c4 | c4 ( f4 ) d4 | c2 -\tweak extra-offset #'(0 . -1.0)-\tweak extra-offset #'(0 . -1.0)\pp b4 | c2 r4
    }
    f2. | e2.
    \bar "|."
  }
}

tenori = {
  \relative c' {
    \clef "treble_8"
    \repeat volta 2 {
      g2 -\tweak extra-offset #'(0 . -1.2)\mf g4 -\tweak extra-offset #'(0 . -1.2)\< | g8 ( a8 ) a4. ( b8 ) \! |
      c4 -\tweak extra-offset #'(0 . -1.2)\> ( b4 ) a4 | a8 ( g8 ) g4 \! r4 |
      c4 ( b4 ) c4 | c2 a4 | g2 g4 | g2 r4 |
      c2 c4 | c8 ( b8 ) b2 | c2 c4 | c8 ( b8 ) b2\fermata |
      b2 b4 | c4 ( d4 ) c4 | b2 a4 | g2 r4 |
      g2 -\tweak extra-offset #'(0 . -1.0)\pp g4 | g8 ( a8 ) a4. ( b8 ) |
      c4 ( b4 ) a4 | a8 ( g8 ) g4 r4 |
      c4 -\tweak extra-offset #'(-1 . -1.0)\f ( b4 ) c4 | c2 a4 | g2 -\tweak extra-offset #'(0 . -1.0)\pp g4 | g2 r4
    }
    c2. | c2.
    \bar "|."
  }
}

bassi = {
  \relative c {
    \clef bass
    \repeat volta 2 {
      c2 -\tweak extra-offset #'(0 . -1.0)\mf c4 -\tweak extra-offset #'(0 . -1.0)\< | c4 c2 <>\! |
      c2 -\tweak extra-offset #'(0 . -1.0)\> c4 | c4 c4 \! r4 |
      c4 ( d4 ) e4 | f2 f4 | g2 g,4 | c2 r4 |
      c4 ( e4 ) c4 | g'4 g2 | fs4 ( d4 ) fs4 | g4 g2\fermata |
      g2 g4 | c,4 ( b4 ) c4 | d2 d4 | g2 r4 |
      c,2 -\tweak extra-offset #'(0 . -1.0)\pp c4 | c4 c2 |
      c2 c4 | c4 c4 r4 |
      c4 -\tweak extra-offset #'(-1 . -1.0)\f ( d4 ) e4 | f2 f4 | g2 -\tweak extra-offset #'(0 . -1.0)\pp g,4 | c2 r4
    }
    f2. | c2.
    \bar "|."
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strofaa = \lyricmode {
  Tan -- tum er -- go sa -- cra -- men -- tum
  ve -- ne -- re -- mur cer -- nu -- i
  et an -- tiqu -- um do -- cu -- men -- tum
  no -- vo ce -- dat ri -- tu -- i.
  Prae -- stet fi -- des sup -- ple -- men -- tum
  sen -- su -- um de -- fec -- tu -- i.
}



strofab = \lyricmode {
  Ge -- ni -- to -- ri ge -- ni -- to -- que
  la -- us_et ju -- bi -- la -- ti -- o
  sa -- lus, ho -- nor, vir -- tus quo -- que
  sit et be -- ne -- di -- cti -- o.
  Pro -- ce -- den -- ti ab u -- tro -- que
  com -- par sit lau -- da -- ti -- o.
  A -- men
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\score {
  <<
    \new StaffGroup = "choir" {
      <<
        \new SolmisasiTimeAndKeySignature {
          \solmisasiMusic \global
        }
        %% BEGIN: Soprano
        \new Staff = "soprani" {
          \new Voice = "vSop" { \global \soprani }
        }
        \new SolmisasiStaff = "soprani_solmisasi" {
          \new SolmisasiVoice = "vSop_solmisasi" {
            \solmisasiMusic { \global \soprani }
          }
        }
        \new Lyrics \lyricsto "vSop" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \lyricsto "vSop" {
          \set stanza = "2."
          \strofab
        }
        %% END: Soprano

        %% BEGIN: Alto
        \new Staff = "contralti" {
          \new Voice = "vAlt" { \global \contralti }
        }
        \new SolmisasiStaff = "contralti_solmisasi" {
          \new SolmisasiVoice = "vAlt_solmisasi" {
            \solmisasiMusic { \global \contralti }
          }
        }
        \new Lyrics \lyricsto "vAlt" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \lyricsto "vAlt" {
          \set stanza = "2."
          \strofab
        }
        %% END: Alto

        %% BEGIN: Tenor
        \new Staff = "tenori" {
          \new Voice = "vTen" { \global \tenori }
        }
        \new SolmisasiStaff = "tenori_solmisasi" \with {
          male-vocal = ##t
        } {
          \new SolmisasiVoice = "vTen_solmisasi" {
            \solmisasiMusic { \global \tenori }
          }
        }
        \new Lyrics \lyricsto "vTen" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \lyricsto "vTen" {
          \set stanza = "2."
          \strofab
        }
        %% END: Tenor

        %% BEGIN: Bass
        \new Staff = "bassi" {
          \new Voice = "vBas" { \global \bassi }
        }
        \new SolmisasiStaff = "bassi_solmisasi" \with {
          male-vocal = ##t
        } {
          \new SolmisasiVoice = "vBas_solmisasi" {
            \solmisasiMusic { \global \bassi }
          }
        }
        \new Lyrics \lyricsto "vBas" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \lyricsto "vBas" {
          \set stanza = "2."
          \strofab
        }
        %% END: Bass
      >>
    }
  >>
  \layout {
    \context {
      \Score
      \consists "Metronome_mark_engraver"
      \override NonMusicalPaperColumn.line-break-permission = ##f
    }
    \context {
      \Staff
      % Allow the span-bar below this context
      \override BarLine.allow-span-bar = ##t
      \dynamicUp
      \RemoveEmptyStaves
    }
    \context {
      \SolmisasiStaff
      % Do not allow the span-bar below this context
      \override BarLine.allow-span-bar = ##f
      % Override staff-affinity to up
      \override VerticalAxisGroup.staff-affinity = #UP
      \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #-0.5
      % Omit everything that will cause redundancy
      \omit DynamicText
      \omit DynamicTextSpanner
      \omit Hairpin
      \omit Script
      \RemoveEmptyStaves
    }
    \context {
      \SolmisasiVoice
      \override Beam.extra-offset = #'(0 . -0.25)
    }
    \context {
      \SolmisasiTimeAndKeySignature
      \omit TimeSignature
    }
    \context {
      \Lyrics
      \override VerticalAxisGroup.staff-affinity = #CENTER
    }
  }
}

\markup {
  \left-column {
    \line { \draw-hline }
    \line {
      Un così grande sacramento veneriamo, dunque, chini
      e il vecchio rito trovi compimento nel nuovo.
    }
    \line {
      Presti la fede supplemento all'insufficienza dei sensi.
    }
    \line {
      Al Padre e al Figlio sia lode e giubilo,
      acclamazione, onore, virtù e benedizione.
    }
    \line {
      A Colui che procede da entrambi, sia rivolta pari lode.
    }
    \line { Amen. }
  }
}
```
{: .lilypond }
![Tantum Ergo Sacramentum](../tantum-ergo-haydn-combined.png)

Compared to the previous two examples of the same piece, some changes have been made here, e.g.:
{: .fs-5 }
- There is no need to use `NullVoice` contexts, as we already have the 'original' non-converted music definition from the standard notation staves.
- I decided to engrave `Lyrics` for each vocal instrument. This way, each instrument could be 'separated' one to another.
- And of course, there are Some simple tweaks added.
