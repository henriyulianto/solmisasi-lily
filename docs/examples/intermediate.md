---
layout: default
title: "Intermediate: A Choral Music Example"
nav_order: 2
parent: Examples of Usage
---

# Engraving a Simple Choral Song: "Tantum Ergo Sacramentum" for Voice (SATB)
{: .no_toc .fs-7 }

In this section, we're going to try to engrave "Tantum Ergo Sacramentum", composed by Franz Joseph Haydn, for Voice (SATB). The original source was taken from [Mutopia Project](https://www.mutopiaproject.org).<br>
{: .fs-5 }
Original .ly file: [tantum_ergo.ly](https://www.mutopiaproject.org/ftp/HaydnFJ/tantum_ergo/tantum_ergo.ly)<br>
Original A4 .pdf file: [tantum_ergo-a4.pdf](https://www.mutopiaproject.org/ftp/HaydnFJ/tantum_ergo/tantum_ergo-a4.pdf)
{: .fs-4 }

**NOTES**
{: .label .label-green }
> In these examples, I omitted all instrument names, just like the original score obtained from Mutopia Project. And to better engrave the solmisasi notation of this piece, some stuffs were added into the score, such as:
> - a `NullVoice` context into the staff of Soprano, and another `NullVoice` context into the staff of Tenor, to be used as the associated voices for their corresponding `Lyrics`.
> - some simple tweaks.

---

## Standard Notation (with slight modifications)
{: .no_toc }

```
%%% tantum-ergo-haydn.ly
\version "2.20.0"
\include "english.ly"

#(set-default-paper-size "a4")

\paper {
  indent = 2.4\mm
  short-indent = 2.4\mm
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
      e2 \pp e4 | e8 ( f8 ) f4. ( g8 ) | a4 ( g4 ) f4 | f8 ( e8 ) e4 r4 |
      g2 \f g4 | a2 f4 | e2 \pp d4 | e2 r4
    }
    a2. | g2.
    \bar "|."
  }
}

contralti = {
  \relative c'' {
    \clef treble
    \repeat volta 2 {
      c2\mf c4\< | c4 c2 <>\! |
      c2 \> c4 | c4 c4 \! r4 |
      e4 ( d4 ) c4 | c4 ( f4 ) d4 | c2 b4 | c2 r4 |
      e2 e4 | e8 ( d8 ) d2 | e2 e4 | e4 d2\fermata |
      d2 d4 | e4 ( d4 ) e4 | d2 c4 | b2 r4 |
      c2 \pp c4 | c4 c2 | c2 c4 | c4 c4 r4 |
      e4 \f ( d4 ) c4 | c4 ( f4 ) d4 | c2 \pp b4 | c2 r4
    }
    f2. | e2.
    \bar "|."
  }
}

tenori = {
  \relative c' {
    \clef "treble_8"
    \repeat volta 2 {
      g2\mf g4\< | g8 ( a8 ) a4. ( b8 ) \! |
      c4 \> ( b4 ) a4 | a8 ( g8 ) g4 \! r4 |
      c4 ( b4 ) c4 | c2 a4 | g2 g4 | g2 r4 |
      c2 c4 | c8 ( b8 ) b2 | c2 c4 | c8 ( b8 ) b2\fermata |
      b2 b4 | c4 ( d4 ) c4 | b2 a4 | g2 r4 |
      g2\pp g4 | g8 ( a8 ) a4. ( b8 ) |
      c4 ( b4 ) a4 | a8 ( g8 ) g4 r4 |
      c4 \f ( b4 ) c4 | c2 a4 | g2 \pp g4 | g2 r4
    }
    c2. | c2.
    \bar "|."
  }
}

bassi = {
  \relative c {
    \clef bass
    \repeat volta 2 {
      c2\mf c4\< | c4 c2 <>\! |
      c2 \> c4 | c4 c4 \! r4 |
      c4 d4 e4 | f2 f4 | g2 g,4 | c2 r4 |
      c4 e4 c4 | g'4 g2 | fs4 d4 fs4 | g4 g2\fermata |
      g2 g4 | c,4 ( b4 ) c4 | d2 d4 | g2 r4 |
      c,2 \pp c4 | c4 c2 |
      c2 c4 | c4 c4 r4 |
      c4 \f d4 e4 | f2 f4 | g2 \pp g,4 | c2 r4
    }
    f2. | c2.
    \bar "|."
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strofaa = {
  \lyricmode {
    Tan -- tum er -- go sa -- cra -- men -- tum
    ve -- ne -- re -- mur cer -- nu -- i
    et an -- tiqu -- um do -- cu -- men -- tum
    no -- vo ce -- dat ri -- tu -- i.
    Prae -- stet fi -- des sup -- ple -- men -- tum
    sen -- su -- um de -- fec -- tu -- i.
  }
}



strofab = {
  \lyricmode {
    Ge -- ni -- to -- ri ge -- ni -- to -- que
    la -- us_et ju -- bi -- la -- ti -- o
    sa -- lus, ho -- nor, vir -- tus quo -- que
    sit et be -- ne -- di -- cti -- o.
    Pro -- ce -- den -- ti ab u -- tro -- que
    com -- par sit lau -- da -- ti -- o.
    A -- men

  }

}

\score {
  <<
    \new ChoirStaff = "choir" {
      <<
        \new Staff = "soprani" {
          \new Voice = "vSop" { \global \soprani }
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "vSop" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "vSop" {
          \set stanza = "2."
          \strofab
        }
        \new Staff = "contralti" {
          \new Voice = "vAlt" { \global \contralti }
        }
        \new Staff = "tenori" {
          \new Voice = "vTen" { \global \tenori }
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "vTen" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "vTen" {
          \set stanza = "2."
          \strofab
        }
        \new Staff = "bassi" {
          \new Voice = "vBas" { \global \bassi }
        }
      >>
    }
  >>
  \layout {
    \context {
      \Score
      \override NonMusicalPaperColumn.line-break-permission = ##f
    }
    \context {
      \Staff
      \dynamicUp
      \RemoveEmptyStaves
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
{: .lilypond}

![Tantum Ergo Sacramentum - Standard Notation](../tantum-ergo-haydn.png)

## Solmisasi Notation
{: .no_toc }

```
%%% tantum-ergo-haydn-solmisasi.ly
\version "2.20.0"
\include "english.ly"
\include "solmisasi.ily"

#(set-default-paper-size "a4")

\paper {
  indent = 2.4\mm
  short-indent = 2.4\mm
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
      e2 -\tweak extra-offset #'(0 . -0.6)\mf e4\< | e8 ( f8 ) f4. ( g8 ) \! |
      a4 ( g4 ) \> f4 | f8 ( e8 ) e4 \! r4 |
      g2 g4 | a2 f4 | e2 d4 | e2 r4 |
      g2 g4 | g4 g2 | a2 a4 | a8 ( g8 ) g2\fermata | \break
      g2 g4 | g2 a4 | g2 fs4 | g2 r4 |
      e2 -\tweak extra-offset #'(0 . -0.6)\pp e4 | e8 ( f8 ) f4. ( g8 ) | a4 ( g4 ) f4 | f8 ( e8 ) e4 r4 |
      g2 -\tweak extra-offset #'(0 . -0.8)\f g4 | a2 f4 | e2 -\tweak extra-offset #'(0 . -0.6)\pp d4 | e2 r4
    }
    a2. | g2.
    \bar "|."
  }
}

contralti = {
  \relative c'' {
    \clef treble
    \repeat volta 2 {
      c2 -\tweak extra-offset #'(0 . -0.6)\mf c4\< | c4 c2 <>\! |
      c2 \> c4 | c4 c4 \! r4 |
      e4 ( d4 ) c4 | c4 ( f4 ) d4 | c2 b4 | c2 r4 |
      e2 e4 | e8 ( d8 ) d2 | e2 e4 | e4 d2\fermata |
      d2 d4 | e4 ( d4 ) e4 | d2 c4 | b2 r4 |
      c2 -\tweak extra-offset #'(0 . -0.6)\pp c4 | c4 c2 | c2 c4 | c4 c4 r4 |
      e4 -\tweak extra-offset #'(0 . -0.8)\f ( d4 ) c4 | c4 ( f4 ) d4 | c2 -\tweak extra-offset #'(0 . -0.6)\pp b4 | c2 r4
    }
    f2. | e2.
    \bar "|."
  }
}

tenori = {
  \relative c' {
    \clef "treble_8"
    \repeat volta 2 {
      g2 -\tweak extra-offset #'(0 . -0.6)\mf g4\< | g8 ( a8 ) a4. ( b8 ) \! |
      c4 \> ( b4 ) a4 | a8 ( g8 ) g4 \! r4 |
      c4 ( b4 ) c4 | c2 a4 | g2 g4 | g2 r4 |
      c2 c4 | c8 ( b8 ) b2 | c2 c4 | c8 ( b8 ) b2\fermata |
      b2 b4 | c4 ( d4 ) c4 | b2 a4 | g2 r4 |
      g2 -\tweak extra-offset #'(0 . -0.6)\pp g4 | g8 ( a8 ) a4. ( b8 ) |
      c4 ( b4 ) a4 | a8 ( g8 ) g4 r4 |
      c4 -\tweak extra-offset #'(0 . -0.8)\f ( b4 ) c4 | c2 a4 | g2 -\tweak extra-offset #'(0 . -0.6)\pp g4 | g2 r4
    }
    c2. | c2.
    \bar "|."
  }
}

bassi = {
  \relative c {
    \clef bass
    \repeat volta 2 {
      c2 -\tweak extra-offset #'(0 . -0.6)\mf c4\< | c4 c2 <>\! |
      c2 \> c4 | c4 c4 \! r4 |
      c4 d4 e4 | f2 f4 | g2 g,4 | c2 r4 |
      c4 e4 c4 | g'4 g2 | fs4 d4 fs4 | g4 g2\fermata |
      g2 g4 | c,4 ( b4 ) c4 | d2 d4 | g2 r4 |
      c,2 -\tweak extra-offset #'(0 . -0.6)\pp c4 | c4 c2 |
      c2 c4 | c4 c4 r4 |
      c4 -\tweak extra-offset #'(0 . -0.8)\f d4 e4 | f2 f4 | g2 -\tweak extra-offset #'(0 . -0.6)\pp g,4 | c2 r4
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
    \new SolmisasiTimeAndKeySignature {
      \solmisasiMusic \global
    }
    \new ChoirStaff = "choir" {
      <<
        \new SolmisasiStaff = "soprani" {
          <<
            %<--- BEGIN A SIMULTANEOUS MUSIC
            \new SolmisasiVoice = "vSop" {
              \solmisasiMusic { \global \soprani }
            }
            %% NullVoice for this staff ------
            \new NullVoice = "nullSop" { \global \soprani }
            %%--------------------------------
          >> %<--- END A SIMULTANEOUS MUSIC
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "nullSop" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "nullSop" {
          \set stanza = "2."
          \strofab
        }
        \new SolmisasiStaff = "contralti" {
          \new SolmisasiVoice = "vAlt" {
            \solmisasiMusic { \global \contralti }
          }
        }
        \new SolmisasiStaff = "tenori" \with {
          male-vocal = ##t
        } {
          <<
            %<--- BEGIN A SIMULTANEOUS MUSIC
            \new SolmisasiVoice = "vTen" {
              \solmisasiMusic { \global \tenori }
            }
            %% NullVoice for this staff ------
            \new NullVoice = "nullTen" { \global \tenori }
            %%--------------------------------
          >> %<--- END A SIMULTANEOUS MUSIC
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "nullTen" {
          \set stanza = "1."
          \strofaa
        }
        \new Lyrics \with {
          \override VerticalAxisGroup.staff-affinity = #CENTER
        } \lyricsto "nullTen" {
          \set stanza = "2."
          \strofab
        }
        \new SolmisasiStaff = "bassi" \with {
          male-vocal = ##t
        } {
          \new SolmisasiVoice = "vBas" {
            \solmisasiMusic { \global \bassi }
          }
        }
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
      \SolmisasiStaff
      \undo\omit TimeSignature
      \override TimeSignature.font-size = #-1
      \dynamicUp
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
{: .lilypond}

![Tantum Ergo Sacramentum - Solmisasi Notation](../tantum-ergo-haydn-solmisasi.png)
