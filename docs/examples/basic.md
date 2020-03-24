---
layout: default
title: "Basic: \"Mary Had a Little Lamb - in D Major and E Major\""
nav_order: 1
parent: Examples of Usage
---

# Engraving a Simple Song: "Mary Had a Little Lamb - in D Major and E Major" in _Solmisasi_
{: .no_toc .fs-7 }

Let's first engrave this song in standard (Western) music notation, without an `include` to the _solmisasi-lily_ library. The snippet would be something like this.
{: .fs-5 }

```
\version "2.20.0"

\paper {
  % some paper settings goes here...
}

\header {
  title = \markup { \sans "Mary Had a Little Lamb" }
  subtitle = "Excerpt — in D Major and E Major"
  composer = "Traditional"
  tagline = \markup {
    \typewriter \fontsize #-1 {
      \concat {
        "Engraved using GNU Lilypond "
        #(string-join (map (lambda (v) (number->string v)) (ly:version)) ".")
      }
    }
  }
}

mary_in_d_maj_notes = {
  \key d \major
  \relative d' {
    fis4. e8 d4 e |
    fis4 4 2 |
    e4 4 2 |
    fis4 a4 2 | \break
    fis4. e8 d4 e |
    fis4 4 4 4 |
    e e fis e |
    d2. r4
  }
}

mary_music = {
  \time 4/4
  \mary_in_d_maj_notes
  \bar "||"
  \transpose d e \mary_in_d_maj_notes
  \bar "|."
}

mary_lyric = \lyricmode {
  % Verse 1
  Ma -- ry had a lit -- tle lamb,
  lit -- tle lamb, lit -- tle lamb.
  Ma -- ry had a lit -- tle lamb whose
  fleece was white as snow.
  % Verse 2
  Ev -- ery -- where that Ma -- ry went,
  Ma -- ry went, Ma -- ry went.
  Ev -- ery -- where that Ma -- ry went
  the lamb was sure to go.
}

\score {
  <<
    \new Staff {
      \new Voice { \mary_music }
      \addlyrics { \mary_lyric }
    }
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText.font-family = #'sans
      \override LyricText.font-size = #0
      \override LyricHyphen.Y-offset = #0.2
      \override LyricHyphen.minimum-distance = #2.0
    }
  }
}
```
{: .lilypond }

And here is the compilation result.
{: .fs-5 }

![](../basic-01.png)

Now, let's transform that score into _solmisasi_ notation. Here is the snippet and its compiled result.
{: .fs-5 }

```
\version "2.20.0"
\include "solmisasi.ily"

\paper {
  % some paper settings goes here...
}

\header {
  title = \markup { \sans "Mary Had a Little Lamb" }
  subtitle = "Excerpt — in D Major and E Major"
  composer = "Traditional"
  tagline = \markup {
    \typewriter \fontsize #-1 {
      \concat {
        "Engraved using GNU Lilypond "
        #(string-join (map (lambda (v) (number->string v)) (ly:version)) ".")
        " — with solmisasi-lily v1.0.0"
      }
    }
  }
}

mary_in_d_maj_notes = {
  \key d \major
  \relative d' {
    fis4. e8 d4 e |
    fis4 4 2 |
    e4 4 2 |
    fis4 a4 2 | \break
    fis4. e8 d4 e |
    fis4 4 4 4 |
    e e fis e |
    d2. r4
  }
}

mary_music = {
  \time 4/4
  \mary_in_d_maj_notes
  \bar "||"
  \transpose d e \mary_in_d_maj_notes
  \bar "|."
}

mary_lyric = \lyricmode {
  % Verse 1
  Ma -- ry had a lit -- tle lamb,
  lit -- tle lamb, lit -- tle lamb.
  Ma -- ry had a lit -- tle lamb whose
  fleece was white as snow.
  % Verse 2
  Ev -- ery -- where that Ma -- ry went,
  Ma -- ry went, Ma -- ry went.
  Ev -- ery -- where that Ma -- ry went
  the lamb was sure to go.
}

\score {
  <<
    \new SolmisasiTimeAndKeySignature {
      \solmisasiMusic \mary_music
    }
    \new SolmisasiStaff {
      \new SolmisasiVoice {
        \solmisasiMusic \mary_music
      }
      \addlyrics {
        \solmisasiLyric \mary_lyric
      }
    }
  >>
  \layout {
    \context {
      \Lyrics
      \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.basic-distance = #0
      \override LyricText.font-family = #'sans
      \override LyricText.font-size = #0
      \override LyricHyphen.Y-offset = #0.2
      \override LyricHyphen.minimum-distance = #2.0
    }
  }
}
```
{: .lilypond }

![](../basic-01-solmisasi.png)

In the compiled result, there is a new object printed at the end of the second system, as shown by the following image.
{: .fs-5 }
<img src="../basic-01-solmisasi-key-equivalence.png" width="100"/>

This object was engraved by a special custom engraver named `Solmisasi_equivalence_key_engraver`, with the purpose of telling the score users/readers about the equivalence of the last note, before the change of the key signature, in the new key signature. It is a common practice in writing music in _solmisasi_ notation. However, this object could be omitted, if you want to.
{: .fs-5 }

If you carefully look at the latest snippet, you'll find a new music function named `\solmisasiLyric`. _**What is this, and why do I need this function?**_
{: .fs-5 }

In this version, _solmisasi-lily_ treats both input notes and rests as notes. In the case of a `rest-event` or `multi-measure-rest` found, it will be treated as a `note-event`. Therefore, Lilypond's `Lyric_engraver` will also print the lyric syllables below/above a _solmisasi_ rest (printed as `0`). So, I decided to code a simple function, named `\solmisasiLyric`, to overcome this problem. If I have time in the future, I will implement a dedicated `Rest_engraver` in the next version of _solmisasi-lily_ library.
{: .fs-5 }

However, there are other alternative ways, such as:<br>
{: .fs-5 }
1. inserting a `\skip` or an empty string (`""`) in lyric mode to be associated with a solmisasi rest. This way, the lyrics sequence will differ to the one in standard notation system.
2. occupying a `NullVoice` context, which holds the original non-converted music, to be used as the associated voice for the lyrics. This way, we'd better create an explicit `Lyrics` context to handle the lyrics.
{: .fs-5 }
