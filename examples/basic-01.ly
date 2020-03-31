%%% basic-01.ly
\version "2.20.0"

% Custom paper dimension
#(set! paper-alist (cons '("custom" . (cons (* 190 mm) (* 120 mm))) paper-alist))

\paper {
  #(set-paper-size "custom")
  #(define fonts
    (set-global-fonts
     #:music "emmentaler"
     #:brace "emmentaler"
     #:roman "Utopia"
     #:sans "Helvetica Neue"
     #:typewriter "Consolas"
     #:factor (/ staff-height pt 20)
   ))
  indent = 0\mm
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
      \new Voice {
        \mary_music
      }
      \addlyrics {
        \mary_lyric
      }
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