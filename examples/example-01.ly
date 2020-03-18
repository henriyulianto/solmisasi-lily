\version "2.20.0"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example 1
%% Basic/simple usage of solmisasi-lily library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\include "solmisasi.ily"

\header {
  title = "Example 1"
  subtitle = "Basic usage of 'solmisasi-lily' library"
  subsubtitle = "    "
  tagline = ##f
}

\paper {
  paper-width = 279\mm
  top-margin = 10\mm
  right-margin = 10\mm
  bottom-margin = 10\mm
  left-margin = 10\mm
  #(define fonts
     (set-global-fonts
      #:music "emmentaler"
      #:brace "emmentaler"
      #:typewriter "Consolas"
      #:factor (/ staff-height pt 20)
      ))
}

music = \relative c' {
  c4 d e f g a b c
}

lyric = \lyricmode {
  do re mi fa sol la si do
}

theScore = \score {
  \new Staff {
    \new Voice {
      \music
    }
    \addlyrics {
      \lyric
    }
  }
}

theSolmisasi = \score {
  \new SolmisasiStaff {
    \new SolmisasiVoice {
      \solmisasiMusic \music
    }
    \addlyrics {
      \solmisasiLyric \lyric
    }
  }
}

\markup \fill-line {
  \override #'(box-padding . 1)
  \box
  \left-column {
    \line { \bold { Standard Music Notation } \score { \theScore } }
    \vspace #1
    \with-color #darkblue \fontsize #-1 \typewriter {
      "music = \\relative c' {"
      "  c4 d e f g a b c"
      "}"
      "lyric = \\lyricmode {"
      "  do re mi fa sol la si do"
      "}"
      "\\score {"
      "  <<"
      "    \\new Staff {"
      "      \\new Voice {"
      "        \\music\n"
      "      }"
      "      \\addlyrics {"
      "        \\lyric"
      "      }"
      "    }"
      " >>"
      "}"
    }
  }
  \override #'(box-padding . 1)
  \box
  \left-column {
    \line { \bold { Solmisasi (Not Angka) } \score { \theSolmisasi } }
    \vspace #1
    \with-color #darkblue \fontsize #-1 \typewriter {
      "music = \\relative c' {"
      "  c4 d e f g a b c"
      "}"
      "lyric = \\lyricmode {"
      "  do re mi fa sol la si do"
      "}"
      "\\score {"
      "  <<"
      \line { "    \\new" \with-color #red \bold "SolmisasiStaff {" }
      \line { "      \\new" \with-color #red \bold "SolmisasiVoice {" }
      \line { \with-color #red \bold "        \\solmisasiMusic" "\\music\n" }
      "      }"
      "      \\addlyrics {"
      \line { \with-color #red \bold "        \\solmisasiLyric" "\\lyric" }
      "      }"
      "    }"
      " >>"
      "}"
    }
  }
}
