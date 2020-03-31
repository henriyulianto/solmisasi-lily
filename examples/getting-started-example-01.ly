%% Getting Started: Example #1
\version "2.20.0"
\include "solmisasi.ily"

\paper {
  paper-width = 80\mm
  paper-height = 250\mm
  indent = 0\mm
}

music = {
  \key c \major
  \relative c' {
    c4 d e f g a b c
  }
}

\new Staff {
  \new Voice {
    \music
  }
}

\markup\vspace #2

\new SolmisasiStaff {
  \new SolmisasiVoice {
    \solmisasiMusic \music
  }
}

\markup\vspace #2

\new SolmisasiStaff \with {
  \revert TimeSignature.stencil
} {
  \new SolmisasiVoice {
    \solmisasiMusic \music
  }
}

\markup\vspace #2

% implicit \score
<<
  \new SolmisasiTimeAndKeySignature {
    \solmisasiMusic \music
  }
  \new SolmisasiStaff \with {
    \revert TimeSignature.stencil
  } {
    \new SolmisasiVoice {
      \solmisasiMusic \music
    }
  }
>>

\markup\vspace #2

% implicit \score
<<
  \new SolmisasiTimeAndKeySignature \with {
    \omit TimeSignature
  } {
    \solmisasiMusic \music
  }
  \new SolmisasiStaff \with {
    \revert TimeSignature.stencil
  } {
    \new SolmisasiVoice {
      \solmisasiMusic \music
    }
  }
>>

\markup\vspace #2

% implicit \score
<<
  \new SolmisasiTimeAndKeySignature {
    \solmisasiMusic \music
  }
  \new SolmisasiStaff {
    \new SolmisasiVoice {
      \solmisasiMusic \music
    }
  }
>>