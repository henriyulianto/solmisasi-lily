%%% helper-functions-demo.ly
%%% By Henri Yulianto, 2020
\version "2.20.0"
\include "solmisasi.ily"
\language "solmisasi"

#(set-global-staff-size 18)

\paper {
  paper-width = 120\mm
  paper-height = 280\mm
  #(define fonts
     (set-global-fonts
      #:typewriter "JetBrains Mono"
      #:factor (/ staff-height pt 20)
      ))
  indent = 0\mm
  tagline = ##f
}

\layout {
  \set Score.proportionalNotationDuration = #(ly:make-moment 1/4)
}

maj_music = { do4 re mi fa sol la si do' }
min_music = { la,4 si, do re mi fa sol la }

\markup {
  \typewriter \left-column {
    "\version \"2.20.0\""
    "\include \"solmisasi.ily\""
    \bold \with-color #red "\language \"solmisasi\""
    "maj_music = { do4 re mi fa sol la si do' }"
    "min_music = { la,4 si, do re mi fa sol la }"
    \vspace #1.5

    \with-color #grey "%% Major Keys %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    \vspace #1
    \with-color #blue "\flexibleDo c' \maj_music"
    \vspace #0.5
    \score { \flexibleDo c' \maj_music }
    \vspace #1
    \with-color #blue "\doEqualsTo d' \maj_music"
    \vspace #0.5
    \score { \doEqualsTo d' \maj_music }
    \vspace #1
    \concat {
      \with-color #blue "\doSamaDengan f' \maj_music"
      \with-color #grey "   % Bahasa Indonesia"
    }
    \vspace #0.5
    \score { \doSamaDengan f' \maj_music }
    \vspace #1.5

    \with-color #grey "%% Minor Keys %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    \vspace #1
    \with-color #blue "\flexibleLa a' \min_music"
    \vspace #0.5
    \score { \flexibleLa a' \min_music }
    \vspace #1
    \with-color #blue "\laEqualsTo d' \min_music"
    \vspace #0.5
    \score { \laEqualsTo d' \min_music }
    \vspace #1
    \concat {
      \with-color #blue "\laSamaDengan e \min_music"
      \with-color #grey "   % Bahasa Indonesia"
    }
    \vspace #0.5
    \score { \laSamaDengan e' \min_music }
    \vspace #1.5

    \with-color #grey "%% Silence %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    \vspace #1
    \with-color #blue "\clef bass \maj_music \min_music"
    \vspace #0.5
    \score { { \clef bass \maj_music \min_music } }
    \vspace #1
    \concat {
      \with-color #blue "\clef bass \maj_music "
      \with-color #red "\silence \min_music"
    }
    \vspace #0.5
    \score { { \clef bass \maj_music \silence \min_music } }
    \vspace #1
    \concat {
      \with-color #blue "\clef bass "
      \with-color #red "\silence \maj_music "
      \with-color #blue "\min_music"
    }
    \vspace #0.5
    \score { { \clef bass \silence \maj_music \min_music } }
  }
}

