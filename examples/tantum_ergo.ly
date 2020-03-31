\layout{
  %papersize = "letter"

  line-width = 190 \mm

  indent = 0 \mm

  %interscoreline = 9.0 \mm

}



\include "english.ly"

%\version "2.20.0"



\header {

  title = "Tantum Ergo Sacramentum"

  poet = "Tommaso d'Aquino"

  composer = "F.J.Haydn (1732 - 1809)"

  enteredby = "Giorgio Vicario"

  maintainer = "Gorgio Vicario"

  maintainerEmail = "katobleto@yahoo.com"

  lastupdated = "2008/2/4"

  copyright = "Public Domain"

  mutopiacomposer = "HaydnFJ"

  mutopiainstrument = "Voice (SATB)"

  mutopiastyle = "Classical"

  source = "Transcription"

  footer = "Mutopia-2008/02/19-1322"

  tagline = \markup { \override #'(box-padding . 1.0) \override #'(baseline-skip . 2.7) \box \center-column { \small \line { Sheet music from \with-url "http://www.MutopiaProject.org" \line { \teeny www. \hspace #-1.0 MutopiaProject \hspace #-1.0 \teeny .org \hspace #0.5 } • \hspace #0.5 \italic Free to download, with the \italic freedom to distribute, modify and perform. } \line { \small \line { Typeset using \with-url "http://www.LilyPond.org" \line { \teeny www. \hspace #-1.0 LilyPond \hspace #-1.0 \teeny .org } by \maintainer \hspace #-1.0 . \hspace #0.5 Reference: \footer } } \line { \teeny \line { This sheet music has been placed in the public domain by the typesetter, for details see: \hspace #-0.5 \with-url "http://creativecommons.org/licenses/publicdomain" http://creativecommons.org/licenses/publicdomain } } } }

}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


soprani = {
  \relative c'' {

    \clef "G^8" \key c\major \time 3/4

    \repeat volta 2 {

      e2\mf^\markup {\italic "Andante"} e4\< | e8 ( f8 ) f4. ( g8 ) \! |

      a4 ( g4 ) \> f4 | f8 ( e8 ) e4 \! r4 |
      g2 g4 | a2 f4 | e2 d4 | e2 r4 |

      g2 g4 | g4 g2 | a2 a4 | a8 ( g8 ) g2\fermata | \break
      g2 g4 | g2 a4 | g2 fs4 | g2 r4 |

      e2 \pp e4 | e8 ( f8 ) f4. ( g8 ) | a4 ( g4 ) f4 | f8 ( e8 ) e4 r4 |

      g2 \f g4 | a2 f4 | e2 \pp d4 | e2 r4
    }
    a2. | g2.

    \bar "||"
  }

}



contralti = {
  \relative c'' {

    \key c\major \time 3/4

    \repeat volta 2 {

      c2\mf c4\< | c4 c2 \! |

      c2 \> c4 | c4 c4 \! r4 |

      e4 ( d4 ) c4 | c4 ( f4 ) d4 | c2 b4 | c2 r4 |

      e2 e4 | e8 ( d8 ) d2 | e2 e4 | e4 d2\fermata |

      d2 d4 | e4 d4 e4 | d2 c4 | b2 r4 |
      c2 \pp c4 | c4 c2 | c2 c4 | c4 c4 r4 |

      e4 \f ( d4 ) c4 | c4 ( f4 ) d4 | c2 \pp b4 | c2 r4
    }
    f2. | e2.

    \bar "||"
  }

}



tenori = {
  \relative c'' {

    \key c\major \time 3/4

    \repeat volta 2 {

      g2\mf g4\< | g8 ( a8 ) a4. ( b8 ) \! |

      c4 \> ( b4 ) a4 | a8 ( g8 ) g4 \! r4 |

      c4 ( b4 ) c4 | c2 a4 | g2 g4 | g2 r4 |

      c2 c4 | c8 ( b8 ) b2 | c2 c4 | c8 ( b8 ) b2\fermata |

      b2 b4 | c4 d4 c4 | b2 a4 | g2 r4 |

      g2\pp g4 | g8 ( a8 ) a4. ( b8 ) |

      c4 ( b4 ) a4 | a8 ( g8 ) g4 r4 |

      c4 \f ( b4 ) c4 | c2 a4 | g2 \pp g4 | g2 r4
    }
    c2. | c2.

    \bar "||"
  }

}



bassi = {
  \relative c {

    \clef bass \key c\major \time 3/4

    \repeat volta 2 {

      c2\mf c4\< | c4 c2 \! |

      c2 \> c4 | c4 c4 \! r4 |
      c4 d4 e4 | f2 f4 | g2 g,4 | c2 r4 |

      c4 e4 c4 | g'4 g2 | fs4 d4 fs4 | g4 g2\fermata |

      g2 g4 | c,4 b4 c4 | d2 d4 | g2 r4 |
      c,2 \pp c4 | c4 c2 |
      c2 c4 | c4 c4 r4 |

      c4 \f d4 e4 | f2 f4 | g2 \pp g,4 | c2 r4
    }
    f2. | c2.

    \bar "||"
  }

}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
    com -- par sit lau -- da -- ti --o.

    A -- men

  }

}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






\score {

  {

    {


      <<

        \new Staff = "r1" \context Voice = "v1" { \soprani }

        \new Staff = "r2" { \contralti }

        \new Staff = "r3" { \tenori }

        \new Staff = "r4" { \bassi }

        \new Lyrics \lyricsto "v1" { \set stanza = "1." \strofaa }

        \new Lyrics \lyricsto "v1" { \set stanza = "2." \strofab }

      >>

    }

  }

  \layout {

    \context { \Staff \RemoveEmptyStaves }


  }
  %\midi { }
}

\markup { }
\markup { _______________________________________________________________________________________________ }
\markup {
  Un così grande sacramento veneriamo, dunque, chini
  e il vecchio rito trovi compimento nel nuovo.
}
\markup { Presti la fede supplemento all'insufficienza dei sensi. }
\markup {
  Al Padre e al Figlio sia lode e giubilo,
  acclamazione, onore, virtù e benedizione.
}
\markup { A Colui che procede da entrambi, sia rivolta pari lode. }
\markup { Amen. }


%{
convert-ly.py (GNU LilyPond) 2.20.0  convert-ly.py: Processing `'...
Applying conversion: 2.11.2, 2.11.3, 2.11.5, 2.11.6, 2.11.10, 2.11.11,
2.11.13, 2.11.15, 2.11.23, 2.11.35, 2.11.38, 2.11.46, 2.11.48,
2.11.50, 2.11.51, 2.11.52, 2.11.53, 2.11.55, 2.11.57, 2.11.60,
2.11.61, 2.11.62, 2.11.64, 2.12.0, 2.12.3, 2.13.0, 2.13.1, 2.13.4,
2.13.10, 2.13.16, 2.13.18, 2.13.20, 2.13.27, 2.13.29, 2.13.31,
2.13.36, 2.13.39, 2.13.40, 2.13.42, 2.13.44, 2.13.46, 2.13.48,
2.13.51, 2.14.0, 2.15.7, 2.15.9, 2.15.10, 2.15.16, 2.15.17, 2.15.18,
2.15.19, 2.15.20, 2.15.25, 2.15.32, 2.15.39, 2.15.40, 2.15.42,
2.15.43, 2.16.0, 2.17.0, 2.17.4, 2.17.5, 2.17.6, 2.17.11, 2.17.14,
2.17.15, 2.17.18, 2.17.19, 2.17.20, 2.17.25, 2.17.27, 2.17.29,
2.17.97, 2.18.0, 2.19.2, 2.19.7, 2.19.11, 2.19.16, 2.19.22, 2.19.24,
2.19.28, 2.19.29, 2.19.32, 2.19.40, 2.19.46, 2.19.49, 2.19.80, 2.20.0
%}
