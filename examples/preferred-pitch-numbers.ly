\version "2.20.0"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example 1
%% Basic/simple usage of solmisasi-lily library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\include "solmisasi.ily"

\header {
  tagline = ##f
}

\paper {
  bookTitleMarkup = ##f
  paper-width = 90\mm
  paper-height = 54\mm
  top-margin = 5\mm
  right-margin = 5\mm
  bottom-margin = 0\mm
  left-margin = 5\mm
  #(define fonts
     (set-global-fonts
      #:sans "Helvetica Neue"
      #:music "emmentaler"
      #:brace "emmentaler"
      #:typewriter "Consolas"
      #:factor (/ staff-height pt 20)
      ))
  indent = 25\mm
  %short-indent = 0\mm
  ragged-right = ##t
}

\markuplist {
  \override #'(padding . 2)
  \fontsize #1
  \table #'(-1 0 -1 0) {
    a. \with-color #blue \sans \not-angka #(ly:make-pitch 0 0 1/2) #0 "is PREFERRED than" \with-color #red \sans \not-angka #(ly:make-pitch 0 1 -1/2) #0
    b. \with-color #blue \sans \not-angka #(ly:make-pitch 0 1 1/2) #0 "is PREFERRED than" \with-color #red \sans \not-angka #(ly:make-pitch 0 2 -1/2) #0
    c. \with-color #blue \sans\not-angka #(ly:make-pitch 0 3 1/2) #0 "is PREFERRED than" \with-color #red \sans \not-angka #(ly:make-pitch 0 4 -1/2) #0
    d. \with-color #blue \sans\not-angka #(ly:make-pitch 0 4 1/2) #0 "is PREFERRED than" \with-color #red \sans \not-angka #(ly:make-pitch 0 5 -1/2) #0
    e. \with-color #blue \sans\not-angka #(ly:make-pitch 0 6 -1/2) #0 "is PREFERRED than" \with-color #red \sans \not-angka #(ly:make-pitch 0 5 1/2) #0
    f. \with-color #red \sans \not-angka #(ly:make-pitch 0 0 -1/2) #0 "is NEVER USED!" ""
    g. \with-color #red \sans \not-angka #(ly:make-pitch 0 3 -1/2) #0 "is NEVER USED!" ""
  }
}