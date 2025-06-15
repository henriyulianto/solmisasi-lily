\version "2.25.26"

#(use-modules
  (guile)
  (lily)
  (lily page)
  (lily paper-system)
  (lily output-svg)
  (lily clip-region)
  (srfi srfi-1)
  (srfi srfi-2)
  (srfi srfi-13))

#(define font-css-url "https://use.typekit.net/stm0twa.css")
#(define svg-module (resolve-module '(lily framework-svg)))
#(module-define! svg-module 'svg-begin
                 (lambda rest
                   (string-append
                    (eo 'svg #t
                        '(xmlns . "http://www.w3.org/2000/svg")
                        '(xmlns:xlink . "http://www.w3.org/1999/xlink")
                        '(version . "1.2")
                        `(width . ,(ly:format "~2fmm" (first rest)))
                        `(height . ,(ly:format "~2fmm" (second rest)))
                        `(viewBox . ,(ly:format "~4f ~4f ~4f ~4f"
                                                (third rest) (fourth rest)
                                                (fifth rest) (sixth rest))))
                    (eo 'defs #f)
                    (eo 'style #t)
                    (ly:format "@import url(\"~a\");" font-css-url)
                    (ec 'style)
                    (ec 'defs)
                    (eo 'style #t '(type . "text/css"))
                    "<![CDATA[
tspan { white-space: pre; }
")))
