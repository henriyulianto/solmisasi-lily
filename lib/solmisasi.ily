#(ly:set-option 'relative-includes #t)
\version "2.24.4"
%% solmisasi.ily
%%
%% (Part of "solmisasi-lily" library for Lilypond)
%%
%% Copyright © 2016-2024 - Henri Yulianto
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%%%%%% DEPECRATED
% #(if (ly:version? < (list 2 21 0))
%      (use-modules (scm song-util))
%      (if (ly:version? < (list 2 24 999))
%          (use-modules (lily song-util))))

%% MAIN WRAPPER SCRIPT for "solmisasi" library

#(ly:message "\n")

%% Taken from openLilyLib's os-path.scm

#(define-public (ly:programming-error fmt . vals)
   #f)

#(define-public solmisasi:log ly:message)

#(use-modules
  (lily)
  (ice-9 regex))

#(define SOLMISASI_LILY_VERSION "2.0.0-beta")
#(define-public SOLMISASI_LILY_LOADED #f)

#(define-public (use-solmisasi-lily-version-one)
   (begin
    (define USE_OLD_VERSION? #t)
    (set! SOLMISASI_LILY_VERSION "1.1.1-beta")))

#(define-public (solmisasi-lily-version)
   SOLMISASI_LILY_VERSION)

#(define-syntax for
   (syntax-rules (in)
     ((for element in list body ...)
      (map (lambda (element)
             body ...)
           list))))

%% #t when running a Windows OS
#(define-public is-windows
   (let ((os (getenv "OS")))
     (if (and (string? os)
              (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os))
         #t #f)))

%% forward or backward slash depending on OS
#(define-public os-path-separator
   (if is-windows
       #\\
       #\/ ))

#(define-public (split-path path)
   "Returns a list with path elements.
    Takes either a path string or a list.
    If 'path' is a string it is split
    respecting the OS dependent path separator,
    if it is a list then the list is returned,
    while elements are converted from symbol to string if necessary."
   (if (string? path)
       (string-split path os-path-separator)
       (map
        (lambda (elt)
          (if (string? elt)
              elt
              (symbol->string elt)))
        path)))

#(define-public (join-unix-path path)
   "Returns a Unix formatted path string from a (symbol?/string?) list."
   (string-join (split-path path) "/"))

#(define-public (join-dot-path path)
   "Returns a string in dot-notation (to be displayed).
   Takes a list with symbol?/string? elements or an
   OS independent path string."
   (let ((path-list (split-path path)))
     (string-join path-list ".")))

#(define-public (get-cwd-list)
   "Return the current working directory as a list of strings."
   (split-path (getcwd)))

#(define-public (absolute-path? path)
   "Test if the given path is absolute.
    Process either a string or a symbol?/string? list."
   (let ((path-list (split-path path)))
     (if (and (> (length path-list) 0)
              ;; consider the path absolute if either the regex for windows volumes is matched
              ;; or the first list element is empty (indicating a "/" unix root)
              (or (regexp-exec (make-regexp "^[a-z]:$" regexp/icase) (car path-list))
                  (= 0 (string-length (car path-list)))))
         #t #f)))

#(define-public (normalize-path path)
   "Return a normalized path by removing '.' and '..' elements.
    If 'path' is a string a normalized string is returned,
    if it is a list a list is returned.
    The input string is OS independent (takes os-dependent path separator)
    but the resulting string is Unix-like (because this is nearly always what we need."
   (let* ((path-list (split-path path))
          (normalized
           (let ((ret '()))
             (for-each
              (lambda (e)
                (set! ret (cond ((equal? e "..")(if (> (length ret) 1) (cdr ret) (cdr (reverse (get-cwd-list)))))
                                ((equal? e ".") (if (= (length ret) 0) (reverse (get-cwd-list)) ret))
                                (else `(,e ,@ret))))) path-list)
             (reverse ret))))
     (if (string? path)
         (string-join normalized "/" 'infix)
         normalized)))

#(define-public (absolute-path path)
   "Return absolute path of given 'path'.
    Path can be given as string or string list.
    If 'path' is an absolute path it is simply normalized,
    if it is a relative path it is interpreted as relative 
    to the current working directory.
    Input is OS independent, output is Unix style."
   (let* ((is-string (string? path))
          (path-list (split-path path))
          (abs-path
           (if (absolute-path? path-list)
               path-list
               (append
                (get-cwd-list)
                (normalize-path path-list)))))
     (if is-string
         (string-join abs-path "/" 'infix)
         abs-path)))

#(define-public (normalize-location location)
   "Returns a normalized path to the given location object"
   (normalize-path (car (ly:input-file-line-char-column location))))

#(define-public (location-extract-path location)
   "Returns the normalized path from a LilyPond location
    or './' if 'location' is in the same directory."
   (let* ((loc (normalize-location location))
          (dirmatch (string-match "(.*/).*" loc))
          (dirname (if (regexp-match? dirmatch)
                       (let ((full-string (match:substring dirmatch 1)))
                         (substring full-string
                                    0
                                    (- (string-length full-string) 1)))
                       ".")))
     (normalize-path dirname)))

%% Return the normalized absolute path and file name of the
%% file where this function is called from (not the one that
%% is compiled by LilyPond).
#(define-public this-file
   (define-scheme-function (location)()
     (normalize-location location)))

#(define-public (file-compiled? location)
   "Return #t if the file where this function is called
    is the one that is currently compiled by LilyPond."
   (let ((outname (ly:parser-output-name))
         (locname (normalize-location location)))
     (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

%% LilyPond format wrapper for this-file-compiled?
#(define-public this-file-compiled?
   (define-scheme-function (location)()
     (file-compiled? location)))

%%%%% End of os-path.scm

#(define-public SOLMISASI_LIB_DIR
   (normalize-path
    (location-extract-path (*location*))))

#(if (ly:version? >= (list 2 25 0))
     (begin
      ; Append the directory of this file to current parser's include path
      (ly:parser-append-to-include-path SOLMISASI_LIB_DIR)
      (ly:parser-append-to-include-path (ly:format "~a/include" SOLMISASI_LIB_DIR))
      )
     (ly:parser-include-string (ly:format "\\include \"~a/include/backward-from-2.25.ily\"\n"
                                          SOLMISASI_LIB_DIR)))

%% List of include files
#(define ily-files
   (list
    "framework-svg.mod"
    ;"logging"
    "define-pitch-names"
    "misc-functions"
    "solmisasi-script-alist"
    "solmisasi-markups"
    "solmisasi-engravers"
    (if (defined? 'USE_OLD_VERSION?)
        "solmisasi-music-parser"
        "solmisasi-music-parser-v2")
    "solmisasi-layout-definition"
    "solmisasi-finished-loading"
    ))

#(ly:message (ly:format "Start loading \"solmisasi-lily\" v~a ...\n"
                        SOLMISASI_LILY_VERSION))

%% INCLUDE ALL
#(let* ((parserStr ""))
   (for file in ily-files
        (set! parserStr
              (ly:format "~a\\include \"~a/include/~a.ily\"\n"
                         parserStr
                         SOLMISASI_LIB_DIR
                         file)))
   (ly:parser-include-string parserStr))

%% Initialize default serif and sans fonts
\paper {
  property-defaults.fonts.serif =
  #(if (eq? 'svg (ly:get-option 'backend))
       "source-serif-4"
       "Lilypond Serif")
  property-defaults.fonts.sans =
  #(if (eq? 'svg (ly:get-option 'backend))
       "source-sans-3"
       "Aptos")
}

#(ly:message "\n")
