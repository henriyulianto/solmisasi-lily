#(ly:set-option 'relative-includes #t)
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

%% MAIN WRAPPER SCRIPT for "solmisasi" library

#(begin
  ;;Taken from openLilyLib's os-path.scm

  (define-public (ly:programming-error fmt . vals)
    #f)

  (use-modules
   (lily)
   (ice-9 regex))

  (define-public _VERSION "1.1.0-beta")
  (define _SOLMISASI_LILY_LOADED #f)
  (define-syntax for
    (syntax-rules (in)
      ((for element in list body ...)
       (map (lambda (element)
              body ...)
            list))))

  ;; #t when running a Windows OS
  (define-public is-windows
    (let ((os (getenv "OS")))
      (if (and (string? os)
               (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os))
          #t #f)))

  ;; forward or backward slash depending on OS
  (define-public os-path-separator
    (if is-windows
        #\\
        #\/ ))

  (define-public (split-path path)
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

  (define-public (join-unix-path path)
    "Returns a Unix formatted path string from a (symbol?/string?) list."
    (string-join (split-path path) "/"))

  (define-public (join-dot-path path)
    "Returns a string in dot-notation (to be displayed).
   Takes a list with symbol?/string? elements or an
   OS independent path string."
    (let ((path-list (split-path path)))
      (string-join path-list ".")))

  (define-public (get-cwd-list)
    "Return the current working directory as a list of strings."
    (split-path (getcwd)))

  (define-public (absolute-path? path)
    "Test if the given path is absolute.
    Process either a string or a symbol?/string? list."
    (let ((path-list (split-path path)))
      (if (and (> (length path-list) 0)
               ;; consider the path absolute if either the regex for windows volumes is matched
               ;; or the first list element is empty (indicating a "/" unix root)
               (or (regexp-exec (make-regexp "^[a-z]:$" regexp/icase) (car path-list))
                   (= 0 (string-length (car path-list)))))
          #t #f)))

  (define-public (normalize-path path)
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

  (define-public (absolute-path path)
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

  (define-public (normalize-location location)
    "Returns a normalized path to the given location object"
    (normalize-path (car (ly:input-file-line-char-column location))))

  (define-public (location-extract-path location)
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

  ;; Return the normalized absolute path and file name of the
  ;; file where this function is called from (not the one that
  ;; is compiled by LilyPond).
  (define-public thisFile
    (define-scheme-function (location)()
      (normalize-location location)))

  (define-public (this-file-compiled? location)
    "Return #t if the file where this function is called
    is the one that is currently compiled by LilyPond."
    (let ((outname (ly:parser-output-name))
          (locname (normalize-location location)))
      (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

  ;; LilyPond format wrapper for this-file-compiled?
  (define-public thisFileCompiled
    (define-scheme-function (location)()
      (this-file-compiled? location)))

;;;;;; End of os-path.scm

  (define-public _SOLMISASI_LIB_DIR
    (normalize-path
     (location-extract-path (*location*))))

  ;; List of include files
  (define ily-files
    (if (not (defined? '_USE_VERSION2))
        '(
           "logging"
           "define-pitch-names"
           "misc-functions"
           "solmisasi-script-alist"
           "solmisasi-markups"
           "solmisasi-engraver"
           "solmisasi-music-parser"
           "solmisasi-layout-definition"
           "solmisasi-finished-loading"
           )
        ;; use version2
        '(
           "logging"
           "define-pitch-names"
           "misc-functions"
           "solmisasi-script-alist"
           "solmisasi-markups"
           "solmisasi-engraver-v2"
           "solmisasi-music-parser-v2"
           "solmisasi-layout-definition-v2"
           "solmisasi-finished-loading"
           )))

  (ly:message (format #f "Start loading \"solmisasi-lily\" v~a ...\n"
                      _VERSION))

  (let* ((parserStr ""))
    (for file in ily-files
         (set! parserStr
               (format #f "~a\\include \"~a/include/~a.ily\"\n"
                       parserStr
                       _SOLMISASI_LIB_DIR
                       file)))
    (ly:parser-include-string parserStr))

  )

% #(define-public (scorify-music music)
%    "Preprocess @var{music}."
%    (set! music (solmisasiMusic music))
%    (ly:make-score
%     (fold (lambda (f m) (f m))
%           music
%           toplevel-music-functions)))
