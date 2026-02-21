%% logging.ily
%%
%% (Part of "solmisasi-lily" library for Lilypond)
%%
%% Copyright (C) 2016 - Henri Yulianto
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

% Define loglevel constants
#(define solmisasi-loglevel-nolog    0)
#(define solmisasi-loglevel-critical 1)
#(define solmisasi-loglevel-warning  2)
#(define solmisasi-loglevel-log      3)
#(define solmisasi-loglevel-debug    4)

% Declare loglevel
#(define-public solmisasi-loglevel   3)

% Log file variable
#(define-public solmisasi-logfile   #f)

% Helper for setting loglevel
setLogLevel =
#(define-void-function (loglevel) (integer?)
   (if (and (>= loglevel solmisasi-loglevel-nolog)
            (<= loglevel solmisasi-loglevel-debug))
       (set! solmisasi-loglevel loglevel)))

% Open a log file when the first entry is actually written
openLogfile =
#(define-void-function () ()
   (if (not solmisasi-logfile)
       (set! solmisasi-logfile
             (open-output-file
              (format #f "~a.solmisasi.log" (ly:parser-output-name))))))

% Needs global variable of loglevel: solmisasi-loglevel

% Critical error
#(define (solmisasi:error fmt . vals)
   (if (>= solmisasi-loglevel solmisasi-loglevel-critical)
       (begin
        ;; open logfile upon first request
        (openLogfile)
        (if (ly:input-location? (*location*))
            (begin
             ;; console output
             (ly:error location
                       (format
                        #f (string-append "solmisasi-lily: " fmt) vals))
             ;; logfile output
             (format solmisasi-logfile fmt vals))
            (begin
             ;; this is an "abuse" of the parameters,
             ;; "location" is actually the "fmt" argument
             (ly:error
              (format
               #f (string-append "solmisasi-lily: " (*location*)) fmt))
             (format solmisasi-logfile
                     (format "error: ~a\n" (*location*)) fmt))))))

% Warning
#(define (solmisasi:warn fmt . vals)
   (if (>= solmisasi-loglevel  solmisasi-loglevel-warning)
       (begin
        (openLogfile)
        (if (ly:input-location? (*location*))
            (begin
             (ly:input-warning (*location*)
                               (format
                                #f (string-append "solmisasi-lily: " fmt) vals))
             (format solmisasi-logfile fmt vals))
            (begin
             (ly:warning
              (format
               #f (string-append "solmisasi-lily: " (*location*)) fmt))
             (format solmisasi-logfile
                     (format "warning: ~a\n" (*location*)) fmt))))))

% Logging
#(define (solmisasi:log fmt . vals)
   (if (>= solmisasi-loglevel solmisasi-loglevel-log)
       (begin
        ;(openLogfile)
        ; (format (current-output-port) "solmisasi-lily: ")
        ;         (format (current-output-port) fmt vals)
        ;         (format (current-output-port) "\n")
        (ly:message (format #f (string-append "solmisasi-lily: " fmt) vals))
        ;(format solmisasi-logfile fmt vals)
        )))

% Debug output
#(define (solmisasi:debug fmt . vals)
   (if (>= solmisasi-loglevel solmisasi-loglevel-debug)
       (begin
        (openLogfile)
        (if (ly:input-location? (*location*))
            (begin
             (ly:input-message (*location*)
                               (format
                                #f (string-append "solmisasi-lily: " fmt) vals))
             (format solmisasi-logfile fmt vals))
            (begin
             (ly:message
              (format
               #f (string-append "solmisasi-lily: " (*location*)) fmt))
             (format solmisasi-logfile
                     (format "log: ~a\n" (*location*)) fmt))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define LOGGING_LOADED #t)
#(solmisasi:log "* Logging module has been loaded.")
