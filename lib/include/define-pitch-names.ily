%% define-pitch-names.ily
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

#(define-public solmisasi-pitchnames
   `(
      ;;===================================================;;
      ;; dob undefined ==> si
      ;; do# = di
      (do . ,(ly:make-pitch -1 0 NATURAL))
      (do# . ,(ly:make-pitch -1 0 SHARP))
      (di . ,(ly:make-pitch -1 0 SHARP))

      ;; reb = ra, rarely used
      ;; re# = ri
      (reb . ,(ly:make-pitch -1 1 FLAT))
      (ra . ,(ly:make-pitch -1 1 FLAT))
      (re . ,(ly:make-pitch -1 1 NATURAL))
      (ri . ,(ly:make-pitch -1 1 SHARP))
      (re# . ,(ly:make-pitch -1 1 SHARP))

      ;; mib = ma
      ;; mi# undefined ==> fa
      (mib . ,(ly:make-pitch -1 2 FLAT))
      (ma . ,(ly:make-pitch -1 2 FLAT))
      (mi . ,(ly:make-pitch -1 2 NATURAL))

      ;; fab undefined ==> mi
      ;; fa# = fi
      (fa . ,(ly:make-pitch -1 3 NATURAL))
      (fi . ,(ly:make-pitch -1 3 SHARP))
      (fa# . ,(ly:make-pitch -1 3 SHARP))

      ;; solb = sal, rarely used
      ;; sol# = sel
      (solb . ,(ly:make-pitch -1 4 FLAT))
      (sal . ,(ly:make-pitch -1 4 FLAT))
      (sol . ,(ly:make-pitch -1 4 NATURAL))
      (sel . ,(ly:make-pitch -1 4 SHARP))
      (sol# . ,(ly:make-pitch -1 4 SHARP))

      ;; lab = le
      ;; la# = li
      (lab . ,(ly:make-pitch -1 5 FLAT))
      (le . ,(ly:make-pitch -1 5 FLAT))
      (la . ,(ly:make-pitch -1 5 NATURAL))
      (la# . ,(ly:make-pitch -1 5 SHARP))
      (li . ,(ly:make-pitch -1 5 SHARP))

      ;; sib = sa
      ;; si# undefined ==> do
      (sib . ,(ly:make-pitch -1 6 FLAT))
      (sa . ,(ly:make-pitch -1 6 FLAT))
      (si . ,(ly:make-pitch -1 6 NATURAL))
      ;;===================================================;;

      ;; include Netherlands' pitch names
      ;; (copied from define-note-names.scm)
      (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
      (ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
      (ces . ,(ly:make-pitch -1 0 FLAT))
      (ceh . ,(ly:make-pitch -1 0 SEMI-FLAT))
      (c . ,(ly:make-pitch -1 0 NATURAL))
      (cih . ,(ly:make-pitch -1 0 SEMI-SHARP))
      (cis . ,(ly:make-pitch -1 0 SHARP))
      (cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
      (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

      (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
      (deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
      (des . ,(ly:make-pitch -1 1 FLAT))
      (deh . ,(ly:make-pitch -1 1 SEMI-FLAT))
      (d . ,(ly:make-pitch -1 1 NATURAL))
      (dih . ,(ly:make-pitch -1 1 SEMI-SHARP))
      (dis . ,(ly:make-pitch -1 1 SHARP))
      (disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
      (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

      (eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
      (eses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
      (eeseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
      (ees . ,(ly:make-pitch -1 2 FLAT))
      (es . ,(ly:make-pitch -1 2 FLAT))
      (eeh . ,(ly:make-pitch -1 2 SEMI-FLAT))
      (e . ,(ly:make-pitch -1 2 NATURAL))
      (eih . ,(ly:make-pitch -1 2 SEMI-SHARP))
      (eis . ,(ly:make-pitch -1 2 SHARP))
      (eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
      (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

      (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
      (feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
      (fes . ,(ly:make-pitch -1 3 FLAT))
      (feh . ,(ly:make-pitch -1 3 SEMI-FLAT))
      (f . ,(ly:make-pitch -1 3 NATURAL))
      (fih . ,(ly:make-pitch -1 3 SEMI-SHARP))
      (fis . ,(ly:make-pitch -1 3 SHARP))
      (fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
      (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

      (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
      (geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
      (ges . ,(ly:make-pitch -1 4 FLAT))
      (geh . ,(ly:make-pitch -1 4 SEMI-FLAT))
      (g . ,(ly:make-pitch -1 4 NATURAL))
      (gih . ,(ly:make-pitch -1 4 SEMI-SHARP))
      (gis . ,(ly:make-pitch -1 4 SHARP))
      (gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
      (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

      (aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
      (ases . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
      (aeseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
      (aes . ,(ly:make-pitch -1 5 FLAT))
      (as . ,(ly:make-pitch -1 5 FLAT))
      (aeh . ,(ly:make-pitch -1 5 SEMI-FLAT))
      (a . ,(ly:make-pitch -1 5 NATURAL))
      (aih . ,(ly:make-pitch -1 5 SEMI-SHARP))
      (ais . ,(ly:make-pitch -1 5 SHARP))
      (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
      (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

      (beses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
      (beseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
      (bes . ,(ly:make-pitch -1 6 FLAT))
      (beh . ,(ly:make-pitch -1 6 SEMI-FLAT))
      (b . ,(ly:make-pitch -1 6 NATURAL))
      (bih . ,(ly:make-pitch -1 6 SEMI-SHARP))
      (bis . ,(ly:make-pitch -1 6 SHARP))
      (bisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
      (bisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
      ))

#(append! language-pitch-names (list (cons 'solmisasi solmisasi-pitchnames)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define PITCHNAMES_LOADED #t)
#(ly:message "* Solmisasi pitch names data has been loaded.")
