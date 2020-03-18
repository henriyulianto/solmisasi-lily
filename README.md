## "solmisasi-lily" – "Solmisasi" library for Lilypond

### A. Preamble

**"solmisasi‐lily"** is a library developed as a workaround on engraving _solmisasi_ numbered music notation that are widely used in Indonesia using the almighty [Lilypond](http://lilypond.org). This library would always be considered in **beta phase**, since some extra works might have to be done by users in engraving such a specific score or sheet music in this notation system.

#### A. 1. What is *solmisasi* (EN: solmization)?

_Solmisasi (not angka)_ is a music notation system which is widely used in Indonesia to represent *solfège* in numbers (`0..7`). It is quite similar to Chinese *Jianpu* system. In Indonesia, particularly, *solmisasi* system takes the form of `1` to `7` (one to seven) for `do`, `re`, `mi`, `fa`, `sol`, `la`, `si`; and `0` (zero) for a single rest, with some rules/conventions described below.<br>
(Notes: These rules/conventions are based on my knowledge about this notation system. It could possibly differ to what rules other Indonesian people know/use.)
- ##### Pitches and Accidentals
  1. _Sharp notes_ (♯) are notated by putting a single "**/**" (upward slash) over the pitch number.
  2. _Flat notes_ (♭) are notated by putting a single "**\\**" (downward backslash) over the pitch number.
  3. This notation system is by far not able to handle _double-flats_ and _double-sharps_. Those kind of pitches should be notated in their 'normalized-enharmonic' pitch, for example:<br>
    `4` for `geses` (g♭♭) in C major (g♭♭ is normalized as f), and<br>
    `5` for `cisis` (c♯♯) in G major.
  4. This notation system doesn't know any _microtonal_ system.
- ##### Octave Pitches
  1. _Single dot_ ABOVE a specific pitch number is used to represent an octave higher pitch.
  2. _Single dot_ BELOW a specific pitch number is used to represent an octave lower pitch.
- ##### Note Durations
  1. Every single pitch/note number is considered as a _quarter note/crotchet_, if there are no horizontal lines (beams) above.
  2. Similarly, every single duration dot (`.`) is considered as a _quarter/crothet rest_, if there are no horizontal lines (beams) above.
  3. Duration dots (`.`) maintain their own "rhythmic space", unlike the standard notation in which they are "glued" left to its parent/corresponding note or rest.
- ##### Scales and Key Signatures
  1. *Solmisasi* occupies _MOVABLE/FLEXIBLE 'DO'_ system. Note pitches are written as if they are in major scales. In a C major passage, the pitch `d` is equivalent with `2` (`re`), or vice versa. This also applies to passages written in A minor.<br>
    In a G major passage, the pitch `fis` is equivalent with `7` (`si`), or vice versa. This also applies to passages written in E minor.
  2. Consequently, major scales are written with the pitch `1` (`do`) as the root, for example: `1=F♯` or `1=f♯` for F♯ major, and `1=E♭` or `1=e♭` for E♭ major.
    Minor scales are often written with the pitch `6` (`la`) as the root, for example: `6=F` or `6=f` for F minor, and `6=G` or `6=g` for G minor.
  3. Key accidental suffixes (sharp and flat) can also be substituted with their equivalent text suffixes, e.g. `is` for sharp, and `es` for flat. For example: `1=Fis` equals to `1=F♯`, and `1=Es` equals to `1=E♭`.<br>
    (Note: For Indonesian people, `Es` or `es` is more commonly used than `Ees` or `ees`, even though, in my opinion, this is inconsistent.)
  4. There are many cases in which minor keys are written in their equivalent major keys, such as `1=B♭` or `1=Bes` for `6=G`, and `1=A♭` or `1=As` for `6=F`.<br>
    (Note: Similar to the case of `es`, `As` or `as` is more commonly used than `Aes` or `aes`.)

#### A. 2. Illustrations of the Differences between the Standard (Western) Notation System and the Number Notation

(to be completed later)

#### A. 3. Further readings
- [Solmization](https://en.wikipedia.org/wiki/Solmization)
- [Solfège](https://en.wikipedia.org/wiki/Solfège)
- [Numbered musical notation](https://en.wikipedia.org/wiki/Numbered_musical_notation)
