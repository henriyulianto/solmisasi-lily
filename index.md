---
layout: default
title: Home
nav_order: 1
description: "\"solmisasi-lily\" – \"Solmisasi\" library for Lilypond"
permalink: /
---

# Let's write _Solmisasi ("notasi angka")_ using Lilypond
{: .no_toc .fs-9 }

**"solmisasi‐lily"** is a library developed as a workaround on engraving _solmisasi_ numbered music notation that are widely used in Indonesia using the almighty [Lilypond](http://lilypond.org). This library would always be considered in **beta phase**, since some extra works might have to be done by users in engraving such a specific score or sheet music in this notation system.
{: .fs-6 }

[Preamble](#preamble){: .btn .btn-primary .fs-5 .mb-4 .mb-md-0 .mr-2 } [Further Reading](#further-reading){: .btn .btn-green .fs-5 .mb-4 .mb-md-0 .mr-2 } [View it on GitHub](https://github.com/henriyulianto/solmisasi-lily){: .btn .fs-5 .mb-4 .mb-md-0 }

![](/assets/img/solmisasi-art2.jpg)

---

## Preamble
{: .no_toc }

_Solmisasi (notasi angka)_ (EN: solmization) is a music notation system which is widely used in Indonesia, especially for vocal scores/sheet music, to represent *solfège* in numbers (`0..7`). It is quite similar to Chinese *Jianpu* system. In Indonesia, particularly, *solmisasi* system takes the form of `1` to `7` (one to seven) for `do`, `re`, `mi`, `fa`, `sol`, `la`, `si`; and `0` (zero) for a single rest, with some rules/conventions described below.<br>
{: .fs-5 }
(Notes: These rules/conventions are based on the author's knowledge about this notation system. It could possibly differ to what rules other Indonesian people know/use. Some constraints that this library applied, will also be explained.)
{: .fs-4 }

---

## _Solmisasi_ Rules/Conventions
{: .no_toc .text-delta .fs-6 }

1. TOC
{:toc .fs-5 }

---

### Pitches
  - _Solmisasi_ is mostly used to write vocal compositions and arrangements. So, possible pitch range is from `E2` (the lowest male pitch, could possibly be lower) up to `C6` (the highest female pitch, could possibly be higher). However, there is no such validation of pitches included in this library.
  - This system is mainly applied in vocal/choral music. Pitch numbers are written in a 'relative' way, based on the gender of vocal instruments. The solmisasi pitch numbers written on male vocal staves (tenor, baritone, and bass) are actually 1-octave lower than the same ones written on female vocal staves (soprano, mezzo-soprano, and alto).
{: .fs-5 }

### Accidentals
  - _Sharp notes_ (♯) are notated by putting a single "**/**" (upward slash) over the pitch number.
  - _Flat notes_ (♭) are notated by putting a single "**\\**" (downward backslash) over the pitch number.
  - This notation system is by far not able to handle _double-flats_ and _double-sharps_. Those kind of pitches should be notated in their 'normalized-enharmonic' pitch, for example:<br>
    `4` for `geses` (g♭♭) in C major (g♭♭ is normalized as f), and<br>
    `5` for `cisis` (c♯♯) in G major.
  - _Solmisasi_ system doesn't know any _microtonal_ system.
{: .fs-5 }

### Octavations
  - 1-octave HIGHER/LOWER pitches are individually written as a grob (graphical object) containing the corresponding pitch number with a _single dot_ ABOVE/BELOW, respectively.
  - 2-octaves HIGHER/LOWER pitch are individually written as a grob containing the corresponding pitch number with a _double dot_ ABOVE/BELOW, respectively. Consequently, this double dot should be stacked vertically. However, to maintain the vertical spaces, this library draws a horizontal double dot.
{: .fs-5 }

### Note Durations
  - Every single pitch/note number is considered as a _quarter note/crotchet_, if there are no horizontal lines (beams) above.
  - Similarly, every single duration dot (`.`) is considered as a _quarter/crothet rest_, if there are no horizontal lines (beams) above.
  - Duration dots (`.`) maintain their own "rhythmic space", unlike the standard notation in which they are "glued" left to its parent/corresponding note or rest.
{: .fs-5 }

### Scales and Key Signatures
  - *Solmisasi* occupies _MOVABLE/FLEXIBLE 'DO'_ system. Note pitches are written as if they are in major scales. In a C major passage, the pitch `d` equals to `2` (`re`), or vice versa. This also applies to passages written in A minor.<br>
    In a G major passage, the pitch `fis` equals to `7` (`si`), or vice versa. This also applies to passages written in E minor.
  - Major scales are written using the pitch `1` (`do`) as the root, for example: `1=F♯` or `1=f♯` for F♯ major, and `1=E♭` or `1=e♭` for E♭ major.
    Minor scales are written using the pitch `6` (`la`) as the root, for example: `6=F` or `6=f` for F minor, and `6=G` or `6=g` for G minor.
  - Key accidental suffixes (sharp and flat) can also be substituted with their equivalent text suffixes, e.g. `is` for sharp, and `es` for flat. For example: `1=Fis` equals to `1=F♯`, and `1=Es` equals to `1=E♭`.<br>
    (Note: To Indonesian people, `Es` or `es` is more commonly used than `Ees` or `ees`, even though, in my opinion, this is inconsistent.)
  - There are many cases in which minor keys are written in their equivalent major keys, such as `1=B♭` or `1=Bes` for `6=G`, and `1=A♭` or `1=As` for `6=F`.<br>
    (Note: Similar to the case of `es`, `As` or `as` is more commonly used than `Aes` or `aes`.)
{: .fs-5 }

## Further Reading
{: .no_toc }
- [Solmization](https://en.wikipedia.org/wiki/Solmization)
- [Solfège](https://en.wikipedia.org/wiki/Solfège)
- [Numbered musical notation](https://en.wikipedia.org/wiki/Numbered_musical_notation)
{: .fs-5 }
