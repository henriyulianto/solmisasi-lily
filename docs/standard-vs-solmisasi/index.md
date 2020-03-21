---
layout: default
nav_order: 2
title: Standard (Western) vs Solmisasi
---

People say: _"A picture is worth a thousand words"_. Let's bring up some pictures here to better show the differences (and implicitly, the equivalencies) between both notation systems.<br>
  Just take a seat, and relax!
{: .fs-6 }

Let's take a reference from what Wikipedia says: [Vocal range](https://en.wikipedia.org/wiki/Vocal_range). Please take a note that the ranges given below **are approximations** and **are not meant to be too rigidly applied**.<br>
{: .fs-5 }
There are 2 solmisasi sub-staves below the standard male vocal staves: _(Actual)_ and _(Real World)_. The pitch numbers on (Actual) are the 'actual' pitch numbers converted from the standard notation pitches, while the ones on (Real World) are the 'real-world' pitch numbers that the solmisasi system applied in vocal music. Using the same Lilypond music passages/sequences, it can be achieved by setting a custom staff context property named `male-vocal` to `##t` (true).
{: .fs-4 }

1. TOC
{:toc .fs-5 }

---

## Pitches in C Major
{: .text-delta .fs-6 }

### Sharp Mode
{: no_toc }

<details>
<summary>Click to show/hide</summary>
<img src="./comparison.png">
</details>

### Flat Mode
{: no_toc }

<details>
<summary>Click to show/hide</summary>
<img src="./comparison-in-flat.png">
</details>

---

## Pitches in Sharp Key Signatures
{: .text-delta .fs-6 }

All pitches shown below are written in **'sharp mode'**.
{: .fs-5 }

- ### G Major or E Minor (One Sharp)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-g-maj.png">
  </details>

- ### D Major of B Minor (Two Sharps)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-d-maj.png">
  </details>

- ### A Major or F♯ Minor (Three Sharps)

  In this key signature, there are 2 modes in writing notes in solmisasi system as described below.
  {: .fs-5 }

  #### Mode 1: Actual
  {: no_toc }

  The actual conversion from standard to solmisasi system is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-a-maj.png">
  </details>

  #### Mode 2: Common Alternative
  {: no_toc }

  However, it is a common practice that the music author define the `A3` pitch (first `A` below middle `C`, or `a` in default Lilypond pitch name syntax) as the natural `1` (`do`). This can be achieved by transposing the converted-to-solmisasi music up by one octave. So, this 'adjusted' conversion is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-a-maj-2.png">
  </details>

- ### E Major or C♯ Minor (Four Sharps)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-e-maj.png">
  </details>

- ### B Major or G♯ Minor (Five Sharps)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-b-maj.png">
  </details>

- ### F♯ Major or D♯ Minor (Six Sharps)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-fis-maj.png">
  </details>

- ### C♯ Major or A♯ Minor (Seven Sharps)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-cis-maj.png">
  </details>

---

## Pitches in Flat Key Signatures
{: .text-delta .fs-6 }

All pitches shown below are written in **'flat mode'**.
{: .fs-5 }

- ### F Major or D Minor (One Flat)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-f-maj.png">
  </details>

- ### B♭ Major (Two Flats)

  In this key signature, there are 2 modes in writing notes in solmisasi system as described below.
  {: .fs-5 }

  #### Mode 1: Actual
  {: no_toc }

  The actual conversion from standard to solmisasi system is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-bes-maj.png">
  </details>

  #### Mode 2: Common Alternative
  {: no_toc }

  Similar to the case of [A Major or F# Minor key](#a-major-or-f-minor-three-sharps), it is a common practice that the music author define the `B♭3` pitch (first `B♭` below middle `C`, or `bes` in default Lilypond pitch name syntax) as the natural `1` (`do`). This can be achieved by transposing the converted-to-solmisasi music up by one octave. So, this 'adjusted' conversion is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-bes-maj-2.png">
  </details>

- ### E♭ Major (Three Flats)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-es-maj.png">
  </details>

- ### A♭ Major (Four Flats)

  In this key signature, there are 2 modes in writing notes in solmisasi system as described below.
  {: .fs-5 }

  #### Mode 1: Actual
  {: no_toc }

  The actual conversion from standard to solmisasi system is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-aes-maj.png">
  </details>

  #### Mode 2: Common Alternative
  {: no_toc }

  Similar to the case of [A Major or F# Minor key](#a-major-or-f-minor-three-sharps), it is a common practice that the music author define the `A♭3` pitch (first `A♭` below middle `C`, or `aes` in default Lilypond pitch name syntax) as the natural `1` (`do`). This can be achieved by transposing the converted-to-solmisasi music up by one octave. So, this 'adjusted' conversion is shown below.
  {: .fs-5 }
  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-aes-maj-2.png">
  </details>

- ### D♭ Major (Five Flats)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-des-maj.png">
  </details>

- ### G♭ Major (Six Flats)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-ges-maj.png">
  </details>

- ### C♭ Major (Seven Flats)

  <details>
  <summary>Click to show/hide</summary>
  <img src="./comparison-ces-maj.png">
  </details>

---

## Preferred Pitch Numbers
{: .text-delta .fs-6 }

In most cases of writing score/sheet music in _solmisasi_ system and based on common practice, especially in Indonesia, regardless of the key signatures being used, there are some pitch numbers which users (singers, vocal groups, or choirs) doesn't like. Therefore, many music authors tend to avoid using these pitch numbers.<br>
The preferred solmisasi pitch numbers are listed below.
{: .fs-5 }
<img src="./preferred-pitch-numbers.png" width="400" />

The question is: _How do we choose the pitches to be used, if the 'original' converted pitches are not the preferred ones?_
{: .fs-5 }

The answer would be totally based on the skill, style, and technique that the author have and use in creating Lilypond scores. Someone could just replace the avoided pitches with the preferred ones. However, if I intend to have both outputs (standard and solmisasi), I would prefer using 'tags' in my Lilypond snippets to distinguish those notes using their enharmonic equivalencies.
{: .fs-5 }

So, what are you waiting for? Let's [get started!](../getting-started/)
{: .fs-7 }
