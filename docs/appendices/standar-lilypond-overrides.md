---
layout: default
title: "Modifications to Standar Lilypond Contexts"
nav_order: 1
parent: APPENDICES
permalink: /appendices/standar-lilypond-overrides.html
---

# Modifications to Standar Lilypond Contexts
{: .no_toc .fs-9 }

In my experience in building and using this library for music engraving, I have slightly modified the behavior of some Lilypond standard contexts and layout objects. These modifications are presented here as a reference for you in engraving your _solmisasi_ music.
{: .fs-5 }

## Translation Contexts
{: .no_toc .text-delta .fs-6 }

1. TOC
{:toc}

---

## **ChoirStaff**

_solmisasi-lily_ includes the following engravers into the [ChoirStaff](http://lilypond.org/doc/v2.20/Documentation/internals/choirstaff) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Bar_number_engraverr](http://lilypond.org/doc/v2.20/Documentation/internals/bar_005fnumber_005fengraver)

</div>

_solmisasi-lily_ adds the following acceptable [custom contexts](./custom-contexts.html) into the ChoirStaff context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [GlobalTempo](./custom-contexts.html#globaltempo)
- [SolmisasiStaff](./custom-contexts.html#solmisasistaff)

</div>

---

## **GrandStaff**

_solmisasi-lily_ adds the following acceptable [custom contexts](./custom-contexts.html) into the [GrandStaff](http://lilypond.org/doc/v2.20/Documentation/internals/grandstaff) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [SolmisasiChoirStaff](./custom-contexts.html#solmisasichoirstaff)
- [SolmisasiLyrics](./custom-contexts.html#solmisasilyrics)
- [SolmisasiStaff](./custom-contexts.html#solmisasistaff)
- [SolmisasiTimeAndKeySignature](./custom-contexts.html#solmisasitimeandkeysignature)
- [SolmisasiVoice](./custom-contexts.html#solmisasivoice)

</div>

---

## **PianoStaff**

_solmisasi-lily_ adds the following acceptable [custom contexts](./custom-contexts.html) into the [PianoStaff](http://lilypond.org/doc/v2.20/Documentation/internals/pianostaff) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [SolmisasiLyrics](./custom-contexts.html#solmisasilyrics)
- [SolmisasiStaff](./custom-contexts.html#solmisasistaff)
- [SolmisasiTimeAndKeySignature](./custom-contexts.html#solmisasitimeandkeysignature)
- [SolmisasiVoice](./custom-contexts.html#solmisasivoice)

</div>

---

## **Score**

_solmisasi-lily_ includes the following engravers into the [Score](http://lilypond.org/doc/v2.20/Documentation/internals/score) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- `\DbBars`<br>
  > Automatically creates double barlines in Staff whenever time signature and key signature changes found.
- [Span_arpeggio_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/span_005farpeggio_005fengraver)

</div>

_solmisasi-lily_ removes the following engravers from the [Score](http://lilypond.org/doc/v2.20/Documentation/internals/score) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Bar_number_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/bar_005fnumber_005fengraver)
- [Metronome_mark_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/metronome_005fmark_005fengraver)

</div>

_solmisasi-lily_ adds the following acceptable [custom contexts](./custom-contexts.html) into the [Score](http://lilypond.org/doc/v2.20/Documentation/internals/score) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [GlobalTempo](./custom-contexts.html#globaltempo)
- [SolmisasiChoirStaff](./custom-contexts.html#solmisasichoirstaff)
- [SolmisasiLyrics](./custom-contexts.html#solmisasilyrics)
- [SolmisasiStaff](./custom-contexts.html#solmisasistaff)
- [SolmisasiTimeAndKeySignature](./custom-contexts.html#solmisasitimeandkeysignature)
- [SolmisasiVoice](./custom-contexts.html#solmisasivoice)

</div>

_solmisasi-lily_ modifies/sets the following [Score](http://lilypond.org/doc/v2.20/Documentation/internals/score) context properties:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Set translator property `barNumberVisibility` to `all-bar-numbers-visible`
- Set translator property `majorSevenSymbol` to:
  ```
  \markup {
    \fontsize #3 {
      \lower #1 {
        "M" \super "7"
      }
    }
  }
  ```
- Set translator property `noChordSymbol` to `(make-bold-markup "(tacet)")`
- Set translator property `scriptDefinitions` to `solmisasi-script-alist`
- Set translator property `tieWaitForNote` to `#t`

</div>

_solmisasi-lily_ overrides the following settings of layout objects in [Score](http://lilypond.org/doc/v2.20/Documentation/internals/score) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `BarNumber.after-line-breaking` to `#f`
- Overrides `BarNumber.extra-offset` to `'(-0.3 . -0.1)`
- Overrides `BarNumber.font-shape` to `'italic`
- Overrides `BarNumber.font-size` to `-0.5`
- Overrides `BarNumber.padding` to `1`
- Overrides `SpanBar.layer` to `-5`
- Overrides `SystemStartBar.collapse-height` to `4`
- Overrides `SystemStartBar.thickness` to `1.9`
- Overrides `TextSpanner.dash-fraction` to `0.35`
- Overrides `TextSpanner.dash-period` to `2`
- Overrides `TimeSignature.style` to `'numbered`
- Overrides `VoltaBracket.font-size` to `-2.5`

</div>

---

## **Staff**

_solmisasi-lily_ overrides the following settings of layout objects in [Staff](http://lilypond.org/doc/v2.20/Documentation/internals/staff) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `InstrumentName.padding` to `0.8`
- Overrides `InstrumentName.self-alignment-X` to `RIGHT`
- Overrides `VerticalAxisGroup.remove-empty` to `#t`
- Overrides `VerticalAxisGroup.remove-first` to `#t`

</div>

---

## **StaffGroup**

_solmisasi-lily_ adds the following acceptable [custom contexts](./custom-contexts.html) into the [StaffGroup](http://lilypond.org/doc/v2.20/Documentation/internals/staffgroup) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [SolmisasiLyrics](./custom-contexts.html#solmisasilyrics)
- [SolmisasiStaff](./custom-contexts.html#solmisasistaff)
- [SolmisasiTimeAndKeySignature](./custom-contexts.html#solmisasitimeandkeysignature)
- [SolmisasiVoice](./custom-contexts.html#solmisasivoice)

</div>

---

## **Voice**

_solmisasi-lily_ overrides the following settings of layout objects in [Voice](http://lilypond.org/doc/v2.20/Documentation/internals/voice) context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `DynamicLineSpanner.direction` to `UP`
- Overrides `DynamicLineSpanner.staff-padding` to `3.0`
- Overrides `DynamicLineSpanner.Y-extent` to `'(2.5 . -2.5)`
- Overrides `DynamicText.direction` to `UP`
- Overrides `Script.direction` to `UP`
- Overrides `TextScript.Y-extent` to `'(2.5 . -2.5)`
- Overrides `TupletNumber.font-size` to `-1`

</div>
