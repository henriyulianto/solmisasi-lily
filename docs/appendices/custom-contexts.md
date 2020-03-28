---
layout: default
title: "Custom Contexts"
nav_order: 2
parent: APPENDICES
---

# Custom Contexts Implemented in _solmisasi-lily_ Library
{: .no_toc .fs-9 }

The following custom translation contexts are created by _solmisasi-lily_ when it is included in a Lilypond file. They are based on standard Lilypond contexts, with some customized context properties and layout object settings in order to be used in _solmisasi_ music engraving.
{: .fs-5 }

## Custom Translation Contexts
{: .no_toc .text-delta .fs-6 }

1. TOC
{:toc}

---

## **GlobalTempo**

Type: `Engraver_group` (See [5.1.6 Defining new contexts](http://lilypond.org/doc/v2.18/Documentation/notation/defining-new-contexts))
{: .pt-5 .fs-5 }

This context is built from the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Axis_group_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/axis_005fgroup_005fengraver)
- [Bar_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/bar_005fengraver)
- [Grace_spacing_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/grace_005fspacing_005fengraver)
- [Metronome_mark_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/metronome_005fmark_005fengraver)
- [Text_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/text_005fengraver)
- [Text_spanner_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/text_005fspanner_005fengraver)
- [Time_signature_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/time_005fsignature_005fengraver)

</div>

This context sets the following property:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Sets translator property `keepAliveInterfaces` to
  ```
  '( metronome-mark-interface
     text-script-interface
     line-spanner-interface )
  ```

</div>

This context sets the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Sets `BarLine.bar-extent` to `'(-0.5 . 0.5)`
- Sets `BarLine.transparent` to `#t`
- Sets `MetronomeMark.break-align-symbols` to `#f`
- Sets `MetronomeMark.direction` to `UP`
- Sets `MetronomeMark.extra-spacing-width` to `'(+inf.0 . -inf.0)`
- Sets `MetronomeMark.Y-offset` to `(/ 12 11)`
- Sets `TextSpanner.direction` to `UP`
- Sets `TextSpanner.Y-offset` to `0`
- Sets `TimeSignature.break-align-symbol` to `#f`
- Sets `TimeSignature.stencil` to `#f`
- Sets `VerticalAxisGroup.nonstaff-nonstaff-spacing.padding` to `0.5`
- Sets `VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding` to `0.5`
- Sets `VerticalAxisGroup.remove-empty` to `#t`
- Sets `VerticalAxisGroup.remove-first` to `#f`
- Sets `VerticalAxisGroup.staff-affinity` to `DOWN`

</div>

---

## **SolmisasiChoirStaff**

Based on: [ChoirStaff](http://lilypond.org/doc/v2.20/Documentation/internals/choirstaff)
{: .pt-5 .fs-5 }

This context also accepts commands for the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [ChoirStaff](http://lilypond.org/doc/v2.20/Documentation/internals/choirstaff)

</div>

This context accepts the following contexts:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [SolmisasiLyrics](#solmisasilyrics)
- [SolmisasiStaff](#solmisasistaff)
- [SolmisasiVoice](#solmisasivoice)

</div>

This context denies the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Staff](http://lilypond.org/doc/v2.20/Documentation/internals/staff)

</div>

This context overrides the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `StaffGrouper.staff-staff-spacing` to
  ```
  `((basic-distance . 0.5)
    (minimum-distance . 0.25)
    (padding . 0.75))
  ```

</div>

---

## **SolmisasiLyrics**

Based on: [Lyrics](http://lilypond.org/doc/v2.20/Documentation/internals/lyrics)
{: .pt-5 .fs-5 }

This context also accepts commands for the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Lyrics](http://lilypond.org/doc/v2.20/Documentation/internals/lyrics)

</div>

This context is also built from the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Bar_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/bar_005fengraver)
- [Separating_line_group_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/separating_005fline_005fgroup_005fengraver)

</div>

This context overrides the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `BarLine.bar-extent` to `'(0 . 1.5)`
- Overrides `BarLine.transparent` to `#t`
- Overrides `BarLine.gap` to `1.0`
- Overrides `LyricText.layer` to `-2`
- Overrides `LyricText.whiteout` to `2`
- Overrides `LyricText.whiteout-style` to `'outline`
- Overrides `LyricText.word-space` to `1`
- Overrides `LyricExtender.layer` to `-4`
- Overrides `LyricHyphen.layer` to `-3`
- Overrides `LyricHyphen.minimum-distance` to `0.4`
- Overrides `VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding` to `0.75`
- Overrides `VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding` to `2.5`

</div>

---

## **SolmisasiStaff**

Based on: [Staff](http://lilypond.org/doc/v2.20/Documentation/internals/staff)
{: .pt-5 .fs-5 }

This context also accepts commands for the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Staff](http://lilypond.org/doc/v2.20/Documentation/internals/staff)

</div>

This context also accepts the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [SolmisasiVoice](../custom-contexts/#solmisasivoice)

</div>

This context is also built from the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Solmisasi_equivalence_key_engraver](../custom-engravers/#solmisasi_equivalence_key_engraver)
- [Solmisasi_note_head_engraver](../custom-engravers/#solmisasi_note_head_engraver)

</div>

This context removes the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Ledger_line_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/ledger_005fline_005fengraver)

</div>

This context omits the following layout objects:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Accidental](http://lilypond.org/doc/v2.20/Documentation/internals/accidental)
- [Clef](http://lilypond.org/doc/v2.20/Documentation/internals/clef)
- [ClefModifier](http://lilypond.org/doc/v2.20/Documentation/internals/clefmodifier)
- [Flag](http://lilypond.org/doc/v2.20/Documentation/internals/flag)
- [KeyCancellation](http://lilypond.org/doc/v2.20/Documentation/internals/keycancellation)
- [TimeSignature](http://lilypond.org/doc/v2.20/Documentation/internals/timesignature)

</div>

This context sets the following properties:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Sets translator property `explicitKeySignatureVisibility` to `begin-of-line-invisible`
- Sets custom _solmisasi_ translator property `male-vocal` to `#f`

</div>

This context overrides the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `BarLine.bar-extent` to `'(-2 . 2)`
- Overrides `BarLine.gap` to `0.8`
- Overrides `BarLine.space-alist.next-note` to `'(semi-fixed-space . 1.5)`
- Overrides `Beam.beam-thickness` to `0.15`
- Overrides `Beam.length-fraction` to `0.5`
- Overrides `Beam.transparent` to `#f`
- Overrides `Dots.staff-position` to `2`
- Overrides `InstrumentName.extra-offset` to `'(0 . -0.35)`
- Overrides `NoteHead.Y-offset` to `-0.65`
- Overrides `Slur.direction` to `DOWN`
- Overrides `StaffSymbol.line-count` to `5`
- Overrides `StaffSymbol.transparent` to `#t`
- Overrides `Stem.color` to `blue`
- Overrides `Stem.direction` to `UP`
- Overrides `Stem.length-fraction` to `0.8`
- Overrides `Stem.thickness` to `14`
- Overrides `Stem.transparent` to `#t`
- Overrides `Stem.X-offset` to `0.65`
- Overrides `TextScript.direction` to `UP`
- Overrides `TextSpanner.direction` to `UP`
- Overrides `Tie.details.height-limit` to `1.1`
- Overrides `TupletBracket.bracket-visibility` to `#t`
- Overrides `TupletBracket.direction` to `UP`
- Overrides `VerticalAxisGroup.default-staff-staff-spacing` to
  ```
  '((basic-distance . 0)
    (padding . 0.5))
  ```

</div>

---

## **SolmisasiTimeAndKeySignature**

Type: `Engraver_group` (See [5.1.6 Defining new contexts](http://lilypond.org/doc/v2.18/Documentation/notation/defining-new-contexts))
{: .pt-5 .fs-5 }

This context is built from the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Axis_group_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/axis_005fgroup_005fengraver)
- [Bar_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/bar_005fengraver)
- [Break_align_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/break_005falign_005fengraver)
- [Font_size_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/font_005fsize_005fengraver)
- [Key_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/key_005fengraver)
- [Pure_from_neighbor_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/pure_005ffrom_005fneighbor_005fengraver)
- [Separating_line_group_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/separating_005fline_005fgroup_005fengraver)
- [Solmisasi_key_engraver](../custom-engravers/#solmisasi_key_engraver)
- [Solmisasi_time_signature_engraver](../custom-engravers/#solmisasi_time_signature_engraver)
- [Time_signature_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/time_005fsignature_005fengraver)

</div>

This context omits the following layout objects:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Clef](http://lilypond.org/doc/v2.20/Documentation/internals/clef)
- [ClefModifier](http://lilypond.org/doc/v2.20/Documentation/internals/clefmodifier)
- [KeyCancellation](http://lilypond.org/doc/v2.20/Documentation/internals/keycancellation)
- [NoteHead](http://lilypond.org/doc/v2.20/Documentation/internals/notehead)
- [Stem](http://lilypond.org/doc/v2.20/Documentation/internals/stem)

</div>

This context sets the following properties:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Sets translator property `explicitKeySignatureVisibility` to `end-of-line-invisible`
- Sets translator property `keepAliveInterfaces` to
  ```
  '( key-signature-interface
     time-signature-interface )
  ```

</div>

This context overrides the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `BarLine.transparent` to `#t`
- Overrides `BarLine.bar-extent` to `'(-0.5 . 0.5)`
- Overrides `TimeSignature.self-alignment-X` to `CENTER`
- Overrides `KeySignature.self-alignment-X` to `CENTER`
- Overrides `TimeSignature.break-visibility` to `end-of-line-invisible`
- Overrides `VerticalAxisGroup.staff-affinity` to `DOWN`
- Overrides `VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding` to `0.5`
- Overrides `VerticalAxisGroup.nonstaff-nonstaff-spacing.padding` to `0.5`
- Overrides `VerticalAxisGroup.remove-empty` to `#t`
- Overrides `VerticalAxisGroup.remove-first` to `#f`

</div>

---

## **SolmisasiVoice**

Based on: [Voice](http://lilypond.org/doc/v2.20/Documentation/internals/voice)
{: .pt-5 .fs-5 }

This context also accepts commands for the following context:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Voice](http://lilypond.org/doc/v2.20/Documentation/internals/voice)

</div>

This context is also built from the following engravers:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- [Pitch_squash_engraver](http://lilypond.org/doc/v2.20/Documentation/internals/pitch_005fsquash_005fengraver)

</div>

This context sets the following properties:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Sets translator property `squashedPosition` to `0`

</div>

This context overrides the following settings to the layout objects within:
{: .pt-5 .fs-5 }

<div class="code-example" markdown="1">

- Overrides `DynamicLineSpanner.staff-padding` to `1.75`
- Overrides `DynamicLineSpanner.Y-extent` to `'(1.5 . -1.5)`
- Overrides `DynamicText.extra-offset` to `'(0 . -0.5)`
- Overrides `Hairpin.extra-offset` to `'(0 . -0.5)`
- Overrides `Hairpin.whiteout` to `1.5`
- Overrides `TextScript.Y-extent` to `'(1.5 . -1.5)`
- Overrides `Beam.extra-offset` to `'(0 . -0.2)`
- Overrides `TupletBracket.shorten-pair` to `'(0 . 0)`
- Overrides `TupletNumber.font-size` to `0`
- Overrides `Glissando.bound-details.left.Y` to `-1.2`
- Overrides `Glissando.bound-details.right.Y` to `0.8`
- Overrides `Glissando.bound-details.left.padding` to `0.2`
- Overrides `Glissando.bound-details.right.padding` to `0.2`
- Overrides `TieColumn.tie-configuration` to `'((-2.65 . -1))`
- Overrides `BreathingSign.text` to
  ```
  \markup {
    \translate #'(0 . -1.5)
    \musicglyph #"comma"
  }
  ```

</div>
