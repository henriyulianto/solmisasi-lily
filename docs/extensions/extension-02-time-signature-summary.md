---
layout: default
nav_order: 1
title: "Extension: \"time-signature-summary\""
parent: EXTENSIONS
ext_name: "time-signature-summary"
---

# {{ page.title }}
{: .no_toc .fs-6 }

<div class="code-example" markdown="1">

Full path (Linux/Mac): `<solmisasi-lily-root-dir>/lib/extension/solmisasi-{{ page.ext_name }}.ily`

Full path (Windows): `<solmisasi-lily-root-dir>\lib\extension\solmisasi-{{ page.ext_name }}.ily`

Description:
> This extension contains some necessary helper functions to summarize all time signature changes throughout the given music. The time signature summary is written to a custom header property.

Main function(s):
> [\updateTimeSignatureSummary](../../appendices/custom-functions/#updateTimeSignatureSummary) [void] - _music_ (music) _property-name_ (symbol)

Example Usage:
```
% Variable 'music' has to be defined already
% For example:
% music = \solmisasiMusic \mus

\updateTimeSignatureSummary \music #'time-signature-summary

\header {
  meter = \markup {
    \vspace #1
    \bold {
      \override #'(baseline-skip . 2.2)
      \override #'(line-width . 40)
      \wordwrap-field #'header:time-signature-summary
    }
  }
}
```
{: .lilypond }

</div>
