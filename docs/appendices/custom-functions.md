---
layout: default
title: "Custom Functions"
nav_order: 4
parent: APPENDICES
---

# Custom Functions Provided by _solmisasi-lily_ Library
{: .no_toc .fs-9 }

1. TOC
{:toc}

---

## Main Functions

<div class="code-example" markdown="1">

### Function: `\solmisasiMusic` [music] - _music_ (music)
{: .fs-5 }
> Parse and translate _music_ into a _solmisasi_-ready form of music.

</div>

<div class="code-example" markdown="1">

### Function: `\solmisasiLyric` [music] - _lyrics_ (music)
{: .fs-5 }
> (Experimental) Parse and translate _lyrics_ into a _solmisasi_-ready form of lyrics.

</div>

<div class="code-example" markdown="1">

### Function: `\withExtensions` [void] - _extension-list_ (list of string)
{: .fs-5 }
> Load/Include all extensions specified in _extension-list_.

</div>

---

## Helper Functions

<div class="code-example" markdown="1">

<a name="flexibleDo"></a>
### Function: `\flexibleDo` [music] - _k_ (pitch) _music_ (music)<br>
{: .fs-5 }
Available in _solmisasi-lily_ core library
> Returns a new music as if the given _music_ is written in _k_ major key.

Aliases:
- `\movableDo`
- `\doIs`
- `\doEqualsTo`
- `\doSamaDengan`

</div>

<div class="code-example" markdown="1">

<a name="flexibleLa"></a>
### Function: `\flexibleLa` [music] - _k_ (pitch) _music_ (music)<br>
{: .fs-5 }
Available in _solmisasi-lily_ core library
> Returns a new music as if the given _music_ is written in _k_ minor key.

Aliases:
- `\movableLa`
- `\laIs`
- `\laEqualsTo`
- `\laSamaDengan`

</div>

<div class="code-example" markdown="1">

<a name="silence"></a>
### Function: `\silence` [music] - _music_ (music)<br>
{: .fs-5 }
Available in _solmisasi-lily_ core library
> Returns a new music as a result of silencing (making skips of) the given _music_.

</div>

<div class="code-example" markdown="1">

<a name="updateKeySignatureSummary"></a>
### Function: `\updateKeySignatureSummary` [void] - _music_ (music) _property-name_ (symbol)<br>
{: .fs-5 }
Available in extension: [key-signature-summary](../../extensions/extension-01-key-signature-summary/)
> Summarizes all key signatures specified in _music_ as a text/string, and save it to a custom header property named _property-name_. _music_ has to be translated first with `\solmisasiMusic`. The custom header property _property-name_ can then be used in `\header` block by using a markup command `\property-name` or `\fromproperty #'header:property-name`.

</div>

<div class="code-example" markdown="1">

<a name="updateTimeSignatureSummary"></a>
### Function: `\updateTimeSignatureSummary` [void] - _music_ (music) _property-name_ (symbol)<br>
{: .fs-5 }
Available in extension: [time-signature-summary](../../extensions/extension-02-time-signature-summary/)
> Summarizes all time signatures specified in _music_ as a text/string, and save it to a custom header property named _property-name_. _music_ has to be translated first with `\solmisasiMusic`. The custom header property _property-name_ can then be used in `\header` block by using a markup command `\property-name` or `\fromproperty #'header:property-name`.

</div>

---

## Examples of usage

<div class="code-example" markdown="1">

### `\flexibleDo`, `\flexibleLa`, and `\silence`
{: .fs-5 }

<img src="../../../examples/helper-functions-demo.png" width="500px"/>

</div>
