---
layout: default
title: EXTENSIONS
nav_order: 5
has_children: true
has_toc: false
permalink: /extensions/
---

# EXTENSIONS: Extending _solmisasi-lily_ Library
{: .no_toc .fs-9 }

It is very possible that this library can't do some stuffs users want to produce in simple ways, for example, the one described in this section: [Printing More Informations in the Header of _Solmisasi_ Score](../examples/advanced-2.html). I, or you, or other developers may add some extensions to this library.
{: .fs-5 }

## Table of Contents
{: .no_toc .text-delta .fs-6 }

1. TOC
{:toc}

---

## Available Extensions

An extension can be provided either as a _free_ product, or as a _paid_ one, by _anyone_. As the author of this library, I will always try to provide free extensions as many as possible. However, considering that some extended features are more complicated and require deeper knowledge in Lilypond/Scheme programming, there are possibilities that this kind of extensions will not be freely provided.
{: .fs-5 }

As of the latest _solmisasi-lily_ version ({{ site.github.latest_release.tag_name }}), here is the list of available extensions, published by me.
{: .fs-5 }

<style>
th:first-of-type, td:first-of-type {
    width: 40px !important;
    min-width: 20px !important;
    text-align: right !important;
}
</style>

| Nr. | Name | Details | Free? | Bundled? |
|:----|:-----|:--------|:------|:---------|
|  1. | `key-signature-summary`  | [<i class="fas fa-external-link-alt"></i> Read This](./extension-01-key-signature-summary.html) | <i class="fas fa-check"></i> | <i class="fas fa-check"></i> since v1.0.5.1-beta |
|  2. | `time-signature-summary` | [<i class="fas fa-external-link-alt"></i> Read This](./extension-02-time-signature-summary.html) | <i class="fas fa-check"></i> | <i class="fas fa-check"></i> since v1.0.5.1-beta |
|  3. | `simultaneous-divisi`    | [<i class="fas fa-external-link-alt"></i> Read This](./extension-03-simultaneous-divisi.html) | <i class="fas fa-check"></i> | <i class="fas fa-check"></i> since v1.0.6-beta   |

---

## Are Extensions Bundled in the Release Archive File?

Some extensions are bundled in the release archive file, others can be distributed separately. To see and download the updated extensions from this repo, please head over to this link: [<i class="fas fa-external-link-alt"></i> lib/extension]({{ site.github.repository_url }}/tree/master/lib/extension)
{: .fs-5 }

---

## About "Extensions"

Extensions mentioned here are basically Lilypond helper snippets to be included in the main music snippet after completely loading _solmisasi-lily_. They must be exist in the directory `lib/extension` inside the root directory of _solmisasi-lily_. They must follow this rule of file naming: `solmisasi-<name-of-extension>.ily`.
{: .fs-5 }

So, let's say there are two extension snippets named `solmisasi-extension-1.ily` and `solmisasi-extension-2.ily` inside the directory `lib/extension`. To use this extension, users have use [\withExtensions](../appendices/custom-functions.html#withExtensions) command like this:
{: .fs-5 }

```
\withExtensions #'(
  "extension-1"
  "extension-2"
)
```
{: .lilypond }

right after the `include "solmisasi.ily"` line.
{: .fs-5 }
