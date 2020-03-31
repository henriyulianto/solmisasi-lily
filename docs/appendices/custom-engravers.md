---
layout: default
title: "Custom Engravers"
nav_order: 3
parent: APPENDICES
permalink: /appendices/custom-engravers.html
---

# Custom Engravers Implemented in _solmisasi-lily_ Library
{: .no_toc .fs-9 }

The following custom engravers are occupied by _solmisasi-lily_ to generate custom layout objects in their accepting contexts.
{: .fs-5 }

## Custom Engravers
{: .no_toc .text-delta .fs-6 }

1. TOC
{:toc}

---

## **Solmisasi_equivalence_key_engraver**

Engrave a _Solmisasi_-style of last pitch equivalency in the new key signature, when a key signature change is found.
{: .fs-5 }

---

## **Solmisasi_key_engraver**

Engrave a _Solmisasi_ key signature, in the form of `1=<major-key-pitch>` or `6=<minor-key-pitch>`.

---

## **Solmisasi_note_head_engraver**

Engrave a _Solmisasi_ pitch number replacing the standard notehead, based on the input `note-event` and current key signature.

---

## **Solmisasi_time_signature_engraver**

Engrave a _Solmisasi_ time signature.
