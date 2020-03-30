---
layout: default
title: "Custom Pitch Names"
nav_order: 5
parent: APPENDICES
---

# Custom _Solmisasi_ Pitch Names
{: .no_toc .fs-9 }

This library also provides a custom language of pitch names, with the purpose of helping Indonesian, or other users in writing pitches in Lilypond, based on common spelling of pitches in Indonesia. Default Lilypond pitch names, which is set to `nederlands`, is also included in this language set. Therefore, by using _solmisasi_ language of pitch names, you can write pitches in both language.
{: .fs-5 }

This language of pitch names is accessible by using this command:
{: .fs-5 }

```
\language "solmisasi"
```
{: .fs-5 }

Insert that line of code before any music definitions in your snippets, and then you can write pitches in this custom language.
{: .fs-5 }

### List of _Solmisasi_ Pitch Names (excluding the ones from `nederlands` set)
{: .no_toc }

<style>
th:first-of-type, td:first-of-type,
th:nth-child(2), td:nth-child(2),
th:nth-child(3), td:nth-child(3) {
  width: 80px !important;
  min-width: 60px !important;
}
th:last-of-type, td:last-of-type {
  width: 120px !important;
  min-width: 100px !important;
}
</style>

<div class="code-example" markdown="1" style="max-width: 400px;">

| Letters | Octave   | Note   | Alter         |
|:--------|:--------:|:------:|:-------------:|
| `do`    |   `-1`   |   `0`  | `NATURAL (0)` |
| `do#`   |   `-1`   |   `0`  | `SHARP (1/2)` |
| `di`    |   `-1`   |   `0`  | `SHARP (1/2)` |
| `reb `  |   `-1`   |   `1`  | `FLAT (-1/2)` |
| `ra`    |   `-1`   |   `1`  | `FLAT (-1/2)` |
| `re`    |   `-1`   |   `1`  | `NATURAL (0)` |
| `ri`    |   `-1`   |   `1`  | `SHARP (1/2)` |
| `re#`   |   `-1`   |   `1`  | `SHARP (1/2)` |
| `mib`   |   `-1`   |   `2`  | `FLAT (-1/2)` |
| `ma`    |   `-1`   |   `2`  | `FLAT (-1/2)` |
| `mi`    |   `-1`   |   `2`  | `NATURAL (0)` |
| `fa`    |   `-1`   |   `3`  | `NATURAL (0)` |
| `fi`    |   `-1`   |   `3`  | `SHARP (1/2)` |
| `fa#`   |   `-1`   |   `3`  | `SHARP (1/2)` |
| `solb`  |   `-1`   |   `4`  | `FLAT (-1/2)` |
| `sal`   |   `-1`   |   `4`  | `FLAT (-1/2)` |
| `sol`   |   `-1`   |   `4`  | `NATURAL (0)` |
| `sel`   |   `-1`   |   `4`  | `SHARP (1/2)` |
| `sol#`  |   `-1`   |   `4`  | `SHARP (1/2)` |
| `lab`   |   `-1`   |   `5`  | `FLAT (-1/2)` |
| `le`    |   `-1`   |   `5`  | `FLAT (-1/2)` |
| `la`    |   `-1`   |   `5`  | `NATURAL (0)` |
| `la#`   |   `-1`   |   `5`  | `SHARP (1/2)` |
| `li`    |   `-1`   |   `5`  | `SHARP (1/2)` |
| `sib`   |   `-1`   |   `6`  | `FLAT (-1/2)` |
| `sa`    |   `-1`   |   `6`  | `FLAT (-1/2)` |
| `si`    |   `-1`   |   `6`  | `NATURAL (0)` |

</div>
