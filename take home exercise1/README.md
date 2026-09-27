# Take-home Exercise 1 — R + Quarto workspace

**Draft: analysis and student interpretation pending. Do not submit these templates.**

This folder uses the same R-script and `knitr::read_chunk()` pattern as Exercise
2A. It contains the report, revealjs presentation, learning guide, R workspace,
and data provenance. All analytical work will be written in R by the student.

## Start in RStudio

Open `ISSS626-GAA.Rproj` in the website root and run:

```r
source("take home exercise1/R/check-environment.R")
source("take home exercise1/R/download-data.R")
source("take home exercise1/R/render.R")
```

The first two scripts check setup and acquire/verify original inputs. The last
renders the website, reusing available frozen results, without executing unfinished
analytical sections. This updates navigation on the existing pages as well. For
older exercises with no frozen results, use their download scripts documented in
the main project README before a full build.
Install any missing workspace dependencies (`knitr`, `rmarkdown`, `jsonlite`,
`digest`) through RStudio. Method-dependent packages are listed by the checker.

## Where to work

- `index.qmd`: exercise landing page and completion status.
- `technical-report.qmd`: report narrative and R chunks.
- `executive-summary.qmd`: revealjs deck, with 10 content slides plus cover/contents.
- `learning-guide.qmd`: concepts, study-design worksheet, and stage outputs.
- `R/prepare.R`: labelled student preparation sections.
- `R/analysis.R`: labelled student analysis sections.
- `data/metadata/`: source metadata and checksums.
- `data/raw/`: local original files, ignored by Git.
- `data/derived/`: student intermediates, ignored by Git.
- `figures/`: completed R-generated figures for reuse in the report/slides.

The report reads the labelled R sections directly. Replace each explicit pending
`stop()` with your own work and remove the corresponding chunk's `eval: false`
when it is ready. Enable chunks in report order. Do not copy rendered output into
the report as a substitute for running the code that produces it.

## Final publication checklist

The accepted plan reserves final publication for completed student analysis.
Development is on `codex/take-home-exercise1`; the production site uses `main`.

1. Complete and run the student R preparation and analysis; review the outputs.
2. Complete interpretations (at most 150 words per major visual), the planning
   discussion (at most 500 words), and the summary (at most 10 content slides).
3. Include the analytical `sessionInfo()`, execution order, seeds, source records,
   and an accurate AI-use declaration. Check all references and data licenses.
4. Remove pending text, draft labels, and exercise `noindex` metadata.
5. Change exercise GitHub links from the development branch to `main`.
6. Render the whole site from R using Quarto and the existing freezer when
   appropriate; explicitly rerun this report when external R scripts change.
7. Check every new route, navigation menu, figure, slide control, and old exercise.
8. Commit the source and `_site/` together, push the completed changes to `main`,
   and verify Netlify publishes that revision before submitting links to eLearn.

## Current assistance

AI prepared the document structures, conceptual learning guide, website
integration, source acquisition, and R setup/rendering helpers. It did not produce
the assessed analytical solution or interpretations. The unfinished analytical
sections remain explicit student tasks.
