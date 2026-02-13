
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HFSutils

Personal helper functions for statistical modeling, simulation, and
reporting in applied research.

This package collects small, reusable functions that I use across
projects to avoid code duplication and to keep analysis scripts
readable. Functions are documented but primarily designed for personal
use rather than as a general-purpose public API.
## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(HFSutils)

SEtoCI(
  estimates = c(0.2, 0.4),
  std_errors = c(0.05, 0.05)
)
#>       Lower     Upper
#> 1 0.1020018 0.2979982
#> 2 0.3020018 0.4979982
```

## Function overview (for newcomers)

If you're new to this package, this list shows what each exported
function is for and when to use it.

### Confidence intervals and correlations

- `SEtoCI()` — Convert estimates + standard errors into confidence
  intervals; supports Fisher z back-transformation for correlations.
- `cor_fastCI()` — Compute a correlation and a quick Fisher z
  confidence interval from two vectors.

### Simulation helpers

- `exact_rnorm()` — Simulate normal data and (optionally) rescale so
  the sample SD exactly matches a target SD.
- `noise_filler()` — Add random noise to a variable when its variance
  is below 1, aiming for unit variance.

### Education recoding helpers

- `convert_edu_level_to_years()` — Recode NUS2000 education levels
  (0–8) to approximate years of education.
- `convert_edu_level_to_group()` — Recode NUS2000 education levels into
  broad education groups.

### Score construction

- `score_scale()` — Build scale scores from item-level data with
  optional reverse coding and missingness thresholds.

### Plot helper

- `HFS_theme()` — Apply a `ggplot2` theme with transparent background
  and customizable foreground color.

### Number formatting helpers

- `numformat()` — General numeric formatter with commas, decimal
  control, and optional leading-zero removal.
- `format_num()` — Force fixed decimal formatting (e.g., `1.20`).
- `format_int()` — Format integer-like values with thousands
  separators.
- `to_percent()` — Convert proportions to percent strings.
- `expSup()` — Format scientific notation as LaTeX-style exponent text.
