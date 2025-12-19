
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HFSutils

Personal helper functions for statistical modeling, simulation, and
reporting in applied research.

This package collects small, reusable functions that I use across
projects to avoid code duplication and to keep analysis scripts
readable. Functions are documented but primarily designed for personal
use rather than as a general-purpose public API.

## Scope and philosophy

- Functions are **opinionated** and reflect my own workflows.
- APIs may change over time.
- Not all edge cases are handled.
- For published work, package versions should be **pinned** for
  reproducibility.

The package is organized by functionality rather than by project.

## Installation

You can install the development version of HFSutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hafsunde/HFSutils")
```

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

## Contents

The package currently includes helpers for:

### Simulation

- Exact control of empirical variance
- Adding noise to reach a target variance

### Formatting

- Numeric formatting for tables and figures
- Percent, integer, and scientific notation helpers

### Scale scoring

- Reverse coding of questionnaire items
- Missing-data tolerant score computation

### OpenMx utilities

- Extraction of estimates, standard errors, and Wald confidence
  intervals

Some helper functions are intentionally internal and accessed via
`HFSutils:::`.
