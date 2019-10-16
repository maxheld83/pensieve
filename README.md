# Pensieve: An R package for the Scientific Study of Human Subjectivity.

<img src="https://github.com/maxheld83/pensieve/blob/master/logo.png?raw=true" align="right" height=140/>

<!-- badges: start -->
[![Actions Status](https://github.com/maxheld83/pensieve/workflows/CICD/badge.svg)](https://github.com/maxheld83/pensieve/actions)
[![codecov](https://codecov.io/gh/maxheld83/pensieve/branch/master/graph/badge.svg)](https://codecov.io/gh/maxheld83/pensieve)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/ghactions)](https://cran.r-project.org/package=ghactions)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](http://www.gnu.org/licenses/agpl-3.0)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/pensieve)](http://www.r-pkg.org/pkg/pensieve)
<!-- badges: end -->

> *The name 'Pensieve' is a homonym of 'pensive', meaning deeply, seriously thoughtful; but it also a pun, the 'sieve' part of the word alluding to the object's function of sorting meanings from a mass of thoughts or memories.*

-- [Joanne K. Rowling](https://www.pottermore.com/writing-by-jk-rowling/pensieve)

Q Methodology employs straightforward factor analysis available in many statistical packages, but it also calls for specific datasets, custom procedures and bespoke outputs not readily available in mainstream software.
`Pensieve` is a new open-source R package to support researchers in their study of human subjectivity, supplemented by a web frontend for easy access.
As a feature-rich, one-stop-shop, the software supports a Q study all the way from conception to publishing, including:

- Custom Q data objects, extending the R S3 object-oriented system, including robust data validation.
- Helpers to construct and sample a P and Q-Set.
- Tools to administer Q sorts, online and on paper.
- Factor extraction, including PCA and other algorithms.
- Factor retention, including a Q-specific, modified parallel analysis.
- Factor rotation, including both automatic, as well as by-hand manual methods.
- Factor scoring, including Q-orthodox and alternative methods.
- Interactive plots, aiding in the presentation and interpretation of Q methodological results.

The package adheres to best practices in (R) software development and reproducible research, including source control and dependency management, unit and integration testing as well as an extensive documentation.

Additional, planned features are discussed, feedback and contributions are invited.


## Installation

To install, run:

```r
remotes::install_github("maxheld83/pensieve")
```
