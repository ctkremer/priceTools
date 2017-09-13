
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

This package provides a suite of tools for exploring how differences in ecosystem function between pairs of communities relates to differences in their species richness and composition. It focuses on applications of the Price equation to ecosystem ecology (following Fox & Kerr 2012), and additional advances and graphical approaches proposed by Bannar-Martin et al. (in review). These \[R\] tools emerged from an sDiv working group, sCAFE, in 2015-16.

Installation
------------

This package does not yet reside on CRAN; in the mean time, it can be downloaded and installed directly from github.

``` r
#First, install the 'devtools' package:
install.packages("devtools")

#Then run the following command:
devtools::install_github("ctkremer/priceTools")
```

Usage
-----

(Insert an example that shows how to use the package to solve a simple problem)

Major functions
---------------

This package provides tools for:

1.  Automating the pairwise comparison of sets of communitie using the Price equation, while accounting for additional structure (such as treatments) that distinguish groups of communities.

2.  Graphically summarizing the results of Price comparisons among communities as vector diagrams.

3.  Performing statistical/significance tests on the components produced by Price comparisons.
