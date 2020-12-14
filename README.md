# rtransparent

<div align="justify">
  
## Overview

rtransparent is a package that you can use to automatically identify and extract indicators of transparency (data availability, code availability, statements of conflicts of interest, statements of funding, statements of protocol registration) from the TXT and PMC XML version of published research articles in biomedicine.


## Authors

Stylianos (Stelios) Serghiou (sstelios@stanford.edu). This package also utilizes functions from the `oddpub` package of Nico Riedel (nico.riedel@bihealth.de).


## Publication

`rtransparent` was validated and subsequently used to extract indicators of transparency from the entire open access literature from PubMed Central. This work is available as a preprint on bioRxiv at: https://www.biorxiv.org/content/10.1101/2020.10.30.361618v1.


## Installation

First, install the `oddpub` package.

```r
devtools::install_github("quest-bih/oddpub")
```

Then, install the `rtransparent` package.

```r
devtools::install_github("serghiou/rtransparent")
```


## Naming convention
Functions that work on txt files extracted from PDFs do not end in `_pmc`. Functions that work on XML files from NLM end in `_pmc`. To run all functions on PMC use the `rt_all_pmc` function. To get all meta-data from PMC articles, use the `rt_meta_pmc` function.


## Coming soon

1. Harmonization of code - all code will adopt the naming convention used in `rt_coi_pmc.R`.
2. Commenting - more comments will be added to explain non-trivial aspects of the code.
3. Vignette - a vignette explaining how to use the code is coming.


</div>
