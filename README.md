# rtransparent

<div align="justify">
  
## Overview

`rtransparent` is an R package that automatically identifies and extracts indicators of transparency (data availability, code availability, statements of conflicts of interest, statements of funding, statements of protocol registration) from TXT and PMC XML versions of published research articles in biomedicine.


## Authors

Stylianos (Stelios) Serghiou (sstelios@stanford.edu). This package also utilizes functions from the `oddpub` package of Nico Riedel (nico.riedel@bihealth.de).


## Publication

`rtransparent` was validated and subsequently used to extract indicators of transparency from the entire open access literature from PubMed Central. This work is available as a preprint on bioRxiv at: https://www.biorxiv.org/content/10.1101/2020.10.30.361618v1.


## Installation

First, install the `oddpub` package.

```r
devtools::install_github("quest-bih/oddpub")
```

Then, install the `rtransparent` package. Note that you need to use the argument
`build_vignettes = T` to render the vignette, otherwise this will not be
accessible.

```r
devtools::install_github("serghiou/rtransparent", build_vignettes = T)
```


## Usage

The best way to learn how to use this package is to inspect the vignette. This
can be done by installing the package as explained above and then using the
command `vignette("rtransparent")`. This package uses the following naming 
convention: functions that work with TXT files extracted from PDF files do not 
end in `_pmc`. Functions that work with XML files from PubMed Central (PMC) end 
in `_pmc`. To run all functions on PMC use the `rt_all_pmc` function. To get all
meta-data from PMC articles, use the `rt_meta_pmc` function.


## Coming soon

1. Harmonization of code - all code will adopt the naming convention used in `rt_coi_pmc.R`.
2. Commenting - more comments will be added to explain non-trivial aspects of the code.
3. Vignette - a vignette explaining how to use the code is coming.


## Getting help

If you encounter a bug, please file an issue with a minimal reproducible example [here](https://github.com/serghiou/rtransparent/issues) and please Label it as a "bug" (option on the right of your window). For help on how to use the package, please file an issue with a clearly actionable question [here](https://github.com/serghiou/rtransparent/issues) and label it as "help wanted." For all other questions and discussion, please email the author.

</div>
