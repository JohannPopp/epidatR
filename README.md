# epidatR

This package reads [EpiData](https://www.epidata.dk/) epx-files directly into R. So far it contains one major function `read.EpiData()`. The other functions in this repository are helpers.

Compared to the allready existing package [epxToR](https://cran.r-project.org/package=epxToR), `read.EpiData()` not only transfers epx-files into an R-`data.frame`. Additionally, values defined as missing in EpiData will be set to `NA`, defined value and variable labels will be applied and variables will be converted to appropriate R-classes according to their field type in EpiData. 

If this conversion is not wanted or causes some trouble, the original raw codes can be transferred into the `data.frame` as `character`-variables, by setting the argument `convert="none"`.

If the epx-file consists of one data set only, the output will be a `data.frame`. If the epx-file contains a relational data base with multiple data sets, it will be a `list` of multiple `data.frame`s.

Variable labels, study information and information about the relation of data sets are given as attributes.

Helper functions are: 

* `epx.extract()` extracts the xml-like data from the epx-file.
* `epx.read()` transfers the extracted data into a `data.frame`.
* `epx.missing()` sets defined missings to `NA`.
* `epx.class()` applies value labels and variable classes.
* `epidatR.example()` returns the path of the example files in the attached package.


The function works fine with the example files "Beispielprojekt.epx", "marathon.epx", "sample.v3.epx" and "Clinical_Example.epx", that are stored under /inst/extdata. Further testing is needed and I would appreciate feed back.

The function can not jet read encrypted epx-files. I would need much more expertise in cryptography to implement this feature. Perhaps there are some experts to help me out. For experimentation I have added the files "sample.v3_password.epx" with simple (symetric) encryption (Password = "password") and "sample.v3_ExtendedAccess.epx" with asymetric encryption produeced by the Extended Access module (Login = "admin", Password = "password"). 

The package is not jet available from CRAN. You can install it from here, using the devtools-package.

```{r}
devtools::install_github("https://github.com/JohannPopp/epidatR")
```

## Still To Do List
| Date       | To do                   | Solved | Notes                       |
|------------|-------------------------|--------|-----------------------------|
| 2023-11-28 | Reading encrypted files |        | complicated                 |
| 2023-11-28 | Setting the time zone   | 2023-11-28 | generally set to "UTC"  |
| 2023-11-28 | Writing epx files       |        | lot of work. Do we need it? |


 <!-- badges: start -->
  [![R-CMD-check](https://github.com/JohannPopp/epidatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohannPopp/epidatR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


