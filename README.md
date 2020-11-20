# Instructions to run the code in this repository

This code was written to be executed within RStudio.

## Scientific scope

The code in this repository is intended to estimate anthropogenic carbon in the ocean based on the eMLR(C*) method.

## Sharing code across analysis

Background information about sharing code across analysis in this repository, can be found [here](https://jdblischak.github.io/workflowr/articles/wflow-07-common-code.html){target="_blank"} on the workflowr homepage.

## Using child documents

Code chunks that are used across several .Rmd files are located in /analysis/child. Following child documents are available:

- setup.Rmd: Defines global options, loads libraries, functions and auxillary files. To run .Rmd files manually, the code in this child document must be executed first (Click "Run all", or Strg+Alt+R). This refers only to documents downstream of read_World_Ocean_Atlas_2018.Rmd, because this is where most auxillary files are created.


## Using functions

Functions are stored in .R files located under /code.


## Unevaluated chunks

- 


A [workflowr][] project.

[workflowr]: https://github.com/jdblischak/workflowr
