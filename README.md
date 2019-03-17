# PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data

## About this package

This program is developed based on the Shiny framework, a set of R packages and a 
collection of scripts written by members of Junhyong Kim Lab at University of Pennsylvania. 
Its goal is to facilitate fast and interactive RNA-Seq data analysis and visualization. 
Current version of PIVOT supports routine RNA-Seq data analysis including normalization, 
differential expression analysis, dimension reduction, correlation analysis, clustering and 
classification. Users can complete workflows of DESeq2, monocle and scde package with
just a few button clicks. All analysis reports can be exported, and the program state can be
saved, loaded and shared.

  * See http://kim.bio.upenn.edu/software/pivot.shtml for more details.

## Installation

  * Main Program: Please copy and paste the following command to R console. 
  * Upgrading R and Rstudio to the latest version (R >= 3.4, Rstudio > 1.0.0) is strongly recommended. 

```
# Dependecies that needs to be manually installed.
# You may need to paste the following code line by line 
# and choose if previously installed packages should be updated (recommended).

install.packages("devtools") 
library("devtools")
install.packages("BiocManager")
BiocManager::install("BiocUpgrade") 
BiocManager::install("GO.db")
BiocManager::install("HSMMSingleCell")
BiocManager::install("org.Mm.eg.db")
BiocManager::install("org.Hs.eg.db")
BiocManager::install("DESeq2")
BiocManager::install("SingleCellExperiment")
BiocManager::install("scater")
BiocManager::install("monocle")
BiocManager::install("GenomeInfoDb")

# Install PIVOT
install_github("qinzhu/PIVOT")
BiocManager::install("BiocGenerics") # You need the latest BiocGenerics >=0.23.3
```
 * (Optional but strongly recommended):
   * For report generation, you need Pandoc: http://pandoc.org/installing.html
   * For PDF report generation, you need Latex: https://www.latex-project.org/get/
   * If you have 10x data output from Cell Ranger, please install Cell Ranger R Kit from https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/rkit
   to allow PIVOT to directly read in the data.

## Running PIVOT

  * To run PIVOT, in Rstudio console, use command 
```
library(PIVOT)
pivot()
```

## User manual

See here: https://rawgit.com/qinzhu/PIVOT/master/inst/app/www/manual_file.html 

Or download: https://kim.bio.upenn.edu/software/pivot/manual_file.html.zip

## Troubleshooting

 * URL 'http://xxx.tgz': status was '404 Not Found'
   * Call `chooseCRANmirror()` to select another CRAN mirror.
   
 * "Maximum DLL loaded error". Unfortunately current R only permits maximum of 100 loaded DLLs. This issue will be fixed with the release of the developmental version of R (See https://stackoverflow.com/questions/36974206/error-maximal-number-of-dlls-reached). 
 
 For now, we suggest only load necessary modules when launching PIVOT. If your analysis require entire workflow, consider adding the environmental variable "R_MAX_NUM_DLLS=150" to .Renviron file located at "/Library/Frameworks/R.framework/Resources/etc"(MacOs); or .bash_profile with "export R_MAX_NUM_DLLS=150" (Linux). 
  
 * 'SingleCellExperiment' package cannot be correctly installed
    * Please update your bioconductor to the latest version (>=3.6) and retry installation using the following command:
 
 ```
BiocManager::install("BiocUpgrade") 
BiocManager::install("SingleCellExperiment")
```
 
 * If you ran into any problems like 'SingleCellExperiment'，'SCESet' or 'pData', its likely that you have old scater installed. The new scater package changes all the grammar so you need to first remove the old package by calling `remove.packages("scater")` and reinstall the latest version by using `BiocManager::install("scater")`.
   
 * Dependency openssl configuration failed
   * Linux: Please install the latest libgdal-dev package (apt-get install libgdal-dev)
   * MacOS: Please install brew (https://brew.sh/) first, then in terminal `brew install openssl`. Then try install PIVOT again.
  
 * MacOS specific: you might need to install xcode developer tools if you encounter installation error such as 'missing xcrun'.
 
 To install, Open Terminal, and run the following:

`xcode-select --install`

 * If a dependency fails to install, try installing the package separately using `BiocManager::install` if it is from BioConductor or `install.packages()` if it is CRAN (if you are unsure, try one and if it fails, try the other). Some users found this was necessary for the BioConductor packages `GenomicAlignments` and `rtracklayer`. If the package still fails to install, you can try binaries from BioConductor/CRAN or package manager if on Linux. Some users found this was necessary for the CRAN package `nloptr`.
 
## Citation

* Zhu, Q., Fisher, S. A., Dueck, H., Middleton, S., Khaladkar, M., & Kim, J. (2018). PIVOT: platform for interactive analysis and visualization of transcriptomics data. BMC bioinformatics, 19(1), 6.

* For specific analysis, please check the citation listed in the module.



Qin Zhu

Junhyong Kim Lab

University of Pennsylvania

2015 - 2017
