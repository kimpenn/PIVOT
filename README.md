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
# dependecies that needs to be manually installed
install.packages("devtools")
library("devtools")
source("http://bioconductor.org/biocLite.R")  
biocLite("GO.db")
biocLite("HSMMSingleCell")
biocLite("org.Mm.eg.db")
biocLite("org.Hs.eg.db")
biocLite("DESeq2")

# Install PIVOT
install_github("qinzhu/PIVOT")
install_github("qinzhu/PIVOT.launcher")
```
 * (Optional but strongly recommended):
   * For report generation, you need Pandoc: http://pandoc.org/installing.html
   * For PDF report generation, you need Latex: https://www.latex-project.org/get/

## Running PIVOT

  * Note you MUST launch PIVOT with Rstudio.
  * To run PIVOT, in Rstudio console, use command 
```
library(PIVOT)
pivot()
```

* For advanced users, if you want to only load needed modules,

Then you can either use 
```
pivot_module()
```
which shows the available modules in PIVOT:

|ID|Module|
|---|---|
|1|PIVOT.analysis|
|2|DESeq2|
|3|edgeR|
|4|scde|
|5|monocle|
|6|PIVOT.network|
|7|caret|
|8|PIVOT.toolkit|

Then use `pivot(#ID_vector)` to launch selected modules, e.g., pivot(c(1,2,3)) to launch PIVOT with the base PIVOT module, DESeq2 and edgeR.

Alternatively, use
```
pivot_launcher()
```
to launch a window to directly pick modules or install required components.

![ScreenShot](https://github.com/qinzhu/PIVOT/tree/master/inst/app/manual/figures/launcher.png)

Press set module when ready, then launch PIVOT by calling `pivot`.

## User manual

See here: https://cdn.rawgit.com/qinzhu/PIVOT/60489858/inst/app/manual/manual_file.html.

## Troubleshooting

 * URL 'http://xxx.tgz': status was '404 Not Found'
   * Call `chooseCRANmirror()` to select another CRAN mirror.
   
 * Linux specific: Dependency openssl configuration failed
   * Please install the latest libgdal-dev package (apt-get install libgdal-dev)
   
## Citation

* Qin Zhu, Stephen A Fisher, Hannah Dueck, Sarah Middleton, Mugdha Khaladkar, Junhyong Kim. PIVOT: Platform for Interactive Analysis and Visualization of Transcriptomics Data (Preprint) bioRxiv 053348; doi: http://dx.doi.org/10.1101/053348

* For specific analysis, please check the citation listed in the module.



Qin Zhu

Junhyong Kim Lab

University of Pennsylvania

2015 - 2017
