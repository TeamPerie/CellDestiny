<br> 

<p align="right" width="100%">
    <img width="10%" src="https://github.com/TeamPerie/CellDestiny/blob/main/images/logoCurie.png">
</p>


# CellDestiny: An RShiny application for the visualization and analysis of single cell lineage-tracing data

<br> 

<p align="center" width="100%">
    <img width="40%" src="https://github.com/TeamPerie/CellDestiny/blob/main/images/logoCelldestiny.jpg">
</p>

## AIM :

CellDestiny is a user-friendly RShiny application that allows you to easily visualize and analyze single cell lineage-tracing datasets (lentiviral barcoding, integration site gene therapy, CRISPR gene therapy, etc.).

CellDestiny comprises two distinct modules: 

1. Visualisation of Quality Control Parameters 
2. Data Visualisation and Analysis

The **QC** module enables the user to check the consistency of technical replicates and to check for repeat-use barcodes across individuals. 

The **analysis** module enables the user to explore the data through several graphing options to assess clonal diversity, clone-size distributions as well as barcode sharing.

After plotting, images and their corresponding matrices can be downloaded such that statistical analysis of the data can be performed using external software packages such as R, and Prism.

## HOW TO USE

There are 3 ways to use CellDestiny. The first one is to use the **package** in Rstudio, the second one is to **launch the shiny app from the package** in Rstudio. The third one is to use the **web application**. 

1) Package installation in Rstudio 

Previous to its installation, you'll need all packages listed in [DESCRIPTION](https://github.com/TeamPerie/CellDestiny/blob/main/DESCRIPTION) file ("Imports" section) installed and an R version>= 4.1.0

  ***
    library(devtools)
    devtools::install_github("TeamPerie/CellDestiny", quiet = TRUE)
  ***
  
Make sure you have internet access. 

You can then:

* use functions from the package to explore your data like done in [UserManual](https://github.com/TeamPerie/CellDestiny/tree/main/UserManual/package) directory. 
 
2) Rshiny app from Rstudio

* first install install the package as in 1)

* launch the user friendly interface as follow :

        Launch_myApp()
  

3) Web application

Otherwise, you can use the web application by clicking https://perie-team.shinyapps.io/CellDestiny/.

## INPUTS FORMAT

### Sample names and metadata

To use the app, you need two input files:
1. the **count matrix** with samples in columns and cell identifier (i.e. barcodes) in rows
2. the corresponding **metadata**, listing all sample **variables** used in sample **names**

Good news ! You can use the script [create_metadata.Rmd](https://github.com/TeamPerie/HadjAbed-et-al._2022/blob/main/code_for_figures/GeneTherapy/ANALYSIS_matrices/scripts/1.create_metadata.Rmd) from [HadjAbed-et-al._2022](https://github.com/TeamPerie/HadjAbed-et-al._2022) article to generate your metadata file. You just need to adapt the "fill parameters" section to your data. 

---
<p align="center" width="100%">
    <img src="https://github.com/TeamPerie/CellDestiny/blob/main/images/meta_script.png">
</p>
---

#### Example : 

To give an example we imagine an experiment with 4 samples in total. The samples comprise 2 cell types (B and T lymphocytes) and 2 treatment conditions (+/- interferon alpha). 

Sample names can be provided in the following format, using underscores to separate words (<=> variables) :

  ***
    sample 1: LT_IFN
    sample 2: LT_noIFN
    sample 3: LB_IFN
    sample 4: LB_noIFN
  ***

The corresponding metadata file for these samples must then be provided in the following format :  

  ***
    cellType | treatment
    ---------------------
    LT       | IFN
    LB       | noIFN
  ***

With variables as column names with their listed values.

What is important here is that:
1. all columns must contain the **list of all values** (i.e. LT, LB) of variables (i.e. cellType) used in your sample names. The **Ordering** of metadata **columns** must match the ordering of your **sample variables**. 
2. all values have to be written in the **exact same way** than in the sample names names (i.e. be careful when using uppercase lettering and avoid white spaces within sample names)
3. an underscore (**"_"**) has to be used **only as a variable name separator** and must not be used in value names

### QC versus analysis matrices

QC matrix and its corresponding metadata must contain technical duplicate information. In the analysis module technical replicates should be merged prior (by summing or averaging the values between replicates) to uploading.

#### Example : 
Using the same example experiment as above, here is an example of permitted QC matrix sample name:
  ***
    sample 1 duplicate 1: LT_IFN_a
    sample 1 duplicate 2: LT_IFN_b
    sample 2 duplicate 1: LT_noIFN_a
    sample 2 duplicate 2: LT_noIFN_b
    sample 3 duplicate 1: LB_IFN_a
    sample 3 duplicate 2: LB_IFN_b
    sample 4 duplicate 1: LB_noIFN_a
    sample 4 duplicate 2: LB_noIFN_b
  ***

And its metadata:
  ***
    cellType | treatment | duplicate
    --------------------------------
    LT       | IFN       | a
    LB       | noIFN     | b
  ***

Inputs matrices examples can be found in this directory [testData/LentiviralBarcodingData/](https://github.com/Louisahadj/CellDestiny/tree/master/testData/LentiviralBarcodingData). 

## USER MANUAL

To have an overview of CellDestiny functionalities, check out [User Manual](https://github.com/TeamPerie/CellDestiny/tree/main/UserManual) directory. You'll find all possible QC, analysis and graphs that CellDestiny proposes. 

1) For package users

Read [package/ subdirectory](https://github.com/TeamPerie/CellDestiny/tree/main/UserManual/package) pdf files or re-run Rmd files to practice.

2) For app users

Read [app/ subdirectory](https://github.com/TeamPerie/CellDestiny/tree/main/UserManual/app) pdf file.

To play with the web app "test dataset", click on the [link](https://perie-team.shinyapps.io/CellDestiny/) to open the web application. 

In it, you see on the left a menu with the two parts: QC and Analysis. Both of them have their respective "Load data" sub-parts where you can load your matrix and metadata files. 

---
<p align="center" width="100%">
    <img src="https://github.com/TeamPerie/CellDestiny/blob/main/images/Load_data.png">
</p>
---


## Article describing CellDestiny

The article describing CellDestiny has been published here (to update when the paper is accepted) and the Github repository linked to the article is accesible here: https://github.com/TeamPerie/HadjAbed-et-al._2022.

## Authors

Please do not hesitate to post an issue or contact the authors :

Louisa Hadj Abed : louisa.hadj-abed@curie.fr

Leïla Perié : leila.perie@curie.fr

Jason Cosgrove : jason.cosgrove@curie.fr

### LICENSE

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.


