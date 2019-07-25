# Statistical Learning

This repository is intended to provide a slideshow of classification models, with a focus on the statistical properties of each approach. Specifically, a wide variety of both linear and non-linear methods are adopted and the compared, ranging from **Linear Probability Model** and **Logistic Regression** to **Quadratic Discriminant Analysis** and **Generalised Additive Models**.

In order to do that, the wine quality dataset is considered. For more information, please refer to [https://archive.ics.uci.edu/ml/datasets/wine+quality](https://archive.ics.uci.edu/ml/datasets/wine+quality).

## Installation

Clone the repository where it is more convenient for you.

```r
git clone https://github.com/clissa/Statistical_Learning.git

```

Once the download is finished, a ready-to-use conda environment can be created just opening a Terminal (or the Anaconda Navigator if you are on Windows) and executing the following code:

```r
cd Statistical_Learning

### create conda environment
conda create -n <custom_r_env> r-essentials r-base
conda activate <custom_r_env>

### install base software
conda install -c r rstudio
conda install -c mdekstrand r-plotroc
```

Otherwise, you are just ready to start investigating the report or playing around with the R code.

## Usage
The repository contains both the ready-made statistical analysis and all of the R code used to obtain the results.

### Report
A thorough report is presented in the file *Original_report.html*, that describes in details all the steps of the analysis. A thorough argumentation of statistical properties of each approach is also dealt with, with particular attention to how to evaluate the various techniques and how to compare their performances. 
Furthermore, the report's plots are mostly interactive so to allow the user to explore the results autonomously and to dig into details of greater personal interest.

### Code
The code adopted to obtain the report is then available in the R scripts. They are organised in a modular structure so that each chapter is developed in a self-contained script. The numbering of each file establishes a clear execution pipeline ordering. 

The main script is *000_run_project.R* that executes the source code of the three parts of the analysis:
- **Data exploration:** contains all the preliminary data investigation and it is organised in the script *030_Exploratory.R*
- **Modelling:** performs all the training and evaluation of the proposed methods and it is hosted in the script *076_Modeling_Classification.R*
- **Reporting:** gather together the results of the analysis and the markdown files with the comments to produce a final report

Backtracing these scripts, it is then possible to get the source code that performs each bit of the analysis itself, from data visualisation to modelling. Also, it may be useful to explore how to exploit **R Markdown** for building a report in an efficient and (hopefully) appealing way.
