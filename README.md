# Teaching Data Science to Undergraduate and Graduate Students in Biology using R, RStudio and Learnr: Analysis of Three Years Data Including COVID-19 Pandemic Lockdowns

Guyliann Engels (1), Philippe Grosjean (1) & Frédérique Artus (2)

(1) Numerical Ecology Department, Complexys and InforTech Institutes, University of Mons, Belgium
(2) Pedagogical Support and Quality Assurance Department, University of Mons, Belgium


We examine the impact of transforming courses in biostatistics into three data science courses for a curriculum in biology at the University of Mons, Belgium. The study spans over three complete academic years. A blended learning and flipped classroom approaches were adopted, with an emphasis on project-based biological data analysis. Four successive types of exercises of increasing difficulties are proposed to the students. Tutorials written with the R package learnr are identified as a critical step to transition between theory and application of the concepts. Cognitive workload to complete the learnr tutorials is measured for the three courses and is only lower for the last course, suggesting students need a long time to get used to their software environment (R, RStudio and git). A comparison between final summative assessment and grading of applied biological data analysis (projects) exhibits a low correlation. It suggests that the final exam does not assess practical skills very well. The final exam was dropped at the benefit of an ongoing assessment. Data relative to students' activity, collected primarily for the ongoing assessment, are also used to establish students profiles according to their learning strategies. Several suboptimal strategies are observed and discussed. Finally, the timing of students contributions, and the efficiency of teacher-learner interactions related to these contributions are analyzed before, during and after mandatory distance learning due to covid-19 lockdown. A lag phase was visible at the beginning of the first lockdown, but the work of the students was not markedly affected during the second lockdown period that lasted much longer.

The aim of this project is to answer the following questions:

- How do the student perceive the approach of the course and the tools used? In particular, what is the general emotional state of the students? What is the workload of the learnr tutorials (NASA-TLX)?

- Are grades obtained by the students at a final exam correlated to their recorded activity during the course (projects)?

- Are there different student profiles that can be identified from their activities? If yes, how these profiles correlate with their grade?

- What is the temporal progression of the students, and the intensity of support required during COVID lockdown periods? Did it impact students' productivity?


## Workflow

We will make the data and the analyses public, most probably via Zenodo. In the meantime, the anonymized data are downloadable from <https://filedn.com/lzGVgfOGxb6mHFQcRn9ueUb/sdd_data.zip>. Raw data are processed with these steps:

1. Data from our MongoDB database are anonymized and saved into `.csv` files. That step is not documented because it contains sensible personal data that we are not allowed to disclose. However, we made this pretreatment as minimal as possible, focusing mainly on the replacement of personal data with random identifiers for the students.

2. Anonymized data are processed to produce table of final results in the `\data` subdirectory in this repository.

3. Data from the `/data` subdirectory are used to run the analyses, and to create tables and figures in the manuscript.


## How to reproduce the analyses?

First, make sure you have R, RStudio & LaTeX installed on your computer. We used R 4.0.5 for these analyses. R can be installed from <https://cran.r-project.org>, RStudio Desktop is here: <https://www.rstudio.com/products/rstudio/>. The LaTeX installation depends on your system, see: <https://www.latex-project.org/get/>. For explanations about R Markdown documents, see: <https://support.rstudio.com/hc/en-us/articles/200552056-Using-Sweave-and-knitr>.

Once R and RStudio are installed, you will need to get additional R packages. You should run `R/install.R` to do so:

```
source("R/install.R")
```

You should be able to regenerate the manuscript `docs/teaching_data_science.Rmd` (step 3 above), or the `docs/supplemental_materials.Rmd` notebook just be knitting them (open the `.Rmd` file inside RStudio and click on the **knit** button there).

If you want to regenerate the tables in `/data`, you will have to run the three `R/data_preparationX.R` files. Before doing so, you will have to download the anonymized datasets from <https://filedn.com/lzGVgfOGxb6mHFQcRn9ueUb/sdd_data.zip>. Unzip this file somewhere. You should have three directories: `sdd_2018-2019`,  `sdd_2019-2020` & `sdd_2020-2021` with a `data`subdirectory and various `.csv` files inside (`users.csv`, `projects.csv`, ...). In the `R/data_preparationX.R` file, you will have to change line 8 and indicate the actual directory on your computer where you unzipped the anonymized data. At that point, running the code in these scripts should regenerate the tables in `/data`.

Of course, the code in the `.Rmd` and `.R` file can be examined in more details to understand the logic of the analyses done.
