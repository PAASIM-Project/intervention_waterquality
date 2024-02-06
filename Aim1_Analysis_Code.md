---
title: "Aim 1 Analysis Code"
author: "Courtney Victor"
date: "started 05 February 2024; most recent edits 06 February 2024"
output:
  html_document:
    toc: true
    toc_depth: 4
    toc_float: true
    number_sections: true
    code_folding: hide
    theme: cosmo
    keep_md: true
  md_document:
    variant: markdown_github

---



# Introduction

**Aim 1**: to test how the provision of an improved piped water network impacts water access and quality. 

*Hypotheses*: I predict that a) individuals who live in neighborhoods with the improved piped water network and b) individuals who have a household connection to an improved water supply will have improved cwater quality and access compared to individuals who do not live in such a) neighborhoods or b) households.

The data for this analysis comes from the PAASIM “Pesquisa sobre o Acesso à Água e a Saúde Infantil em Moçambique” (PAASIM- Research on Access to Water and Child Health in Mozambique). The purpose of this project is to evaluate the impact of a new piped water network among informal settlements in the city of Beira using a matched control study design. A detailed description of the study protocol can be found here: https://bmjopen.bmj.com/content/13/3/e067341. The pre-specified analysis plan can be found here: https://osf.io/4rkn6/. 


```r
# CREATE PACKAGE LIST:
Packages <- c("tidyverse", "knitr", "kableExtra", "readxl", "evaluate") 

# LOAD PACKAGES:
lapply(Packages, library, character.only = TRUE)

# SUPPRESS UNHELPFUL `dplyr` MESSAGES: 
options(dplyr.summarise.inform = FALSE)
```

**Packages used:**

tidyverse, knitr, kableExtra, readxl, evaluate

# Data

### Exposure
In brief, our intervention will be defined in two ways: 
* (1) People living in neighborhoods with the improved piped water network and 
* (2) individuals who have a household connection to an improved water supply. 

![**Figure 1.** Intervention Definitions](Figure 1_Intervention Definitions.png)

### Outcome 
We have water quality and access data from 548 households x 5 timepoints. Water quality will be defined by presence of *E. coli* (primary outcome), total coliforms, free and total chlorine, pressure, and enteropathogen data (from a subset of 100 households). We also have survey data that includes questions on satisfaction with water pressure, service, availability, and quality. Water access will be defined by the HWISE score, more information can be found here: https://www.ipr.northwestern.edu/wise-scales/measure-water-insecurity/. 


```r
# Remove the `Packages` variable from your environment
rm(Packages)

data <- read_excel("../../../../../../OneDrive-SharedLibraries-EmoryUniversity/Levy, Karen - 1. PAASIM/3. Data and Analysis/Data/Blinded dataset/Cleaned/PAASIM full cleaned data.xlsx")

# # doing some simple data exploration
# head(data)
# str(data)
# View(data)
```

## data formatting

ADD TEXT HERE ON WHAT I DO TO THE DATA BELOW AND WHY 


```r
# Create new dataset with important variables of interest using dplyr::select()

# Create false intervention variable (will be deleted after unblinding )

# cleanup
# (always clean up after yourself)
```

# Analysis Outline

I briefly describe an outline of the planned analyses below: 

* Table 1: Baseline differences in covariates of interest (neighborhood-level)
* Table 2: Baseline differences in covariates of interest (household-level)
* Table 3: Univariate analysis of water quality and access (neighborhood-level)
* Table 4: Univariate analysis of water quality and access (household-level)
* Table 5: Multivariate modeling of neighborhood-level intervention status and water quality and access 
* Table 6: Multivariate modeling of household-level intervention status and water quality and access 
* Figure 1: Visualization of model results (primary outcome *E. coli*)

## Data overview

In most of the empirically focused papers we write, the Results section starts off with a short subsection usually entitled something like "overview". Thus, it's good to have some code here that does basic calculations of things like how many samples you collected, how many species of plants and pollinators, how many interactions, how many seeds you counted, etc.

In this context, I think it is helpful to put **in-line code** into the **text** section of the report (i.e., not in a chunk) so that the results can be, well, contextualized. For example, we could report on the mean sepal width in the `iris` dataframe that is included with R:

* mean sepal width in the `iris` dataframe: 3.0573333

Sometimes it makes sense to include a code chunk (i.e., not just inline code) here, if needed to make more complex calculations; anything more than a couple of operations is typically best left to a code chunk, and can then be reported on in the text below the code chunk. The chunk below includes code from Loy et al.'s accelerated snowmelt and seed production analysis; the lines of codes included outside of the chunk (in the default Rmarkdown text) are repeated within the chunk (but commented out and thus not evaluated in the chunk). While this is by definition repetitive, it makes it easy for a reader of the Rmarkdown report to see how the calculations were conducted, without having to separately open the .Rmd file.


```r
# FIRST THING if altering: CHANGE CODE CHUNK HEADER TO REMOVE `eval = F`!

# this code is just an example of one way this could be done; change to best suit the purposes of your project.

# most of this is displayed in text below the code chunk

# Number of plants
# * `r nrow(masterdat)` plants

# Number of plants per treatment
plants.per.treat = masterdat %>% group_by(plot.treat) %>%
  summarise(no_rows = length(plot.treat))
    # + `r as.numeric(plants.per.treat[2,2])` plants in control plots
    # + `r as.numeric(plants.per.treat[1,2])` plants in snowmelt-accelerated plots

# Total number of seeds
# * `r as.integer(sum(masterdat$totalseeds))` total seeds
# "as.integer" makes it so that it does not display in scientific notation
```

## Analysis component 1

Now we move on to the actual data analyses...

* it is **really important** to give detailed context to each analysis chunk; for example:
    + what does this chunk do?
    + why this particular analysis?
    + what question is it answering?
    + what are the specifics involved, e.g. specification of random effects, other components like zero-inflation, etc.
* if you found information online that helped you with the analysis, I strongly suggest including links to it here; likewise, if you were following the analysis in a paper, I would include that here as well
* It is also **critically important** to edit this information as your analysis is changed / updated
* a minor point is to consider the use of `suppressWarnings()` if one or more operations in your code is generating warning messages that are not particularly helpful and which are cluttering up your report
    + BUT: if the warnings are helpful or potentially important, don't do that!
    + as noted above, you can also do this with `warnings = F` in the code chunk header

In addition, for many if not most analyses, we typically have to do a little bit of additional formatting that isn't in the "data formatting" section above. That is perfectly reasonable and often makes the report easier to follow logically. If you are doing that, however, please be sure to include a little bit of text outlining the formatting changes to be made here.


```r
# FIRST THING: CHANGE CODE CHUNK HEADER TO REMOVE `eval = F`!

# put any code related to data formatting for an analysis here

# this is where you would put your analysis code

# STREAMLINE MODEL RESULTS
# (for models including categorical factors with many levels...
# ...especially if there are interaction terms,
# we typically simplify model results using the
# `Anova` function from the `car` package)
aov.my.model = Anova(my.model)

# DISPLAY MODEL RESULTS
aov.my.model

# SAVE MODEL RESULTS (often but not always want to do this)
# this is in addition to displaying them in the Rmarkdown report
# for mixed-effects models, the `broom.mixed` package is very helpful for that:
out.aov.my.model = tidy(out.aov.my.model)
write.csv(out.aov.my.model, file = "results/my-model-results.csv")
# **IMPORTANT:** note the "results/" at the start of the file path, 
# which saves the output into the "results" folder in our project (key!)

# CLEANUP
# if there are temporary variables, etc. that were generated in this chunk,
# remove them here, e.g. the objects created by `broom.mixed`:
# (uncomment out the one line of code below, alter as needed)
rm(aov.my.model, out.aov.my.model)
# ...but don't remove your model objects yet, because you need to do model
# assessment on them (next code chunk)
```

* after each analysis, **it is really (really) important** to include some text about what the results mean
* discuss *directionality* of results, not just whether or not differences are significant; and also ideally effect sizes
* ideally for each analysis, plots and analyses are done near to one another
    + in contrast to doing all the analyses, then all the plots separately

### you can potentially break analyses down into subcomponents with headers

breaking an analysis down into smaller bits makes sense for many analyses... 


```r
# this is where you would put your analysis code

# see code chunk above for more specifics!!

# cleanup
# (always clean up after yourself)
```

...but remember for each of those bits to **put some text here** to give some context about the results!

### model validation, analysis 1

ALWAYS include data validation: do your data meet the assumptions of the models you are running? It's good to include just a bit of text here about your validation, but typically it is very brief.

If you ran an analysis two or more alternative ways, you may wish to split the validation component into multiple chunks. Remember to include some explanatory text, usually before and *always* after each chunk, even if very brief.


```r
# FIRST THING: CHANGE CODE CHUNK HEADER TO REMOVE `eval = F`!

# this is where you would put your code for model validation / assessment

# (for GLMMs, we typically use the `DHARMa` package for this, e.g.
# replace "my.model" with the name of the model you are assessing
# and probably change the name "sim.out.my.model" to something more descriptive)
# simulate residuals using DHARMa:
sim.out.my.model <- simulateResiduals(fittedModel = my.model, plot = F)
plot(sim.out.my.model) # plot residual plots
testDispersion(sim.out.my.model, plot = F) # print dispersion results
testZeroInflation(sim.out.my.model, plot = F) # print zero-inflation results

# CLEANUP
# again, remove anything that isn't subsequently used in the analysis
rm(sim.out.my.model)
```

as always, remember for each of those bits to **put some text here** to give some context about the validation results! Just showing a validation plot without any context is unhelpful. Tell the reader / your future self *why* your plot (or other result) is consistent (or not!) with model assumptions.

### plots analysis 1

* I strongly prefer to have plots associated with each analysis, near each analysis in the Rmarkdown document (as opposed, e.g., to putting all of the analyses together at the end)
    + I think it makes it easier to read / interpret the analysis report, but also makes it easier to code.
* give just a bit of brief context for each plot, before the code chunk in which you execute the plot
* when saving plots, **be sure to save them in the `plots` subfolder** (see code in the chunk below)

**saving plots and** `.gitignore`

* this project has been set up with the `plots/` subfolder included in `.gitignore`
* thus, any plots you save as part of the project (assuming you save them in the `plots/` subfolder, as you always should!) will be saved on your local computer, but not on GitHub
    + the rationale behind this is that Git does not do well with versioning binary files like image files / .pdfs / Word & Excel docs; thus it is re-writing the *entire* file every time it gets updated, even with a tiny change
* you should display at least the key plots in your .Rmarkdown report, so they will be viewable by anyone checking out the project
    + if anyone wants to generate the exact saved plots (i.e. the graphics files) that are in the `plots/` subfolder, the code should be set up to be reproducible so that should be straightforward



```r
# FIRST THING: CHANGE CODE CHUNK HEADER TO REMOVE `eval = F`!

# any data preparation / summarization needed for plotting
# code below is just a placeholder!
myplotdata = mydata %>% group_by(mygroupingvar1, mygroupingvar1) %>% 
  summarize(mean = mean(), stdev = sd()...) 

# create the actual plot (almost always using `ggplot2`)
myplot = ggplot(myplotdata, ...)

# DISPLAY the plot in your Rmarkdown report 
# (usually just a single line of code, 
# the name of the ggplot object you created above)
myplot

# SAVE PLOT
# example, but key thing is the `plots/` prefix in the file path
ggsave(myplot, file = "plots/myplot.pdf", width = 5, height = 3)

# # CLEANUP
# # remember to also clean up any objects such as data summarizations
# # set up for your plotting, e.g.:
rm(myplotdata, myplot)
```

you may wish to include some brief text here, e.g., "this plot shows the negative overall effect of potassium cyanide exposure on bee survival, underscoring our statistical results"

## Analysis 2

Repeat as needed for each analysis component (analysis 2, 3...). remember to include:

* analysis chunk(s)---that's the easy part
* model validation
* plots associated with each analysis

These should all occur together, to keep the overall analysis report easy to read and understand.

**IMPORTANT**---always remember to check that every code chunk has a unique name, otherwise your Rmarkdown report will not knit. This is particularly important when copying and pasting code chunks.

# Session Info and Package Loading Messages

`sessionInfo` is key (though minimally) for reproducibility---replicate the code chunk below exactly

before `sessionInfo` we print the messages associated with package loading, using the `replay` function from the `evaluate` package. We do this at the end to make the report more readable, such that readers do not have to wade through long lists of (typically) not-very-helpful messages. But for troubleshooting those messages can be important, so they are listed here.


```r
sessionInfo()
```

```
## R version 4.2.2 (2022-10-31)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS 14.3
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] evaluate_0.23    readxl_1.4.3     kableExtra_1.4.0 knitr_1.45      
##  [5] lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
##  [9] purrr_1.0.2      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1    
## [13] ggplot2_3.4.4    tidyverse_2.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] cellranger_1.1.0  bslib_0.6.1       compiler_4.2.2    pillar_1.9.0     
##  [5] jquerylib_0.1.4   tools_4.2.2       digest_0.6.34     viridisLite_0.4.2
##  [9] timechange_0.3.0  jsonlite_1.8.8    lifecycle_1.0.4   gtable_0.3.4     
## [13] pkgconfig_2.0.3   rlang_1.1.3       cli_3.6.2         rstudioapi_0.15.0
## [17] yaml_2.3.8        xfun_0.41         fastmap_1.1.1     xml2_1.3.6       
## [21] withr_3.0.0       systemfonts_1.0.5 hms_1.1.3         generics_0.1.3   
## [25] sass_0.4.8        vctrs_0.6.5       grid_4.2.2        tidyselect_1.2.0 
## [29] svglite_2.1.3     glue_1.7.0        R6_2.5.1          fansi_1.0.6      
## [33] rmarkdown_2.25    tzdb_0.4.0        magrittr_2.0.3    scales_1.3.0     
## [37] htmltools_0.5.7   colorspace_2.1-0  utf8_1.2.4        stringi_1.8.3    
## [41] munsell_0.5.0     cachem_1.0.8
```

