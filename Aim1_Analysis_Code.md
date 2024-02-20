---
title: "Aim 1 Analysis Code"
author: "Courtney Victor"
date: "started 05 February 2024; most recent edits 20 February 2024"
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
Packages <- c("tidyverse", "knitr", "kableExtra", "readxl", "evaluate", "boot", "table1", "broom", "flextable") 

# LOAD PACKAGES:
lapply(Packages, library, character.only = TRUE)

# SUPPRESS UNHELPFUL `dplyr` MESSAGES: 
options(dplyr.summarise.inform = FALSE)
```

**Packages used:**

tidyverse, knitr, kableExtra, readxl, evaluate, boot, table1, broom, flextable

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

## Data Formatting

I filtered the master dataset to only the variables that will be used in this analysis and only to visits from households that have completed the study (study_complete). I rename variables to assist with downstream analysis. I also include code for creating a fake intervention arm/variable to use in the analysis. When we are ready to unblind, I will delete this text and the code for the false intervention variable and will update the input data file to the unblinded dataset. 

I will use visit = 1 to create all baseline variables. I dropped visit = 0 as these were only if the mother wasn't 31 weeks pregnant. These data were just used to plan future enrollment visits. 


```r
# Create new dataset with variables used in this analysis
data_filt <- data %>%
  filter(study_complete == 1 & visit != 0) %>%
  select(main_id, visit, neighborhood, code, SES_score, high_poverty, san_basic_obs, fixed_emp_pri, secondary_complete, num_lt5, num_HH, months_in_hh, human_feces, animal_feces, HFIAS_score, HFIA_category, HW_dwell, season_rainy, flooding_HH_yard, ecolimpn_source, ecolimpn_stored, coliformmpn_source, coliformmpn_stored, C_10, water_service_ladder, improved_water, C_21, C_22, C_17, C_20, C_19, C_28_A1, C_28_A2, A_W_9, JMP_H2, C_4, C_23, C_18, O_W_7, O_W_8, C_12, LT30min, timeperweek, flow_rate, C_24, C_25, HWISE_scale, HWISE_insecure, M_1B, C_15_D, C_31, sub_SES_score, sub_density, sub_SES_tertile, sub_density_tertile, sub_density_strata, SES_score_bl) %>%
  rename(water_source = C_10, sat_color_appear = C_21, sat_taste_smell = C_22, sat_service = C_17, sat_pressure = C_20, sat_avail = C_19, storage_tank = C_28_A1, water_storage = A_W_9, water_HW = JMP_H2, share_FIPAG = C_4, sufficient_quant = C_23, sat_afford = C_18, free_chlor = O_W_7, total_chlor = O_W_8, collect_contin = C_12, avail_hr = C_24, avail_day = C_25, use_meter = M_1B, use_respond = C_15_D, child_consump = C_31)

# Create false intervention variable (will be deleted after unblinding)
matching_fake_arm <- read_excel("../../../../../../OneDrive-SharedLibraries-EmoryUniversity/Levy, Karen - 1. PAASIM/3. Data and Analysis/Data/Blinded dataset/matching_fake_arm.xlsx")

all_data <- merge(matching_fake_arm, data_filt, by = "code", all = TRUE)

# Checking visit numbers - there should be 642 for each visit
table(all_data$visit)
```

```
## 
##   1   2   3   4   5 
## 642 642 642 642 642
```

```r
# cleanup
rm(data_filt, data, matching_fake_arm)

# Water storage 
table(all_data$water_storage)
```

```
## 
##    1    3    4 
##   30 3097    9
```

```r
# Water available at HW facility
table(all_data$water_HW)
```

```
## 
##    0    1 
## 1642 1567
```

```r
# Sharing of drinking water connection 
table(all_data$share_FIPAG)
```

```
## 
##   0   1 
## 538 760
```

```r
# Sufficient quantity of drinking water 
table(all_data$sufficient_quant)
```

```
## 
##    1    2  999 
##  429 2776    4
```

```r
# Distance of HH to water main 


# Distance of HH to drinking water source 


# Time to collect water (min)
summary(all_data$collect_contin)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    1.00    5.00   10.42   10.00  240.00       1
```

```r
# Piped water flow rate (L/min)
options(digits = 4)
summary(all_data$flow_rate) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.1     0.1     0.1     5.0     432
```

```r
# Availability (hrs/day)
summary(all_data$avail_hr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     9.0    12.0    13.2    16.0    24.0       1
```

```r
# Availability (days/week)
summary(all_data$avail_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    7.00    7.00    6.52    7.00    7.00       1
```

```r
# Water insecure households 
table(all_data$HWISE_insecure)
```

```
## 
##    0    1 
## 2551  659
```

```r
# Water usage (L/day) - meter
summary(all_data$use_meter)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0     235     676    3859     999  766637    2161
```

```r
# Water usage (L/day) - respondent 
summary(all_data$use_respond)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     3.0    60.0    80.0    86.2   100.0   515.0       1
```

```r
# Child consumption of drinking water source 
table(all_data$child_consump)
```

```
## 
##    0    1 
## 1266 1301
```

# Analysis

I briefly describe an outline of the planned analyses below. First, I will import and filter the data for variables that will be used in this analysis. I will univariately explore the associations between both levels of our intervention (household and neighborhood) and water quality and access. Following this, I will construct multivariate generalized estimating equations (GEE) to look at the association between both levels of the intervention and various water quality and access variables. 

* Table 1: Baseline differences in covariates of interest (neighborhood-level)
* Table 2: Baseline differences in covariates of interest (household-level)
* Table 3: Univariate analysis of water quality and access (neighborhood-level)
* Table 4: Univariate analysis of water quality and access (household-level)
* Table 5: Multivariate modeling of neighborhood-level intervention status and water quality and access 
* Figure 2: Plots associated with data from Table 5
* Table 6: Multivariate modeling of household-level intervention status and water quality and access 
* Figure 3: Plots associated with data from Table 6


## Data overview


## Table 1 - Baseline differences in covariates of interest (neighborhood-level)

Question - clarify with Josh that this table needs to be included because we are matching on the neighborhood-level characteristics so those vars would already be in the model


```r
# Formatting
data_visit1 <- all_data %>%
  filter(visit == 1)

# Data Analysis

# Cleanup
# rm(insert removable objects here)
```

## Table 2 - Baseline differences in covariates of interest (household-level)


```r
# Formatting- exclude until after we run models

#data_visit1$san_basic_obs <- 
 # factor(data_visit1$san_basic_obs, levels=c(1,0),
  #       labels=c("Yes", 
   #               "No"))

#data_visit1$fixed_emp_pri <- 
 # factor(data_visit1$fixed_emp_pri, levels=c(1,0),
  #       labels=c("Yes", 
   #               "No"))

#data_visit1$human_feces <- 
 # factor(data_visit1$human_feces, levels=c(1,0),
  #       labels=c("Yes", 
   #               "No"))

#data_visit1$animal_feces <- 
 # factor(data_visit1$animal_feces, levels=c(1,0),
  #       labels=c("Yes", 
   #               "No"))

#data_visit1$HFIA_category <- 
 # factor(data_visit1$HFIA_category, levels=c(1,2,3,4),
  #       labels=c("Food secure", 
 #                 "Mildly food insecure",
  #                "Moderately food insecure",
   #               "Severely food insecure"))

#data_visit1$HW_dwell <- 
  #factor(data_visit1$HW_dwell, levels=c(1,0),
   #      labels=c("Yes", 
    #              "No"))

#data_visit1$season_rainy <- 
 # factor(data_visit1$season_rainy, levels=c(1,0),
  #       labels=c("Yes", 
   #             "No"))

#data_visit1$flooding_HH_yard <- 
 # factor(data_visit1$flooding_HH_yard, levels=c(1,0),
  #       labels=c("Yes", 
   #               "No"))

#data_visit1$fakearm <- 
 # factor(data_visit1$fakearm, levels=c(1,0),
  #       labels=c("Intervention", 
   #               "Control")) 

# Labeling vars - can edit this code later to make this code shorter 
label(data_visit1$SES_score) <- "SES Score"
label(data_visit1$num_lt5) <- "Number of children under 5 in household"
label(data_visit1$num_HH) <- "Number of people in household"
label(data_visit1$months_in_hh) <- "Months living in household"
label(data_visit1$san_basic_obs) <- "Basic sanitation access"
label(data_visit1$fixed_emp_pri) <- "Fixed employment status of primary wage earner"
label(data_visit1$human_feces) <- "Observed human feces in or near the household"
label(data_visit1$animal_feces) <- "Observed animal feces in or near the household"
label(data_visit1$HFIA_category) <- "Household Food Insecurity Access Scale (categorized)"
label(data_visit1$HW_dwell) <- "Handwashing station (with soap and water) in house or yard"
label(data_visit1$season_rainy) <- "Rainy season (Dec-Apr)"
label(data_visit1$flooding_HH_yard) <- "Any flooding in household or yard in the last month"
label(data_visit1$fakearm) <- "FIPAG connection at household"

caption <- "Table 2. Baseline differences in covariates of interest based household-intervention status"
# footnote <- "ᵃ example footnote here"

# Data Analysis

# Create Table 1
table1(~ SES_score + num_lt5 + num_HH + months_in_hh + san_basic_obs + fixed_emp_pri + human_feces + animal_feces + HFIA_category + HW_dwell + season_rainy + flooding_HH_yard | fakearm, data = data_visit1, caption = caption,  render.continuous=my.render.cont, render.categorical=my.render.cat) # Add in footnote = footnote if applicable
```

```
## Warning in table1.formula(~SES_score + num_lt5 + num_HH + months_in_hh + :
## Terms to the right of '|' in formula 'x' define table columns and are expected
## to be factors with meaningful labels.
```

```{=html}
<div class="Rtable1"><table class="Rtable1"><caption>Table 2. Baseline differences in covariates of interest based household-intervention status</caption>

<thead>
<tr>
<th class='rowlabel firstrow lastrow'></th>
<th class='firstrow lastrow'><span class='stratlabel'>0<br><span class='stratn'>(N=399)</span></span></th>
<th class='firstrow lastrow'><span class='stratlabel'>1<br><span class='stratn'>(N=243)</span></span></th>
<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=642)</span></span></th>
</tr>
</thead>
<tbody>
<tr>
<td class='rowlabel firstrow'>SES Score</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>66 (&plusmn; 12)</td>
<td class='lastrow'>68 (&plusmn; 12)</td>
<td class='lastrow'>66 (&plusmn; 12)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Number of children under 5 in household</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>0.76 (&plusmn; 0.73)</td>
<td class='lastrow'>0.66 (&plusmn; 0.82)</td>
<td class='lastrow'>0.72 (&plusmn; 0.77)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Number of people in household</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>4.8 (&plusmn; 2.3)</td>
<td class='lastrow'>5.0 (&plusmn; 2.4)</td>
<td class='lastrow'>4.9 (&plusmn; 2.4)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Months living in household</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>60 (&plusmn; 78)</td>
<td>81 (&plusmn; 100)</td>
<td>68 (&plusmn; 88)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>2 (0.5%)</td>
<td class='lastrow'>0 (0%)</td>
<td class='lastrow'>2 (0.3%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Basic sanitation access</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.31 (&plusmn; 0.46)</td>
<td>0.43 (&plusmn; 0.50)</td>
<td>0.35 (&plusmn; 0.48)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>2 (0.5%)</td>
<td class='lastrow'>0 (0%)</td>
<td class='lastrow'>2 (0.3%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Fixed employment status of primary wage earner</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.35 (&plusmn; 0.48)</td>
<td>0.44 (&plusmn; 0.50)</td>
<td>0.38 (&plusmn; 0.49)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>29 (7.3%)</td>
<td class='lastrow'>15 (6.2%)</td>
<td class='lastrow'>44 (6.9%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Observed human feces in or near the household</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.0076 (&plusmn; 0.087)</td>
<td>0.0082 (&plusmn; 0.091)</td>
<td>0.0078 (&plusmn; 0.088)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>2 (0.5%)</td>
<td class='lastrow'>0 (0%)</td>
<td class='lastrow'>2 (0.3%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Observed animal feces in or near the household</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>0.11 (&plusmn; 0.31)</td>
<td class='lastrow'>0.082 (&plusmn; 0.28)</td>
<td class='lastrow'>0.097 (&plusmn; 0.30)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Household Food Insecurity Access Scale (categorized)</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>3.4 (&plusmn; 0.82)</td>
<td class='lastrow'>3.3 (&plusmn; 0.85)</td>
<td class='lastrow'>3.4 (&plusmn; 0.83)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Handwashing station (with soap and water) in house or yard</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>0.24 (&plusmn; 0.43)</td>
<td class='lastrow'>0.40 (&plusmn; 0.49)</td>
<td class='lastrow'>0.30 (&plusmn; 0.46)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Rainy season (Dec-Apr)</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>Mean (SD)</td>
<td class='lastrow'>0.48 (&plusmn; 0.50)</td>
<td class='lastrow'>0.53 (&plusmn; 0.50)</td>
<td class='lastrow'>0.50 (&plusmn; 0.50)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>Any flooding in household or yard in the last month</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.36 (&plusmn; 0.48)</td>
<td>0.26 (&plusmn; 0.44)</td>
<td>0.32 (&plusmn; 0.47)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>4 (1.0%)</td>
<td class='lastrow'>0 (0%)</td>
<td class='lastrow'>4 (0.6%)</td>
</tr>
</tbody>
</table>
</div>
```

```r
# RR for continuous variables 
continuous_vars <- c("SES_score", "num_lt5", "num_HH", "months_in_hh")
categorical_vars<- c("san_basic_obs", "fixed_emp_pri", "human_feces", "animal_feces", "HFIA_category", "HW_dwell", "season_rainy", "flooding_HH_yard")

# Function for continuous variables using linear regression 
calculate_rr_continuous <- function(data, group_var, continuous_var) {
  model <- lm(continuous_var ~ get(group_var), data = data)
  coef_val <- exp(coef(model)[2])  # Extract coefficient for the group variable and exponentiate
  p_val <- summary(model)$coefficients[2, 4]  # Extract p-value
  return(c(Risk_Ratio = coef_val, p_value = p_val))
}

# Loop function over all continuous variables
rr_continuous <- lapply(continuous_vars, function(var) {
  calculate_rr_continuous(data = data_visit1, group_var = "fakearm", continuous_var = data_visit1[[var]])
})

print(rr_continuous)
```

```
## [[1]]
## Risk_Ratio.get(group_var)                   p_value 
##                  11.60753                   0.01239 
## 
## [[2]]
## Risk_Ratio.get(group_var)                   p_value 
##                    0.8994                    0.0900 
## 
## [[3]]
## Risk_Ratio.get(group_var)                   p_value 
##                    1.2512                    0.2465 
## 
## [[4]]
## Risk_Ratio.get(group_var)                   p_value 
##                 1.282e+09                 3.282e-03
```

```r
# Function for categorical variables using Poisson regression 
calculate_rr_categorical <- function(data, group_var, categorical_var) {
  model <- glm(categorical_var ~ get(group_var), data = data, family = poisson)
  coef_val <- exp(coef(model)[2])  # Extract coefficient for the group variable and exponentiate
  p_val <- summary(model)$coefficients[2, 4]  # Extract p-value
  return(c(Risk_Ratio = coef_val, p_value = p_val))
}

# Loop function over all categorical variables
rr_categorical <- lapply(categorical_vars, function(var) {
  calculate_rr_categorical(data = data_visit1, group_var = "fakearm", categorical_var = data_visit1[[var]])
})

print(rr_categorical)
```

```
## [[1]]
## Risk_Ratio.get(group_var)                   p_value 
##                   1.38138                   0.01529 
## 
## [[2]]
## Risk_Ratio.get(group_var)                   p_value 
##                   1.27057                   0.07149 
## 
## [[3]]
## Risk_Ratio.get(group_var)                   p_value 
##                    1.0892                    0.9255 
## 
## [[4]]
## Risk_Ratio.get(group_var)                   p_value 
##                    0.7819                    0.3651 
## 
## [[5]]
## Risk_Ratio.get(group_var)                   p_value 
##                    0.9859                    0.7501 
## 
## [[6]]
## Risk_Ratio.get(group_var)                   p_value 
##                 1.6765432                 0.0003436 
## 
## [[7]]
## Risk_Ratio.get(group_var)                   p_value 
##                     1.112                     0.351 
## 
## [[8]]
## Risk_Ratio.get(group_var)                   p_value 
##                   0.71614                   0.02724
```

```r
# Cleanup
rm(caption)
```

## Table 3 - Univariate association between intervention with water quality and access  (neighborhood-level)

```r
# Formatting

# Data Analysis 
## Continuous Vars
### Linear regression function 
run_linear_regression <- function(data, predictor, responses) {
  results <- map_dfr(responses, function(response) {
    # Fit linear regression model
    model <- lm(paste(response, "~", predictor), data = data)
    
    # Extract coefficients and compute risk ratio, p-value, and CI
    coefficients <- tidy(model)
    coefficient <- coefficients$estimate[2]  # coefficient for the predictor
    p_value <- tidy(model)$p.value[2]
    conf_int <- confint(model)[2, ]
    risk_ratio <- exp(coefficient)  # Risk ratio calculation
    
    # Create a data frame for the result
    result_df <- data.frame(
      Response = response,
      Risk_Ratio = risk_ratio,
      P_Value = p_value,
      CI_Lower = conf_int[1],
      CI_Upper = conf_int[2]
    )
    
    return(result_df)
  })
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

### List of continuous outcome vars- need to add distance variables (hh to water main and hh to water source)
predictor <- "fakearm"
responses <- c("ecolimpn_stored", "ecolimpn_source", "coliformmpn_stored", "coliformmpn_source", "free_chlor", "total_chlor", "collect_contin", "timeperweek", "flow_rate", "avail_hr", "avail_day", "use_meter", "use_respond")

results <- run_linear_regression(all_data, predictor, responses)
results <- t(results)

results_table_continuous <- kable(results, align = "lccc") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:5, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#0073e6")

results_table_continuous
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> Response </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> Risk_Ratio </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> P_Value </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> CI_Lower </th>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> CI_Upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> ecolimpn_stored </td>
   <td style="text-align:center;width: 2cm; "> 4620703239.83373 </td>
   <td style="text-align:center;width: 2cm; "> 0.134480968282728 </td>
   <td style="text-align:center;width: 2cm; "> -6.89249680539625 </td>
   <td style="text-align:left;width: 2cm; "> 51.4001222991373 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> ecolimpn_source </td>
   <td style="text-align:center;width: 2cm; "> 4228.59335489967 </td>
   <td style="text-align:center;width: 2cm; "> 0.314616881797399 </td>
   <td style="text-align:center;width: 2cm; "> -7.92860007623903 </td>
   <td style="text-align:left;width: 2cm; "> 24.6278494292865 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> coliformmpn_stored </td>
   <td style="text-align:center;width: 2cm; "> 7.50049918609126e+99 </td>
   <td style="text-align:center;width: 2cm; "> 3.50943081461508e-08 </td>
   <td style="text-align:center;width: 2cm; "> 148.397798951397 </td>
   <td style="text-align:left;width: 2cm; "> 311.543988614369 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> coliformmpn_source </td>
   <td style="text-align:center;width: 2cm; "> 1.34786156130145e+32 </td>
   <td style="text-align:center;width: 2cm; "> 0.00754786234610607 </td>
   <td style="text-align:center;width: 2cm; "> 19.7234266329578 </td>
   <td style="text-align:left;width: 2cm; "> 128.239057934431 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> free_chlor </td>
   <td style="text-align:center;width: 2cm; "> 0.977292086369315 </td>
   <td style="text-align:center;width: 2cm; "> 0.193632986565986 </td>
   <td style="text-align:center;width: 2cm; "> -0.0576094904322808 </td>
   <td style="text-align:left;width: 2cm; "> 0.0116700722067528 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> total_chlor </td>
   <td style="text-align:center;width: 2cm; "> 0.966106977934194 </td>
   <td style="text-align:center;width: 2cm; "> 0.0601549434933112 </td>
   <td style="text-align:center;width: 2cm; "> -0.0704355863346723 </td>
   <td style="text-align:left;width: 2cm; "> 0.00147417093848123 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> collect_contin </td>
   <td style="text-align:center;width: 2cm; "> 1991.39681356567 </td>
   <td style="text-align:center;width: 2cm; "> 7.18161937207607e-35 </td>
   <td style="text-align:center;width: 2cm; "> 6.40185402863091 </td>
   <td style="text-align:left;width: 2cm; "> 8.79132914707934 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> timeperweek </td>
   <td style="text-align:center;width: 2cm; "> 9797615188.18988 </td>
   <td style="text-align:center;width: 2cm; "> 3.81381157759573e-10 </td>
   <td style="text-align:center;width: 2cm; "> 15.8241001481819 </td>
   <td style="text-align:left;width: 2cm; "> 30.1867095415483 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> flow_rate </td>
   <td style="text-align:center;width: 2cm; "> 1.00699904790832 </td>
   <td style="text-align:center;width: 2cm; "> 0.412273630649581 </td>
   <td style="text-align:center;width: 2cm; "> -0.00970299599444412 </td>
   <td style="text-align:left;width: 2cm; "> 0.0236523325196732 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> avail_hr </td>
   <td style="text-align:center;width: 2cm; "> 3.35961911558405 </td>
   <td style="text-align:center;width: 2cm; "> 1.46451956947061e-08 </td>
   <td style="text-align:center;width: 2cm; "> 0.793535162941465 </td>
   <td style="text-align:left;width: 2cm; "> 1.63012005524339 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> avail_day </td>
   <td style="text-align:center;width: 2cm; "> 0.826254522640353 </td>
   <td style="text-align:center;width: 2cm; "> 1.19177115164572e-05 </td>
   <td style="text-align:center;width: 2cm; "> -0.276170784310355 </td>
   <td style="text-align:left;width: 2cm; "> -0.105534043962429 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> use_meter </td>
   <td style="text-align:center;width: 2cm; "> 5.66978223970862e-64 </td>
   <td style="text-align:center;width: 2cm; "> 0.954619647322604 </td>
   <td style="text-align:center;width: 2cm; "> -5166.00310974568 </td>
   <td style="text-align:left;width: 2cm; "> 4874.74251926506 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> use_respond </td>
   <td style="text-align:center;width: 2cm; "> 1.75718950247817e-05 </td>
   <td style="text-align:center;width: 2cm; "> 2.63943091536956e-10 </td>
   <td style="text-align:center;width: 2cm; "> -14.335988634504 </td>
   <td style="text-align:left;width: 2cm; "> -7.56243097724345 </td>
  </tr>
</tbody>
</table>

```r
## Categorical Vars 

## Poisson regression function - model did not converge with log binomial (based on Liz/Josh's suggestion)
run_poisson_regression_with_rr <- function(data, predictor, responses) {
  results <- map_dfr(responses, function(response) {
    # Fit Poisson regression model
    model <- glm(paste(response, "~", predictor), family = poisson, data = data)
    
    # Extract coefficients and compute risk ratio, p-value, and CI
    coefficients <- tidy(model)
    coefficient <- coefficients$estimate[2]  # coefficient for the predictor
    p_value <- coefficients$p.value[2]
    conf_int <- confint(model)[2, ]
    
    # Compute risk ratio approximation
    risk_ratio <- exp(coefficient)  # Risk ratio calculation
    
    # Create a data frame for the result
    result_df <- data.frame(
      Response = response,
      Risk_Ratio_Approx = risk_ratio,
      P_Value = p_value,
      CI_Lower = conf_int[1],
      CI_Upper = conf_int[2]
    )
    
    return(result_df)
  })
  
  return(results)
}

### List of categorical variables - add enteropathogens, categorical FIB, do I include sharing? what var to use for storage?
responses <- c("sat_color_appear", "sat_taste_smell", "sat_service", "sat_pressure", "sat_avail", "water_storage", "water_HW", "sufficient_quant", "sat_afford", "HWISE_insecure", "child_consump")

results <- run_poisson_regression_with_rr(all_data, predictor, responses)
```

```
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
## Waiting for profiling to be done...
```

```r
results <- t(results)

results_table_cat <- kable(results, align = "lccc") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:5, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#0073e6")

results_table_bw <- kable(results, align = "lccc", format = "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:5, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "black", background = "white") %>%
  row_spec(1:nrow(results), background = "white")  # Alternate row colors


results_table_cat
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;">  </th>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...1 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...2 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...3 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...4 </th>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...5 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...6 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...7 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...8 </th>
   <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...9 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...10 </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(0, 115, 230, 255) !important;"> 2.5 %...11 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Response </td>
   <td style="text-align:left;width: 2cm; "> sat_color_appear </td>
   <td style="text-align:center;width: 2cm; "> sat_taste_smell </td>
   <td style="text-align:center;width: 2cm; "> sat_service </td>
   <td style="text-align:center;width: 2cm; "> sat_pressure </td>
   <td style="text-align:left;"> sat_avail </td>
   <td style="text-align:center;"> water_storage </td>
   <td style="text-align:center;"> water_HW </td>
   <td style="text-align:center;"> sufficient_quant </td>
   <td style="text-align:left;"> sat_afford </td>
   <td style="text-align:center;"> HWISE_insecure </td>
   <td style="text-align:center;"> child_consump </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Risk_Ratio_Approx </td>
   <td style="text-align:left;width: 2cm; "> 0.4479 </td>
   <td style="text-align:center;width: 2cm; "> 1.0795 </td>
   <td style="text-align:center;width: 2cm; "> 0.5094 </td>
   <td style="text-align:center;width: 2cm; "> 1.5885 </td>
   <td style="text-align:left;"> 0.7322 </td>
   <td style="text-align:center;"> 1.0018 </td>
   <td style="text-align:center;"> 1.1620 </td>
   <td style="text-align:center;"> 0.8052 </td>
   <td style="text-align:left;"> 2.4177 </td>
   <td style="text-align:center;"> 1.0305 </td>
   <td style="text-align:center;"> 0.9610 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> P_Value </td>
   <td style="text-align:left;width: 2cm; "> 9.388e-165 </td>
   <td style="text-align:center;width: 2cm; "> 2.717e-04 </td>
   <td style="text-align:center;width: 2cm; "> 4.884e-209 </td>
   <td style="text-align:center;width: 2cm; "> 2.467e-117 </td>
   <td style="text-align:left;"> 1.451e-28 </td>
   <td style="text-align:center;"> 9.322e-01 </td>
   <td style="text-align:center;"> 3.475e-03 </td>
   <td style="text-align:center;"> 3.777e-24 </td>
   <td style="text-align:left;"> 0.000e+00 </td>
   <td style="text-align:center;"> 7.084e-01 </td>
   <td style="text-align:center;"> 4.906e-01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> CI_Lower </td>
   <td style="text-align:left;width: 2cm; "> -0.86114 </td>
   <td style="text-align:center;width: 2cm; "> 0.03524 </td>
   <td style="text-align:center;width: 2cm; "> -0.71754 </td>
   <td style="text-align:center;width: 2cm; "> 0.42342 </td>
   <td style="text-align:left;"> -0.36705 </td>
   <td style="text-align:center;"> -0.04016 </td>
   <td style="text-align:center;"> 0.04910 </td>
   <td style="text-align:center;"> -0.25873 </td>
   <td style="text-align:left;"> 0.85765 </td>
   <td style="text-align:center;"> -0.12829 </td>
   <td style="text-align:center;"> -0.15331 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> CI_Upper </td>
   <td style="text-align:left;width: 2cm; "> -0.74602 </td>
   <td style="text-align:center;width: 2cm; "> 0.11755 </td>
   <td style="text-align:center;width: 2cm; "> -0.63184 </td>
   <td style="text-align:center;width: 2cm; "> 0.50221 </td>
   <td style="text-align:left;"> -0.25683 </td>
   <td style="text-align:center;"> 0.04365 </td>
   <td style="text-align:center;"> 0.25051 </td>
   <td style="text-align:center;"> -0.17492 </td>
   <td style="text-align:left;"> 0.90803 </td>
   <td style="text-align:center;"> 0.18632 </td>
   <td style="text-align:center;"> 0.07268 </td>
  </tr>
</tbody>
</table>

```r
results_table_bw
```

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;font-weight: bold;color: black !important;background-color: white !important;">   </th>
   <th style="text-align:left;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...1 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...2 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...3 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...4 </th>
   <th style="text-align:left;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...5 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...6 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...7 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...8 </th>
   <th style="text-align:left;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...9 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...10 </th>
   <th style="text-align:center;font-weight: bold;color: black !important;background-color: white !important;"> 2.5 %...11 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white !important;"> Response </td>
   <td style="text-align:left;width: 2cm; background-color: white !important;"> sat_color_appear </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> sat_taste_smell </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> sat_service </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> sat_pressure </td>
   <td style="text-align:left;background-color: white !important;"> sat_avail </td>
   <td style="text-align:center;background-color: white !important;"> water_storage </td>
   <td style="text-align:center;background-color: white !important;"> water_HW </td>
   <td style="text-align:center;background-color: white !important;"> sufficient_quant </td>
   <td style="text-align:left;background-color: white !important;"> sat_afford </td>
   <td style="text-align:center;background-color: white !important;"> HWISE_insecure </td>
   <td style="text-align:center;background-color: white !important;"> child_consump </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white !important;"> Risk_Ratio_Approx </td>
   <td style="text-align:left;width: 2cm; background-color: white !important;"> 0.4479 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 1.0795 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 0.5094 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 1.5885 </td>
   <td style="text-align:left;background-color: white !important;"> 0.7322 </td>
   <td style="text-align:center;background-color: white !important;"> 1.0018 </td>
   <td style="text-align:center;background-color: white !important;"> 1.1620 </td>
   <td style="text-align:center;background-color: white !important;"> 0.8052 </td>
   <td style="text-align:left;background-color: white !important;"> 2.4177 </td>
   <td style="text-align:center;background-color: white !important;"> 1.0305 </td>
   <td style="text-align:center;background-color: white !important;"> 0.9610 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white !important;"> P_Value </td>
   <td style="text-align:left;width: 2cm; background-color: white !important;"> 9.388e-165 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 2.717e-04 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 4.884e-209 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 2.467e-117 </td>
   <td style="text-align:left;background-color: white !important;"> 1.451e-28 </td>
   <td style="text-align:center;background-color: white !important;"> 9.322e-01 </td>
   <td style="text-align:center;background-color: white !important;"> 3.475e-03 </td>
   <td style="text-align:center;background-color: white !important;"> 3.777e-24 </td>
   <td style="text-align:left;background-color: white !important;"> 0.000e+00 </td>
   <td style="text-align:center;background-color: white !important;"> 7.084e-01 </td>
   <td style="text-align:center;background-color: white !important;"> 4.906e-01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white !important;"> CI_Lower </td>
   <td style="text-align:left;width: 2cm; background-color: white !important;"> -0.86114 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 0.03524 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> -0.71754 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 0.42342 </td>
   <td style="text-align:left;background-color: white !important;"> -0.36705 </td>
   <td style="text-align:center;background-color: white !important;"> -0.04016 </td>
   <td style="text-align:center;background-color: white !important;"> 0.04910 </td>
   <td style="text-align:center;background-color: white !important;"> -0.25873 </td>
   <td style="text-align:left;background-color: white !important;"> 0.85765 </td>
   <td style="text-align:center;background-color: white !important;"> -0.12829 </td>
   <td style="text-align:center;background-color: white !important;"> -0.15331 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white !important;"> CI_Upper </td>
   <td style="text-align:left;width: 2cm; background-color: white !important;"> -0.74602 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 0.11755 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> -0.63184 </td>
   <td style="text-align:center;width: 2cm; background-color: white !important;"> 0.50221 </td>
   <td style="text-align:left;background-color: white !important;"> -0.25683 </td>
   <td style="text-align:center;background-color: white !important;"> 0.04365 </td>
   <td style="text-align:center;background-color: white !important;"> 0.25051 </td>
   <td style="text-align:center;background-color: white !important;"> -0.17492 </td>
   <td style="text-align:left;background-color: white !important;"> 0.90803 </td>
   <td style="text-align:center;background-color: white !important;"> 0.18632 </td>
   <td style="text-align:center;background-color: white !important;"> 0.07268 </td>
  </tr>
</tbody>
</table>

```r
# Cleanup
# rm(insert removable objects here)
```

## Table 4 - Univariate association between intervention with water quality and access (household-level)

```r
# Formatting

# Data Analysis 

# Cleanup
# rm(insert removable objects here)
```

## Table 5 - Multivariate association between intervention with water quality and access (neighborhood-level)

### Model Construction (Table 5)


### Model Validation (Table 5)


```r
# (for GLMMs, we typically use the `DHARMa` package for this, e.g.
# replace "my.model" with the name of the model you are assessing
# and probably change the name "sim.out.my.model" to something more descriptive)
# simulate residuals using DHARMa:
#sim.out.my.model <- simulateResiduals(fittedModel = my.model, plot = F)
#plot(sim.out.my.model) # plot residual plots
#testDispersion(sim.out.my.model, plot = F) # print dispersion results
#testZeroInflation(sim.out.my.model, plot = F) # print zero-inflation results

# CLEANUP
# again, remove anything that isn't subsequently used in the analysis
#rm(sim.out.my.model)
```

### Plots (Table 5) 

```r
# any data preparation / summarization needed for plotting
# code below is just a placeholder!
#myplotdata = mydata %>% group_by(mygroupingvar1, mygroupingvar1) %>% 
 # summarize(mean = mean(), stdev = sd()...) 

# create the actual plot (almost always using `ggplot2`)
#myplot = ggplot(myplotdata, ...)

# DISPLAY the plot in your Rmarkdown report 
# (usually just a single line of code, 
# the name of the ggplot object you created above)
#myplot

# SAVE PLOT
# example, but key thing is the `plots/` prefix in the file path
#ggsave(myplot, file = "plots/myplot.pdf", width = 5, height = 3)

# # CLEANUP
# # remember to also clean up any objects such as data summarizations
# # set up for your plotting, e.g.:
#rm(myplotdata, myplot)
```

## Table 6 - Multivariate association between intervention with water quality and access (household-level)

### Model Construction (Table 6)


### Model Validation (Table 6)



### Plots (Table 6) 


# Session Info


```r
sessionInfo()
```

```
## R version 4.2.2 (2022-10-31)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS 14.3.1
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
##  [1] flextable_0.9.4  broom_1.0.5      table1_1.4.3     boot_1.3-29     
##  [5] evaluate_0.23    readxl_1.4.3     kableExtra_1.4.0 knitr_1.45      
##  [9] lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
## [13] purrr_1.0.2      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1    
## [17] ggplot2_3.4.4    tidyverse_2.0.0 
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.8              jsonlite_1.8.8          viridisLite_0.4.2      
##  [4] bslib_0.6.1             Formula_1.2-5           shiny_1.8.0            
##  [7] askpass_1.2.0           highr_0.10              fontLiberation_0.1.0   
## [10] cellranger_1.1.0        yaml_2.3.8              gdtools_0.3.5          
## [13] pillar_1.9.0            backports_1.4.1         glue_1.7.0             
## [16] uuid_1.2-0              digest_0.6.34           promises_1.2.1         
## [19] colorspace_2.1-0        htmltools_0.5.7         httpuv_1.6.13          
## [22] gfonts_0.2.0            fontBitstreamVera_0.1.1 pkgconfig_2.0.3        
## [25] httpcode_0.3.0          xtable_1.8-4            scales_1.3.0           
## [28] svglite_2.1.3           later_1.3.2             officer_0.6.3          
## [31] fontquiver_0.2.1        tzdb_0.4.0              timechange_0.3.0       
## [34] openssl_2.1.1           generics_0.1.3          ellipsis_0.3.2         
## [37] cachem_1.0.8            withr_3.0.0             cli_3.6.2              
## [40] magrittr_2.0.3          crayon_1.5.2            mime_0.12              
## [43] fansi_1.0.6             MASS_7.3-60.0.1         xml2_1.3.6             
## [46] textshaping_0.3.7       tools_4.2.2             data.table_1.14.10     
## [49] hms_1.1.3               lifecycle_1.0.4         munsell_0.5.0          
## [52] zip_2.3.0               compiler_4.2.2          jquerylib_0.1.4        
## [55] systemfonts_1.0.5       rlang_1.1.3             grid_4.2.2             
## [58] rstudioapi_0.15.0       rmarkdown_2.25          gtable_0.3.4           
## [61] curl_5.2.0              R6_2.5.1                fastmap_1.1.1          
## [64] utf8_1.2.4              ragg_1.2.7              stringi_1.8.3          
## [67] crul_1.4.0              Rcpp_1.0.12             vctrs_0.6.5            
## [70] tidyselect_1.2.0        xfun_0.41
```

