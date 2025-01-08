# PAASIM Water Quality Analysis

**Authors**

Courtney P. Victor<sup>1</sup>, Joshua V. Garn<sup>2</sup>, Rassul Nalá<sup>3</sup>, João Luis Manuel<sup>4</sup>, Magalhães Mangamela<sup>5</sup>, Sandra McGunegill<sup>1</sup>, Jedidiah S. Snyder<sup>1</sup>, Sydney Hubbard<sup>1</sup>, Christine S. Fagnant-Sperati<sup>6</sup>, Joe Brown<sup>7</sup>, Thomas Clasen<sup>1</sup>, Konstantinos T. Konstantinidis<sup>8</sup>, Elizabeth T. Rogawski McQuade<sup>9</sup>, Lance A. Waller<sup>10</sup>, Karen Levy<sup>6#</sup> & Matthew C. Freeman<sup>1#</sup> on behalf of The PAASIM Study Authorship Group

#Authors contributed equally

**Affiliations**

1. Gangarosa Department of Environmental Health, Rollins School of Public Health, Emory University, Atlanta, Georgia, United States of America
2. Department of Epidemiology, Biostatistics, and Environmental Health, School of Public Health, University of Nevada, Reno, Nevada, United States of America
3. INS – Instituto Nacional de Saúde, Ministério de Saúde, Maputo, República de Moçambique
4. Beira Operations Research Center, National Health Institute (INS), Ministry of Health of Mozambique, Beira, Mozambique
5. AURA – Autoridade Reguladora de Água, Former Executive Secretary of the Water Regulation Council (CRA), (AURA, IP), Water Regulatory Authority, Public Institute, Maputo, Mozambique
6. Department of Environmental and Occupational Health Sciences, School of Public Health, University of Washington, Seattle, Washington, United States of America
7. Department of Environmental Science and Engineering, Gillings School of Global Public Health, University of North Carolina at Chapel Hill, Chapel Hill, North Carolina, USA
8. School of Civil and Environmental Engineering, Georgia Institute of Technology, Atlanta, Georgia, USA
9. Department of Epidemiology, Rollins School of Public Health, Emory University, Atlanta, Georgia, United States of America
10. Department of Biostatistics and Bioinformatics, Rollins School of Public Health, Emory University, Atlanta, Georgia, United States of America

## Background 

The objective of this analysis is to test how the provision of an improved piped water network affects water quality and access in Beira, Mozambique. This research is part of the PAASIM Study, which examines the impact of a new piped water network on acute and chronic health outcomes among children in low-income areas of urban Mozambique. The [PAASIM study protocol](https://bmjopen.bmj.com/content/13/3/e067341#ref-89) and [pre-analysis plan](https://osf.io/4rkn6/) for the PAASIM study are published elsewhere. We assess the following specific questions: 
1. What is the impact of the intervention on and association between having a direct connection and the prevalence of any *E. coli* in source water at the final study visit? [pre-specified primary outcome of the PAASIM Study]
2. What is the impact of the intervention on water quality and access across all timepoints? [pre-specified secondary outcome]
3. What is the association between having a direct connection and water quality and access across all timepiints? [pre-specified secondary outcome]
4. What is the joint effect of living in intervention sub-neighborhoods and having a direct connection on water quality and access throughout the study period?

We hypothesized that individuals who live in sub-neighborhoods where improvements to the piped water network had been made and individuals who have a direct connection to the piped network will have better water quality and increased access compared to individuals who do not live in such sub-neighborhoods or households.

## File Structure:

* the **main project folder** (not a subfolder) should ONLY contain:
    + R project file ("___.Rproj")&mdash;this will be renamed automatically following your project setup (you shouldn't have to do anything)
    + Rmarkdown file ("___.Rmd")&mdash;you should **rename the file** called `Rmarkdown-template-Brosi-lab.Rmd` once you start your new project; this will become your primary analysis file
    + Rmarkdown html report ("___.html")&mdash;**delete this file** (`Rmarkdown-template-Brosi-lab.html`) once you start your new project; when you knit your .Rmd file to create a report, it will generate a new .html file reflecting your updated name for the .Rmd file
    + .gitignore (automatic; don't mess)
    + .DS_Store (automatic; don't mess)
    + in addition, you may include one "sandbox" coding file, that you can play around in without worrying about making a mess (including e.g. storing deleted code temporarily, etc.). This can be a .R file or .Rmd; you'll have to manually add this if you want it
    + if there are multiple code files you are playing around with (i.e. more than one), you are welcome to create an additional `temp` or `sandbox` folder that you can keep other files in; be sure to update the README to inform viewers as to what that folder is about
* **data-in subfolder** should contain the data files you will import into your project:
    + copy into this folder the data you will input (typically you will do this on your local computer, rather than on the GitHub website)
* **data-out subfolder** contains data that were formatted / subset / cleaned / etc. in this analysis, that are saved for other analyses in the future:
    + NOTE: this folder is optional and may need to be deleted from some projects; WAIT to delete it until you are pretty sure you won't be saving any data for export
* **results subfolder** contains tables with model results
    + this one might also be optional if you don't want to save any, say, output tables from analyses, but again WAIT to delete until you are sure you won't use
* **plots subfolder** has saved plots from the analysis
    + save plots here; it is rare to have an analysis where you don't save any plots but if you find yourself in that situation, you may delete this subfolder
    + this subfolder is set up in the .gitignore, such that plots within it are not saved to the project (because Git does not do well with versioning non-text files)
    + when you create plots, they will still be saved on your local computer; and typically you should also include plots in your .Rmarkdown report, so they will be visible to those viewing the project
    
