# PAASIM Water Quality Analysis

* Authors
  Courtney P. Victor<sup>1<sup>, Joshua V. Garn<sup>2<sup>, Rassul Nalá3, João Luis Manuel4, Magalhães Mangamela5, Sandra McGunegill1, Jedidiah S. Snyder1, Sydney Hubbard1, Christine S. Fagnant-Sperati6, Joe Brown7, Thomas Clasen1, Konstantinos T. Konstantinidis8, Elizabeth T. Rogawski McQuade9, Lance A. Waller10, Karen Levy6* & Matthew C. Freeman1* on behalf of The PAASIM Study Authorship Group

*Authors contributed equally

* when the analysis is published, a citation and link to the paper
* a brief description of where the data are from
* a brief description of what the analysis is getting at 

**File structure:**&mdash;YOU WILL NEED TO CHANGE SOME THINGS! read the below carefully:

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
    
