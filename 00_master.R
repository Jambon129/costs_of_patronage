# master script for Replication Excercise of Guo (2018): The Costs of Patronage: 
# Evidence from the British Empire
# author (of replication): Andreas Chmielowski
# purpose: Replication Excercise for Econometrics III, 
# PhD Economics, Gothenburg University
#--------------------------------------

# set working directory (commented out because everything remains in that folder)
# setwd("C:/Users/xchmia/Documents/courses/1st_year/econometrics_3/replication/guo2018/R")

# packages (loads needed packages if installed, installs and loads them if not)
packages = c("tidyverse",    # data manipulation
             "ggplot2",      # plotting
             "haven",        # handle stata data
             "lfe",          # handle FE models
             "plm",          # handle more FE models
             "multcomp",     # to mimic "lincom" from stata
             "Hmisc",        # create leads an lags
             "did",          # Callaway and Sant'Anna's estimator
             "mediation",    # for the mediation analysis
             "beepr",        # beep sound so I am notified when calculation is completed
             "tools")        # for compiling

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# clear workspace
rm(list = ls())

# get data
patronage <- read_dta("analysis.dta")

# create all input for tables
source("./01_tables.R") 

# create all figures
source("./02_figures.R") 

# compile the document
knitr::knit2pdf("./replication_chmielowski.Rnw")
