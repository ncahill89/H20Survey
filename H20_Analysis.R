#----------------------------------------------------------------------
# H20 Survey Analysis
# Niamh Cahill 2020
#----------------------------------------------------------------------

###Clear workspace
rm (list = ls( )) 

########Required Packages and Libraries########
library(tidyverse)
library(dplyr)
library(rjags)
library(R2jags)
library(ggpubr)
##############################################################

####Load the required R functions
source("R/results.R")
source("R/run_gmsl_model.R")

#### If you wish to view the data run the following
load("data/survey_H20.rda")
load("data/survey_H14.rda")

#### For the following functions you need to chooose:
## 1. A scenario (Red or Blue), 
## 2. A projection year (2100 or 2300) 
## 3. A survey (H14 or H20)

#### Run the model to get the PDF for GMSL
#### Once the model run is complete the PDF data will be saved! 
#### Therefore, you only need to run the model for a given scenario once.
#### Note: if you move the PDF data files from the results folder then plot_PDF() won't work.
run_gmsl_model(select_scenario = "Red",
              select_year = 2100,
              survey = "H20")

#### Plot the PDF for GMSL
#### If the relevant PDF data is not in the results folder (saved there from previous step) then this won't work. 
plot_PDF(select_year = 2100,
         select_scenario = "Red",
         survey = "H20")

#### Find the exceedence probability for a given gmsl value
get_exceedence_prob(select_year = 2100,
                    select_scenario = "Red",
                    survey = "H14",
                    gmsl = 0.98)
