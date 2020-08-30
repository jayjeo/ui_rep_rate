


#start by producing decile reweightings which are used in both scripts
source("analysis/source/process_basic_cps.R")


#runs the bootstraps used to calculate standard errors
#commented out because of the time it takes to run
##source(analysis/source/run_bootstraps.R)


#produces output for the replacement rate paper, and saves a dataframe with benefits filtered to the eligible and in labour force
#this script will also call generate_robustness_table.R
source("analysis/source/plot_ui.R")

