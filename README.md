US Unemployment Insurance Replacement Rates During the Pandemic - Ganong, Noel, and Vavra

All questions should be directed to ganong@uchicago.edu

Scripts (in analysis/source)

master.R runs three scripts: process_basic_cps.R, plot_ui.R, and run_bootstraps.R. The running of these three scripts will internally source four
other scripts: ui_calculator.py, prelim.R, graph_prelim.R, and generate_robustness_table.R.

process_basic_cps.R uses the MORG to run the logit regression in the paper's main specification and calculate the unemployment rates in Table 1

plot_ui.R produces all tables, figures, and statistics in both the body and the appendix of the paper except for Table 2.

generate_robustness_table.R produces Table 2 and the statistic of the share of workers in tipped occupations 
referenced in section 5.4

ui_calculator.py calculates weekly state unemployment benefits given an earnings history. Please see details
for implementation at https://github.com/PSLmodels/ui_calculator. Note that the argument of the function 
call use_condaenv() in plot_ui.R should be replaced once these details are implemented. 

prelim.R loads packages, defines occupation and industry codes, and sets working paths

graph_prelim.R defines functions and settings for use in plot generation


Inputs (in analysis/inputs)

BAM_Q2_2019.csv, BAM_2018_benchmarks.csv are from the Benefits Accuracy Measurement survey by the Department of Labor

ar_5159.csv is from ETA 5159: Claims and Payment Activities, published by the Department of Labor

ASEC_1719.csv.gz is from the 2017-2019 IPUMS ASEC supplements

basic_cps.csv.gz is the 2020 IPUMS CPS

replicates_1719.csv.gz is the table of replicate weights from the 2017-2019 IPUMS ASEC supplements
cpiauscl.csv is monthly CPI data
