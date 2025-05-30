#####################################################################
#
# Taboo trade-off aversion in choice behaviours: a discrete choice
# model and application to health-related decisions
#
# Authors: NVR Smeele, S van Cranenburgh, B Donkers, MH Schermer,
#          EW de Bekker-Grob
#
# Affiliations of the corresponding author:
#          Erasmus School of Health Policy & Management,
#          Erasmus University Rotterdam,
#          Erasmus Choice Modelling Centre
#
# Discrete Choice Experiment: New Organ Transplantation Policy
#
# Post-estimation analyses:
#   - Likelihood Ratio Test between MNL and MNL-TTOA
#   - Willingness-to-pay calculations (incl. Delta method)
#
# v1.0 (May, 2025)
#
# Corresponding author: Nicholas Smeele (smeele@eshpm.eur.nl)
#
#####################################################################

# Reset R-environment
rm(list = ls())

# Load library
library(apollo)

# Initialise code
apollo_initialise()

# Initialise estimated models
benchmark_mnl = "./src/experiment2/results/mnl_benchmark/Benchmark_MNL"
ttoa_mnl = "./src/experiment2/results/mnl_ttoa/MNL_TTOA"
ttoa_lc = "./src/experiment2/results/lcl_ttoa/LCL-TTOA"

benchmark_mnl_model = apollo_loadModel(benchmark_mnl)
ttoa_mnl_model = apollo_loadModel(ttoa_mnl)
ttoa_lc_model = apollo_loadModel(ttoa_lc)

#####################################################################
#   Likelihood Ratio Test
#####################################################################

# Likelihood Ratio Test between benchmark MNL and TTOA-MNL model
apollo_lrTest(benchmark_mnl_model, ttoa_mnl_model)

#####################################################################
#   WTP calculations for the Benchmark MNL model
#####################################################################

# Compute WTP and its standard error for the benchmark MNL model
deltaMethod_settings=list(expression=c(WTP_deaths="b_deaths/b_premium"))
apollo_deltaMethod(benchmark_mnl_model, deltaMethod_settings)

#####################################################################
#   WTP calculations for the TTOA-MNL model
#####################################################################

# Compute WTP and its standard error for the TTOA-MNL model
deltaMethod_settings=list(expression=c(WTP_deaths="b_deaths/b_premium",
                                       WTP_taboo="-1*(b_taboo/b_premium)"))

apollo_deltaMethod(ttoa_mnl_model, deltaMethod_settings)

#####################################################################
#   WTP calculations for the LCL model
#####################################################################

# Compute WTP and its standard error for the LC model with two "taboo" classes
deltaMethod_settings=list(expression=c(WTP_deaths_1="b_deaths_1/b_premium_1",
                                       WTP_deaths_2="b_deaths_2/b_premium_2",
                                       WTP_deaths_3="b_deaths_3/b_premium_3",
                                       WTP_taboo_1="-1*(b_taboo_1/b_premium_1)",
                                       WTP_taboo_2="b_taboo_2/b_premium_2",
                                       WTP_taboo_3="-1*(b_taboo_3/b_premium_3)"))
                                       
apollo_deltaMethod(ttoa_lc_model, deltaMethod_settings)