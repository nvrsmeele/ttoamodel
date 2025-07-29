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
# Discrete Choice Experiment: New Health Insurance Policy
#
# Post-estimation analyses:
#   - Likelihood Ratio Tests
#   - Willingness-to-pay calculations (incl. Delta method)
#
# v1.0 (July, 2025)
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
benchmark_mnl = "./src/experiment1/results/mnl_benchmark/Benchmark_MNL"
ttoa_mnl = "./src/experiment1/results/mnl_ttoa/MNL-TTOA"
rum_lc = "./src/experiment1/results/lc_rum/RUM-LC"
ttoa_lc = "./src/experiment1/results/lc_ttoa/TTOA-LC"

benchmark_mnl_model = apollo_loadModel(benchmark_mnl)
ttoa_mnl_model = apollo_loadModel(ttoa_mnl)
lc_rum_model = apollo_loadModel(rum_lc)
lc_ttoa_model = apollo_loadModel(ttoa_lc)

#####################################################################
#   Likelihood Ratio Test
#####################################################################

# Likelihood ratio test between benchmark MNL and TTOA-MNL model
apollo_lrTest(benchmark_mnl_model, ttoa_mnl_model)

# Likelihood ratio test between RUM-LC and TTOA-LC model (note: TTOA-LC model without covariates)
apollo_lrTest(lc_rum_model, lc_ttoa_model)

#####################################################################
#   WTP calculations for the Benchmark MNL model
#####################################################################

# Compute WTP and its standard error for the benchmark MNL model
deltaMethod_settings=list(expression=c(WTP_savelives="b_deaths/b_premium"))
apollo_deltaMethod(benchmark_mnl_model, deltaMethod_settings)

#####################################################################
#   WTP calculations for the TTOA-MNL model
#####################################################################

# Compute WTP and its standard error for the TTOA-MNL model
deltaMethod_settings=list(expression=c(WTP_savelives="b_deaths/b_premium",
                                       WTP_taboo_neg="b_taboo_neg/b_premium",
                                       WTP_taboo_pos="b_taboo_pos/b_premium"))

apollo_deltaMethod(ttoa_mnl_model, deltaMethod_settings)

#####################################################################
#   WTP calculations for the benchmark LC model with only RUM classes
#####################################################################

# Compute WTP and its standard error for the classes in the LC model
deltaMethod_settings=list(expression=c(WTP_savelives_1="b_deaths_1/b_premium_1",
                                       WTP_savelives_2="b_deaths_2/b_premium_2",
                                       WTP_savelives_3="b_deaths_3/b_premium_3"))

apollo_deltaMethod(lc_rum_model, deltaMethod_settings)

#####################################################################
#   WTP calculations for the TTOA-LC model
#####################################################################

# Compute WTP and its standard error for the classes in the LC model
deltaMethod_settings=list(expression=c(WTP_savelives_1="b_deaths_1/b_premium_1",
                                       WTP_savelives_2="b_deaths_2/b_premium_2",
                                       WTP_savelives_3="b_deaths_3/b_premium_3",
                                       WTP_taboo_neg_1="b_taboo_neg_1/b_premium_1",
                                       #WTP_taboo_pos_1="b_taboo_pos_1/b_premium_1", --> not present in class 1
                                       WTP_taboo_neg_2="b_taboo_neg_2/b_premium_2",
                                       WTP_taboo_pos_2="b_taboo_pos_2/b_premium_2",
                                       WTP_taboo_neg_3="b_taboo_neg_3/b_premium_3",
                                       WTP_taboo_pos_3="b_taboo_pos_3/b_premium_3"))

apollo_deltaMethod(lc_ttoa_model, deltaMethod_settings)