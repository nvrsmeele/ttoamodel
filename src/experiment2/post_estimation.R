##################################################################
# Experiment 2 "Organ Transplantation Policy Decisions":
# Post-esimtation analyses of the discrete choice models.
# Likelihood Ratio Tests, Willingness-to-pay (incl. Delta method).
##################################################################

# Reset the global environment
rm(list = ls())

# Load library
library(apollo)

# Initialise code
apollo_initialise()

# Initialise estimated models
benchmark_mnl = "src/experiment2/mnl_benchmark/Benchmark_MNL"
ttoa_mnl = "./src/experiment2/mnl_ttoa/TTOA_MNL"
ttoa_lcl = "src/experiment2/lcl_ttoa/TTOA-LCL"

benchmark_mnl_model = apollo_loadModel(benchmark_mnl)
ttoa_mnl_model = apollo_loadModel(ttoa_mnl)
ttoa_lcl_model = apollo_loadModel(ttoa_lcl)

# Likelihood Ratio Test between benchmark MNL and TTOA-MNL model
apollo_lrTest(benchmark_mnl_model, ttoa_mnl_model)

# Compute WTP and its standard error for the benchmark MNL model
deltaMethod_settings=list(expression=c(WTP_deaths="b_deaths/b_premium"))
apollo_deltaMethod(benchmark_mnl_model, deltaMethod_settings)

# Compute WTP and its standard error for the TTOA-MNL model
deltaMethod_settings=list(expression=c(WTP_deaths="b_deaths/b_premium",
                                       WTP_taboo="-1*(b_taboo/b_premium)"))

apollo_deltaMethod(ttoa_mnl_model, deltaMethod_settings)

# Compute WTP and its standard error for the LC model with two "taboo" classes
deltaMethod_settings=list(expression=c(WTP_deaths_1="b_deaths_1/b_premium_1",
                                       WTP_deaths_2="b_deaths_2/b_premium_2",
                                       WTP_deaths_3="b_deaths_3/b_premium_3",
                                       WTP_taboo_1="-1*(b_taboo_1/b_premium_1)",
                                       WTP_taboo_2="b_taboo_2/b_premium_2",
                                       WTP_taboo_3="-1*(b_taboo_3/b_premium_3)"))
                                       
apollo_deltaMethod(ttoa_lcl_model, deltaMethod_settings)