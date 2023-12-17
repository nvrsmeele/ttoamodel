##################################################################
# Experiment 1 "Health Insurance Policy Decisions":
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
benchmark_mnl = "src/experiment1/mnl_benchmark/Benchmark_MNL"
ttoa_mnl = "src/experiment1/mnl_ttoa/TTOA_MNL"
ttoa_lcl = "src/experiment1/lcl_ttoa/TTOA-LCL"

benchmark_mnl_model = apollo_loadModel(benchmark_mnl)
ttoa_mnl_model = apollo_loadModel(ttoa_mnl)
ttoa_lcl_model = apollo_loadModel(ttoa_lc)

# Likelihood ratio test between benchmark MNL and TTOA-MNL model
apollo_lrTest(benchmark_mnl_model, ttoa_mnl_model)

# Compute WTP and its standard error for the benchmark MNL model
deltaMethod_settings=list(expression=c(WTP_savelives="b_deaths/b_premium"))
apollo_deltaMethod(benchmark_mnl_model, deltaMethod_settings)

# Compute WTP and its standard error for the TTOA-MNL model
deltaMethod_settings=list(expression=c(WTP_savelives="b_deaths/b_premium",
                                       WTP_taboo="b_taboo/b_premium"))

apollo_deltaMethod(ttoa_mnl_model, deltaMethod_settings)

# Compute WTP and its standard error for the classes in the LC model
deltaMethod_settings=list(expression=c(WTP_savelives_1="b_deaths_1/b_premium_1",
                                       WTP_taboo_1="b_taboo_1/b_premium_1",
                                       WTP_savelives_2="b_deaths_2/b_premium_2",
                                       WTP_taboo_2="b_taboo_2/b_premium_2",
                                       WTP_savelives_3="b_deaths_3/b_premium_3",
                                       WTP_taboo_3="b_taboo_3/b_premium_3"))

apollo_deltaMethod(ttoa_lcl_model, deltaMethod_settings)