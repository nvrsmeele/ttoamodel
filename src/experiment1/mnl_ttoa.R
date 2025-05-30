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
# Model: Multinomial Logit (MNL) with taboo trade-off aversion (TTOA)
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

# Set core controls
apollo_control = list(
  modelName       = "MNL_TTOA",
  modelDescr      = "TTOA-MNL model",
  indivID         = "RESPID",
  outputDirectory = "./src/experiment1/results/mnl_ttoa"
)

# Load data
path_data = "./data/experiment1/healthinsurance_dce.csv"
database = read.csv(path_data, header=TRUE)

# Initialise model params
apollo_beta=c(b_deaths     = 0,
              b_premium    = 0,
              b_taboo_neg  = 0,
              b_taboo_pos  = 0)

apollo_fixed = c()

# Checkpoint for model inputs
apollo_inputs = apollo_validateInputs()

# Define model and likelihood function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Create list of choice probabilities P
  P = list()
  
  # List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A1"]]  = b_deaths * A1_DEATHS + b_premium * A1_PREM + b_taboo_neg * A1_TABOO_NEG + b_taboo_pos * A1_TABOO_POS
  V[["A2"]]  = b_deaths * A2_DEATHS + b_premium * A2_PREM + b_taboo_neg * A2_TABOO_NEG + b_taboo_pos * A2_TABOO_POS
  
  # Initialise settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A1=1, A2=2), 
    avail         = list(A1=1, A2=1), 
    choiceVar     = CHOICE,
    utilities     = V
  )
  
  # Compute choice probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Model estimation
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Print model output with p-values
modelOutput_setting=list(printPVal=2)
apollo_modelOutput(model, modelOutput_setting)

# Save model output with p-values
saveOutput_settings=list(saveEst=TRUE, saveCorr=TRUE, saveCov=TRUE, saveModelObject=TRUE, printPVal=2)
apollo_saveOutput(model, saveOutput_settings)