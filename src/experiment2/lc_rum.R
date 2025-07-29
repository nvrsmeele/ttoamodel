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
# Model: Latent Class Logit (LCL) with RUM classes
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

# Set core controls
apollo_control = list(
  modelName       = "RUM-LC",
  modelDescr      = "LC model with rum params across classes",
  indivID         = "RESPID",
  nCores          = 5,
  seed            = 100,
  outputDirectory = "./src/experiment2/results/lc_rum"
)

# Load data
path_data = "./data/experiment2/processed_data/main/organtransplantation_dce.csv"
database = read.csv(path_data, header=TRUE)

# Initialise model params
apollo_beta=c(# Class 1
              asc_sq_1       = 0,
              b_deaths_1     = 0,
              b_qol_1        = 0,
              b_premium_1    = 0,

              # Class 2
              asc_sq_2       = 0,
              b_deaths_2     = 0,
              b_qol_2        = 0,
              b_premium_2    = 0,

              # Class 3
              asc_sq_3       = 0,
              b_deaths_3     = 0,
              b_qol_3        = 0,
              b_premium_3    = 0,

              # Class 4
              asc_sq_4       = 0,
              b_deaths_4     = 0,
              b_qol_4        = 0,
              b_premium_4    = 0,

              # Class membership
              delta_1          = 0,
              delta_2          = 0,
              delta_3          = 0,
              delta_4          = 0
              )

apollo_fixed = c("delta_4")

# Define latent class components
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["asc_sq"]] = list(asc_sq_1, asc_sq_2, asc_sq_3, asc_sq_4)
  lcpars[["b_deaths"]] = list(b_deaths_1, b_deaths_2, b_deaths_3, b_deaths_4)
  lcpars[["b_qol"]] = list(b_qol_1, b_qol_2, b_qol_3, b_qol_4)
  lcpars[["b_premium"]] = list(b_premium_1, b_premium_2, b_premium_3, b_premium_4)

  # Utilities of class allocation model
  V=list()
  V[["class_1"]] = delta_1
  V[["class_2"]] = delta_2
  V[["class_3"]] = delta_3
  V[["class_4"]] = delta_4

  # Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_1=1, class_2=2, class_3=3, class_4=4),
    avail        = 1,
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

# Checkpoint for model inputs
apollo_inputs = apollo_validateInputs()

# Define model and likelihood function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Create list of probabilities P
  P = list()

  # Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(A1=1, A2=2, A3=3), 
    avail         = list(A1=1, A2=1, A3=1), 
    choiceVar     = FINAL_CHOICE
  )

  for(s in 1:4){
    V=list()
    V[["A1"]] = b_deaths[[s]] * A1_DEATHS + b_qol[[s]] * A1_QOL + b_premium[[s]] * A1_PREM
    V[["A2"]] = b_deaths[[s]] * A2_DEATHS + b_qol[[s]] * A2_QOL + b_premium[[s]] * A2_PREM
    V[["A3"]] = asc_sq[[s]] + b_qol[[s]] * A3_QOL

    mnl_settings$utilities = V
    mnl_settings$componentName = paste0("Class_",s)

    # Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    # Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
  }

  # Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)

  # Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Searching for starting value
apollo_beta = apollo_searchStart(apollo_beta,
                                  apollo_fixed,
                                  apollo_probabilities,
                                  apollo_inputs,
                                  searchStart_settings=list(nCandidates=100))

# Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Output model results
modelOutput_setting=list(printPVal=2)
apollo_modelOutput(model, modelOutput_setting)

# Save model results
apollo_saveOutput(model, modelOutput_setting)