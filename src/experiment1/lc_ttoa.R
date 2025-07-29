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
# Model: Latent Class Logit (LCL) with TTOA classes
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
  modelName       = "TTOA-LC",
  modelDescr      = "LC model with ttoa params across classes",
  indivID         = "RESPID",
  nCores          = 5,
  seed            = 100,
  outputDirectory = "./src/experiment1/results/lc_ttoa"
)

# Load data
path_data = "./data/experiment1/processed_data/main/healthinsurance_dce.csv"
database = read.csv(path_data, header=TRUE)

# Define dummy variables for age
database$AGE_2 = ifelse(database$AGE==2,1,0)
database$AGE_3 = ifelse(database$AGE==3,1,0)

# Initialise model params
## First, estimate TTOA-LC model without class membership covariates
## Then, use the estimated parameters as starting values for the TTOA-LC model with class membership covariates

apollo_beta=c(# Class 1
              b_deaths_1        = -0.06978,
              b_premium_1       = -0.45282,
              b_taboo_neg_1     = -3.03600,
              b_taboo_pos_1     = 0,

              # Class 2
              b_deaths_2        = -0.02377,
              b_premium_2       = -0.05776,
              b_taboo_neg_2     = -0.92179,
              b_taboo_pos_2     = -0.30323,

              # Class 3
              b_deaths_3        = -0.14610,
              b_premium_3       = -0.04992,
              b_taboo_neg_3     = -1.97819,
              b_taboo_pos_3     = -1.35248,

              # Class membership
              delta_1           = 0,
              gamma_relig_1     = 0,
              gamma_age_med_1   = 0,
              gamma_age_high_1  = 0,
              gamma_moral1_1    = 0,
              gamma_moral2_1    = 0,
              gamma_moral3_1    = 0,
              gamma_moral4_1    = 0,
              gamma_moral5_1    = 0,
              gamma_moral6_1    = 0,

              delta_2           = 0,
              gamma_relig_2     = 0,
              gamma_age_med_2   = 0,
              gamma_age_high_2  = 0,
              gamma_moral1_2    = 0,
              gamma_moral2_2    = 0,
              gamma_moral3_2    = 0,
              gamma_moral4_2    = 0,
              gamma_moral5_2    = 0,
              gamma_moral6_2    = 0,

              delta_3           = 0,
              gamma_relig_3     = 0,
              gamma_age_med_3   = 0,
              gamma_age_high_3  = 0,
              gamma_moral1_3    = 0,
              gamma_moral2_3    = 0,
              gamma_moral3_3    = 0,
              gamma_moral4_3    = 0,
              gamma_moral5_3    = 0,
              gamma_moral6_3    = 0
              )

apollo_fixed = c("delta_3", "b_taboo_pos_1", "gamma_relig_3", "gamma_age_med_3",
                 "gamma_age_high_3", "gamma_moral1_3", "gamma_moral2_3", "gamma_moral3_3",
                 "gamma_moral4_3", "gamma_moral5_3", "gamma_moral6_3")

# Define latent class components
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["b_deaths"]] = list(b_deaths_1, b_deaths_2, b_deaths_3)
  lcpars[["b_premium"]] = list(b_premium_1, b_premium_2, b_premium_3)
  lcpars[["b_taboo_neg"]] = list(b_taboo_neg_1, b_taboo_neg_2, b_taboo_neg_3)
  lcpars[["b_taboo_pos"]] = list(b_taboo_pos_1, b_taboo_pos_2, b_taboo_pos_3)

  # Utilities of class allocation model
  V=list()
  V[["class_1"]] = delta_1 + gamma_relig_1 * RELIGION + gamma_age_med_1 * AGE_2 + gamma_age_high_1 * AGE_3 +
                   gamma_moral1_1 * MORALDIM_1 + gamma_moral2_1 * MORALDIM_2 + gamma_moral3_1 * MORALDIM_3 +
                   gamma_moral4_1 * MORALDIM_4 + gamma_moral5_1 * MORALDIM_5 + gamma_moral6_1 * MORALDIM_6

  V[["class_2"]] = delta_2 + gamma_relig_2 * RELIGION + gamma_age_med_2 * AGE_2 + gamma_age_high_2 * AGE_3 +
                   gamma_moral1_2 * MORALDIM_1 + gamma_moral2_2 * MORALDIM_2 + gamma_moral3_2 * MORALDIM_3 +
                   gamma_moral4_2 * MORALDIM_4 + gamma_moral5_2 * MORALDIM_5 + gamma_moral6_2 * MORALDIM_6

  V[["class_3"]] = delta_3 + gamma_relig_3 * RELIGION + gamma_age_med_3 * AGE_2 + gamma_age_high_3 * AGE_3 +
                   gamma_moral1_3 * MORALDIM_1 + gamma_moral2_3 * MORALDIM_2 + gamma_moral3_3 * MORALDIM_3 +
                   gamma_moral4_3 * MORALDIM_4 + gamma_moral5_3 * MORALDIM_5 + gamma_moral6_3 * MORALDIM_6

  # Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_1=1, class_2=2, class_3=3),
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
    alternatives  = c(A1=1, A2=2), 
    avail         = list(A1=1, A2=1), 
    choiceVar     = CHOICE
  )

  for(s in 1:3){
    V=list()
    V[["A1"]] = b_deaths[[s]] * A1_DEATHS + b_premium[[s]] * A1_PREM + b_taboo_neg[[s]] * A1_TABOO_NEG + b_taboo_pos[[s]] * A1_TABOO_POS
    V[["A2"]] = b_deaths[[s]] * A2_DEATHS + b_premium[[s]] * A2_PREM + b_taboo_neg[[s]] * A2_TABOO_NEG + b_taboo_pos[[s]] * A2_TABOO_POS

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