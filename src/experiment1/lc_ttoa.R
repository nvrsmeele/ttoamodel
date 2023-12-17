###############################################################
# Experiment 1 "Health Insurance Policy Decisions":
# Estimating the TTOA-LC model with three classes.
# Two alternatives: A1 and A2.
###############################################################

# Reset the global environment
rm(list = ls())

# Load library
library(apollo)

# Initialise code
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "TTOA-LC",
  modelDescr      = "TTOA-LC model with three classes",
  indivID         = "RESPID",
  nCores          = 5,
  seed            = 100,
  outputDirectory = "./src/experiment1/lc_ttoa"
)

# Load data --> change path_data to location of data file on your local computer
path_data = "./data/experiment1/healthinsurance_dce.csv"
database = read.csv(path_data, header=TRUE)

# Define model parameters; vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(# Class 1
              b_deaths_1     = 0,
              b_premium_1    = 0,
              b_taboo_1      = 0,
              # Class 2
              b_deaths_2     = 0,
              b_premium_2    = 0,
              b_taboo_2      = 0,
              # Class 3
              b_deaths_3     = 0,
              b_premium_3    = 0,
              b_taboo_3      = 0,
              # Class membership
              delta_1        = 0,
              gamma_age_med_1  = 0,
              gamma_age_high_1  = 0,
              gamma_risk2_low_1 = 0,
              gamma_risk2_med_1 = 0,
              gamma_risk2_high_1 = 0,
              gamma_risk2_ehigh_1 = 0,
              gamma_relig_1 = 0,
              delta_2        = 0,
              gamma_age_med_2  = 0,
              gamma_age_high_2  = 0,
              gamma_risk2_low_2 = 0,
              gamma_risk2_med_2 = 0,
              gamma_risk2_high_2 = 0,
              gamma_risk2_ehigh_2 = 0,
              gamma_relig_2 = 0,
              delta_3        = 0,
              gamma_age_med_3  = 0,
              gamma_age_high_3  = 0,
              gamma_risk2_low_3 = 0,
              gamma_risk2_med_3 = 0,
              gamma_risk2_high_3 = 0,
              gamma_risk2_ehigh_3 = 0,
              gamma_relig_3 = 0
              )

# Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_3", "gamma_age_med_3", "gamma_age_high_3", "gamma_risk2_low_3", "gamma_risk2_med_3", "gamma_risk2_high_3",
                 "gamma_risk2_ehigh_3", "gamma_relig_3")

# Define latent class components
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["b_deaths"]] = list(b_deaths_1, b_deaths_2, b_deaths_3)
  lcpars[["b_premium"]] = list(b_premium_1, b_premium_2, b_premium_3)
  lcpars[["b_taboo"]] = list(b_taboo_1, b_taboo_2, b_taboo_3)
  
  # Utilities of class allocation model
  V=list()
  V[["class_1"]] = delta_1 + gamma_age_med_1 * AGE_2 + gamma_age_high_1 * AGE_3 + gamma_risk2_low_1 * RISKAVERS_2_2 + 
                   gamma_risk2_med_1 * RISKAVERS_2_3 + gamma_risk2_high_1 * RISKAVERS_2_4 + gamma_risk2_ehigh_1 * RISKAVERS_2_5 +
                   gamma_relig_1 * RELIGION

  V[["class_2"]] = delta_2 + gamma_age_med_2 * AGE_2 + gamma_age_high_2 * AGE_3 + gamma_risk2_low_2 * RISKAVERS_2_2 +
                   gamma_risk2_med_2 * RISKAVERS_2_3 + gamma_risk2_high_2 * RISKAVERS_2_4 + gamma_risk2_ehigh_2 * RISKAVERS_2_5 +
                   gamma_relig_2 * RELIGION

  V[["class_3"]] = delta_3 + gamma_age_med_3 * AGE_2 + gamma_age_high_3 * AGE_3 + gamma_risk2_low_3 * RISKAVERS_2_2 +
                   gamma_risk2_med_3 * RISKAVERS_2_3 + gamma_risk2_high_3 * RISKAVERS_2_4 + gamma_risk2_ehigh_3 * RISKAVERS_2_5 +
                   gamma_relig_3 * RELIGION
  
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
    V[["A1"]] = b_deaths[[s]] * A1_DEATHS + b_premium[[s]] * A1_PREM + b_taboo[[s]] * A1_TABOO
    V[["A2"]] = b_deaths[[s]] * A2_DEATHS + b_premium[[s]] * A2_PREM + b_taboo[[s]] * A2_TABOO

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
saveOutput_settings=list(saveEst=TRUE, saveCorr=TRUE, saveCov=TRUE, saveModelObject=TRUE, printPVal=2)
apollo_saveOutput(model, saveOutput_settings)
