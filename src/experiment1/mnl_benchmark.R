###############################################################
# Experiment 1 "Health Insurance Policy Decisions":
# Estimating the benchmark MNL model.
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
  modelName       = "Benchmark_MNL",
  modelDescr      = "Benchmark MNL model",
  indivID         = "RESPID",
  outputDirectory = "./src/experiment1/mnl_benchmark"
)

# Load data --> change path_data to location of data file on your local computer
path_data = "./data/experiment1/healthinsurance_dce.csv"
database = read.csv(path_data, header=TRUE)

# Initialise model params
apollo_beta=c(b_deaths     = 0,
              b_premium    = 0)

# Fixed params: should be in quotes (optional)
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
  V[["A1"]]  = b_deaths * A1_DEATHS + b_premium * A1_PREM
  V[["A2"]]  = b_deaths * A2_DEATHS + b_premium * A2_PREM
  
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
apollo_saveOutput(model, modelOutput_setting)