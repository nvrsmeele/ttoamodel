###############################################################
# Experiment 2 "Organ Transplantation Policy Decisions":
# Preprocessing the SP dataset for choice analysis.
###############################################################

# Reset the global environment
rm(list = ls())

# Load library
library(fastDummies)

# Set paths for loading/saving data files
file_path = "./data/experiment2/organtransplantation_dce_raw.csv"
file_save = "./data/experiment2/organtransplantation_dce.csv"

# Load data
data = read.csv(file_path, header=TRUE)

# Generate dummies for all covariates
dummy_cols_list = c("AGE", "EDUC", "HH_INC", "HH_COMP", "DONOR_DEREGIST", "RISKAVERS_1", "RISKAVERS_2",
                    "RISKAVERS_3", "MORALDIM_1", "MORALDIM_2", "MORALDIM_3", "MORALDIM_4", "MORALDIM_5",
                    "MORALDIM_6", "MORALDIM_7", "MORALDIM_8")

full_data = dummy_cols(data, select_columns=dummy_cols_list, remove_selected_columns=FALSE)

# Checkpoint
message("--- Overview of the data ---")
cat("Number of respondents:", length(unique(full_data$RESPID)), "\n")
cat("Number of obsverations:", length(full_data$RESPID), "\n")

# Save data for final SP dataset
write.csv(full_data, file=file_save, row.names=FALSE)
message("---- Dataset is saved!! ----")
