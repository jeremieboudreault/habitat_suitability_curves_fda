# main.R


# Run all script to get results for this project -------------------------------


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0


# Data cleaning and tidying ----------------------------------------------------


# Step 1.
message("Step 1 :: Cleaning field data.")
#source(file.path("R", "s01_clean_field_data.R"))

# Step 2.
message("Step 2 :: Compte salmon numbers.")
#source(file.path("R", "s02_compute_salmon_numbers.R"))

# Step 3.
message("Step 3 :: Select study case.")
#source(file.path("R", "s03_select_study_case.R"))

# Step 4.
message("Step 4 :: Map studied sites.")
#source(file.path("R", "s04_map_studied_sites.R"))


# Prepare data for modelling ---------------------------------------------------


# Step 5.
message("Step 5 :: Generate functional curves.")
source(file.path("R", "s05_generate_fd_curves.R"))

# Step 6.
message("Step 6 :: Prepare data for modelling.")
source(file.path("R", "s06_prepare_data_modelling.R"))


# Fitting models ---------------------------------------------------------------


# Step 7.
message("Step 7 :: Fitting functional regression models.")
source(file.path("R", "s07_fit_functional_models_kfold.R"))

# Step 8.
message("Step 8 :: Prepare data for modelling.")
source(file.path("R", "s08_define_optimal_stopping.R"))

# Step 9.
message("Step 9 :: Compute coefficient standard error with bootstrapping..")
source(file.path("R", "s09_compute_coef_bootstrap.R"))


# Generate results -------------------------------------------------------------


# Step 10.
message("Step 10 :: Plot FRM coefficients.")
source(file.path("R", "s10_plot_frm_coefficients.R"))

# Step 11.
message("Step 11 :: Build traditional HSC models.")
source(file.path("R", "s11_build_classical_models.R"))

# Step 12.
message("Step 12 :: Asses models performance.")
source(file.path("R", "s12_assess_models_performance.R"))

# Step 13.
message("Step 13 :: Compute HSI.")
source(file.path("R", "s13_compute_hsi.R"))
