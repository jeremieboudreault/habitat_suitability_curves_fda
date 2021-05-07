A new look at habitat suitability curves through FDA 👓
================================================================================


+ A scientific research by __Jeremie Boudreault__, André St-Hilaire, Normand Bergeron and Fateh Chebana
+ This project is part of my master studies at [Institut National de la Recherche Scientifique](http://inrs.ca)
+ All scripts and data are made freely available here under the [Creative Common License ![](https://i.creativecommons.org/l/by-nc-nd/4.0/80x15.png)](http://creativecommons.org/licenses/by-nc-nd/4.0/)
+ Any questions should be sent to __Jeremie.Boudreault [at] ete.inrs [dot] ca__


Data
--------------------------------------------------------------------------------


Data consists of a __field survey__ that have been conducted during summer 2017 on the  __Sainte-Marguerite River__ (SMR) and __Petite-Cascapedia River__ (PCR) :

+ `data/field-data/*` : contains the two raw `xlsx` files filled after each day of field work.
+ `data/gps/*` : locations of the studied sites for the two rivers.


R scripts
--------------------------------------------------------------------------------


Scripts are all from Jeremie Boudreault. They make the use of the R package `FDboost` for fitting __functional regression models__ (FRM) :

+ `s01_clean_field_data.R` : clean the field data spreadsheets and produced cleaned datasets.
+ `s02_compute_salmon_numbers.R`: convert measured salmon fork-lengths to number of fry and parr.
+ `s03_select_study_case.R`: select optimal data subset for this project.
+ `s04_map_studied_sites.R`: map sites studied in this project.
+ `s05_generate_fd_curves.R`: generate (functional) curves of availability and selection/preference.
+ `s06_prepare_data_modelling.R`: prepare data prior to fit function regression models (FRM).
+ `s07_fit_function_models.R`: fit FRMs to curves using a parallelized k-fold cross-validation. 
+ `s08_define_optimal_stopping.R`: choose optimal early stopping parameter.
+ `s09_extract_frm_coefficients.R`: extract coefficients surfaces from fitted FRM.
+ `s10_compute_coef_bootstrap.R`: compute standard-error of FRM coefficients using bootstrap. 
+ `s11_build_classical_models.R`: build classical local and regional models of HSC.
+ `s12_assess_models_performance.R`: calculate some goodness-of-fit metrics for the fitted models. 


___Enjoy !___ ✌🏻