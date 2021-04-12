A new look at habitat suitability curves through FDA 👓
================================================================================


+ A scientific research by __Jeremie Boudreault__, André St-Hilaire, Normand Bergeron and Fateh Chebana
+ This is a __side project__ completed after my master degree at [__Institut National de la Recherche Scientifique__](http://inrs.ca) (INRS)
+ All __codes__ and __data__ are freely available here under the [__Creative Common License__ ![](https://i.creativecommons.org/l/by-nc-nd/4.0/80x15.png)](http://creativecommons.org/licenses/by-nc-nd/4.0/)
+ Questions regarding the code or the data should be sent to __Jeremie.Boudreault@ete.inrs.ca__


Data
--------------------------------------------------------------------------------


Data consists of a __field survey__ that have been conducted during summer 2017 on the  __Sainte-Marguerite River__ (SMR) and __Petite-Cascapedia River__ (PCR) :

+ `data/field/*` : contains the two raw `xlsx` files filled after each day of field work 


R scripts
--------------------------------------------------------------------------------


Scripts  are all from Jeremie Boudreault. They make the use of the R package `FDboost` for fitting __functional regression models__ (FRM) :

+ `R/s1_clean_field_data.R` : clean the field data spreadsheets and produced cleaned datasets.
+ `R/s2_compute_salmon_numbers.R`: convert measured salmon fork-lengths to number of fry and parr.
+ `R/s3_select_study_case.R`: select optimal data subset for this project.
+ `R/s4_generate_fcurves.R`: generate (functional) curves of availability and selection/preference.
+ `R/s5_fit_function_models.R`: code to fit functional regression models to curves


Results
--------------------------------------------------------------------------------


