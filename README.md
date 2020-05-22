## Habitat suitability curves through functional data analysis 🏞

* A scientific research by **Jeremie Boudreault**, André St-Hilaire, Normand Bergeron and Fateh Chebana
* This is a **side project** completed after my master degree at [**Institut National de la Recherche Scientifique**](http://inrs.ca) (INRS)
* All **codes** and **data** are freely available here under the [**Creative Common License** ![](https://i.creativecommons.org/l/by-nc-nd/4.0/80x15.png)](http://creativecommons.org/licenses/by-nc-nd/4.0/)
* Questions regarding the code or the data should be sent to **Jeremie.Boudreault@ete.inrs.ca**

## Data

Data are from **field survey** that have been conducted during summer 2017 on the  **Sainte-Marguerite River** (SMR) and **Petite-Cascapedia River** (PCR):

* `data/field/*` : contains the two raw .xlsx file filled after each day of field work 
* `data/*` : contains the cleaned and transformed datasets 

## R codes

Codes are all from Jeremie Boudreault. They make the use of the R package `FDboost` for fitting **functional regression models** (FRM) :

* `R/Data_initial_cleaning.R` : code to clean the field data spreadsheets and produce more adapted datasets
* `R/Data_salmons_numbers.R`: code to convert the salmon lengths to number of fry and parr
* `R/Data_fun_obs.R`: code to obtain functional observations of HSC curves

## Out

A folder for the **results** at each part of the coding process :

* `out/data visualisation/*` : raw data visualisation and tables
* `out/fda models/*` : fitted final models
* `out/coefficients/*` : coefficients of the models
* `out/predictions/*` : leave-one-out predictions and goodness-of-fit
