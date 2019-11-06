# Open-Data-Energy-Benchmarking
Code for the paper: Examining the feasibility of using open data to benchmark building energy usage in cities: a data science and policy perspective

### Lasso_RandomForest.Rmd
This file contains the code for the main analysis of the paper. Specifically, this notebook runs a lasso regression and random forest model on the ten city datasets and the CBECS dataset. Additional functions are added to compute variable importance from theses models, which are run on each of the dataset independently. The datasets read in this file can be found in each of the subdirectories in this repository. 

### geocode.Rmd
This file contains the code used to geocode datasets to detemine their lat/long. Geocoding was done for only a subset of the city datasets where no unique building identifier was included so that multiple datasets could be merged. This notebook uses the Stanford arcgis API for geocoding.

### Final_Vars_Accounting.xlsx
This file contains the final values for feature importance for each dataset. The feature importance graphs in the paper are also in this file. The functions to determine the feature importance values can be found in the Lasso_RandomForest.Rmd notebook.

### BenchmarkingPoliciesGraphs.R
This file contains the code to build several of the figures in the paper. The URL for the data used to build figure 1 is at the top of the script. Several iterations were constructed to build Figure 2, where plot #6 in the script is the code for the final version of this plot. 

### CBECS_Analysis.R
This file contains the main analysis for the CBECS dataset. Several iterations of models (lasso and random forests) were constructed on different subsets of data (e.g., by building type, census region). 

## Dataset Directories
Each of the directories contains the final cleaned dataset for the named city. In each directory, there is an R notebook that shows the cleaning process for that city. Many of the directories contain both the raw (i.e., public) and finalized (i.e., cleaned) datasets, however, many of the raw datasets were not included due to their large size. You can find the URLs for the raw datasets in Appendix B in the journal paper. The cleaning process is also provided in more detail in the paper. 
