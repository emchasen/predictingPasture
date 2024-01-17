The scripts in this repo consist of the code needed to download and clean SSURGO data used for the pasture yield prediction model associated with the Grasslands 2.0 project.

The first script needed is make_WI_grass_soil.R. This script downloads SSURGO data with the package FedData, and then cleans the data by selecting variables of interest and creates new variables by summarizing properties for the first 30 cm of soil depth.

The second script needed is featureElimination.R. This script uses the data file created in make_WI_grass_soil.R and creates and random forest model to predict yield from soil properties and grass species. It reduces the number of features in the model by removing soil variables with mostly NAs, little or no variance, and highly correlated variables.


The last script needed is the figures.R script which was used to create all of the figures in the manuscript.