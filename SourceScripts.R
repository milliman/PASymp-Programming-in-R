## You can include a master-script that sources all the others

# Run the script to load the libraries
source('LoadLibraries.R')

# Run the script to create the model data set
source('ManipulateData.R')

# Run the script to fit the model
source('FitModel.R')

# Run the script to plot model validation
source('PlotResults.R')