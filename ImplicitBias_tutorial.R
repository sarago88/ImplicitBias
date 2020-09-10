### Practice your R skills: Analyzng candy ratings ###
### Sara Gottlieb-Cohen, StatLab Manager           ###

# Load packages 

library(tidyverse)
library(rstatix)

# Load the data for Study 1a and Study 2

Study1a <- read_csv("https://raw.githubusercontent.com/sarago88/ImplicitBias/master/Study1a_Data.csv")

Study2 <- read_csv("https://raw.githubusercontent.com/sarago88/ImplicitBias/master/Study2_Data.csv")

## Analyze the data: Study 1a ##

# Subset the data to only include individuals who were not suspicious (Suspicion == 0), and 
# did pay attention (Exclude == 1).
# Also recode the condition variable to make it more interpretable (.5 = Implicit, -.5 = Explicit).






# Confirm that we now have 267 observations.

nrow(Study1a_analysis)

# Create a correlation matrix of accountability, punishment, concern, and reform.
# First create a matrix with only the variables of interest. 






# Perform a series of t-tests to test for whether accountability, punishment,
# concern and reform differ between conditions.







# Visualize the data: create a bar graph with type of judgment (accountability, punishment, 
# concern, reform) on the x-axis, average judgment on the y-axis, and fill bars according to condition.
# Don't forget error bars (+/- 1 standard error).

# The data must be in a summary table prior to graphing it. First reshape the data to long format, 
# then create a summary table, then plot the data.









## Analyze the data: Study 2 ##

# Recode levels of BiasCondition and HarmCondition to read more intuitively
# (.5 = Implicit, -.5 = Explicit; .5 = High, -.5 = Low).
# Subset the data to include only participants that were not suspicious (Suspicion == 0).





# Perform a series of two-way ANOVAs; harm condition and bias condition are the independent
# variables, and accountability/punishment/concern/reform are the dependent variables.









# Replicate Figure 1 to plot the data just for punishment
# Create a bar graph with harm condition on the x-axis, and average
# punishment judgment on the y-axis. Fill the bars by bias condition.






