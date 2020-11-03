# SimpleSlopes Package
This package is created based on suggestions by Dawson & Richter (2006) for three way interactions. 
It is useful for plotting, checking the significance of slopes from zero, and checking the differences among slopes. 

# How to Install on Your Local Device:
Step 1: install devtools if you dont have it on your devide

`if (!require(devtools)) install.packages('devtools')`

Step 2: 

`install_github("Saklehir/SimpleSlopes")`

# How to Use the Package
Step 1: Load the package
library(SimpleSlopes)

Step 2: Create an `lm` object with three way interactions (continous variables)
Using the airquality dataset in R, we are going to predict `Ozone` by three way interaction of solar radiation (`Solar.R`), wind (`wind`) and temperature (`temp`)

`lm.model <- lm(Ozone ~ Solar.R + Solar.R:wind + Solar.R:temp + wind:temp + Solar.R:wind:temp, data = airquality)`

Step 3: Plot the simple slopes (scores are based on 1 SD higher and lower than the mean for moderator):

`slopePlot(model=lm.model, xvar = 'Solar.R', modvars = c('wind', 'temp'), modNames = c('Wind', 'Temperature'), xlab = 'Solar Radiation', ylab = 'Ozone Level')`

Here `modNames` refers to the moderator names you want to appear in figure legend. Instead of using the ones in the dataset, you can specify the moderating variable names for publishability purposes. `xlab` and `ylab` refer to the predictor and outcome variables respectively. Just like moderators, you can specify predictor and outcome variables to be used in the figure.
 
Step 4: Check if simple slopes are significantly different than zero:

`slopeTest(model=lm.model, xvar = 'Solar.R', modvars = c('wind', 'temp'))`

Step 5: Check if the slopes are significantly different than each other (pairwise slope differences):

`slopeDifference(model=lm.model, xvar = 'Solar.R', modvars = c('wind', 'temp'))`

# How to Cite
Bakaç, C. (2020). SimpleSlopes: An R package for probing 3-way interactions (version 0.1.0). Retrieved from https://github.com/Saklehir/SimpleSlopes
