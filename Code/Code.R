install.packages("corrplot")
library(corrplot)
library(ggplot2)

# Reading the data
crimeData <- read.table("abortion.dat", header = TRUE, sep = "\t")

'''
Question 1

To get a better idea of the overall trends in abortions and crime, provide some descriptive
statistics and visualize the trends over time. Your clients express concerns about the
data quality for Alaska (state ID 2), DC (state ID 9), and Hawaii (state ID 12). In
addition, they would like to restrict your sample to the years 1985 to 1997.
'''

# This will give us an idea on how the values of each attribute are with the help of min, max, mean, median
summary(crimeData)

# Plotting the abortion rate over time
########################################## Descriptive Statistics and Trends to be visualized
plot(crimeData$year, crimeData$lpc_murd, xlab = "Year", ylab = "Murdee-Weighted Abortion Rate")
# Here it looks like the abortion rate did increase over time

# A correlation plot for all the variables. We are avoiding NA values by checking if it's a pairwise-complete observation
cor_matrix <- cor(crimeData, use="pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

# lpc_murd has strong relationship with ... variables
# Heatmap
heatmap(cor_matrix)

'''
Question 2

Replicate the original study by Donohue and Levitt (2001), who used fairly detailed
linear regression models 2 That is, regress the murder rate y_murd on the relevant
abortion index a_murd as well as all control variables contained in the data. In addition,
please include state fixed effects, and a linear time trend. Estimate this model via OLS.
What do you conclude from the results? Does the abortion rate affect the murder rate?
Are the results significant?
'''


'''
Question 3

One of the politicians remembers his undergraduate econometrics class and argues that
we should always add as many regressors as we plausibly can in order to control for
as many confounders as possible. He asks you to add the following variables into your
linear regression model. Do your estimation results change?
'''

# Comparing the 
lr1 <- lm(y ~ d)
summary(lr1)

fit2 <- lm(lpcmurd ~ ., data = crimeData)
summary(fit2)
# R automatically deletes the observations with missing values
################# We need to compare the how this does with the original estimates


'''
Question 4

Another senator proposes the use of a LASSO model to better handle the relatively
large set of control variables. Estimate the specification from Question 2 using LASSO.
What do the estimates say about the effect of abortion rates on murder rates? Do you
have any concerns about this model?
'''



'''
Question 5

If the goal is to control for as many confounders as possible, and still obtain a statistically
reliable estimate of the causal effect of abortion on crime, which model/statistical
procedure would you employ here? Can you carefully walk us through the different
conceptual steps involved and the code? Estimate this model and explain the results.
Are your conclusions qualitatively different from the ones obtained by the original
Donohue and Levitt paper?
'''

Linear Regression is still suitable because our output consists of continuous values. 
