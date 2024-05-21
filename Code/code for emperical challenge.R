library(glmnet)
library(corrplot)
library(ggplot2)


'''
Question 1

To get a better idea of the overall trends in abortions and crime, provide some descriptive
statistics and visualize the trends over time. Your clients express concerns about the
data quality for Alaska (state ID 2), DC (state ID 9), and Hawaii (state ID 12). In
addition, they would like to restrict your sample to the years 1985 to 1997.
'''

# Reading the data
df = "abortion.dat"
OGdata <- read.delim(df, header = TRUE, sep="\t") 
data <- read.delim(df, header = TRUE, sep="\t") 
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')


# Include only the years with all data
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
ggplot(data[data$state %in% c(1:15),], aes(x = year, y = y_murd, color = factor(state))) +
  geom_line() +
  labs(title = "Murder Rate Over Time",
       x = "Year",
       y = "Murder Rate")
scale_color_discrete(name = "State")
# We can see that the murder rate is abnormally high for state 9


# To explain this anomaly, let's look at a comparison between states 2,9,12 and states 6,7
ggplot(data[data$state %in% c(2,9,12,6,7),], aes(x = year, y = y_murd, color = factor(state))) +
  geom_line() +
  labs(title = "Murder Rate Over Time",
       x = "Year",
       y = "Murder Rate")
scale_color_discrete(name = "State")

ggplot(data[data$state %in% c(2,9,12,6,7),], aes(x = year, y = pov, color = factor(state))) +
  geom_line() +
  labs(title = "Murder Rate Over Time",
       x = "Year",
       y = "Murder Rate")
scale_color_discrete(name = "State")

# As we can see from the graph here, 2 and 12 are not so bad and are following the general trend like the remaining
# states. But, state 9 alone stands out possibly due to poverty.

# So let's go ahead and exclude states 9 along with 2 and 12 as well because it has been found that these three states
# have poor data quality
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data$pop <- log(data$pop)

t <- data$year - 85
s <- factor(data$state) ## the states are numbered alphabetically

controls <- data.frame(data[,c(3,10:17)])
y <- data$y_murd
d <- data$a_murd
processedDF <- data.frame(data[,c(3,6,7,10:17)])

# Descriptive statistics of the processed dataset
summary(processedDF)
# So we then had a look at the values of our dataset after pre-processing to understand 
# what kind of numbers we are dealing with


# Correlation plot. We are avoiding NA values by checking if it's a pairwise-complete observation
core <- data.frame(data[,c(3,6,7,10:17)])
cor_matrix <- cor(core, use="pairwise.complete.obs")
corrplot(cor_matrix, method = "color")


# Here we are doing descriptive statistics of variables that have a strong relationship with y_murd
# as seen in the correlation plot
boxplot(data$a_murd, main = "Boxplot of Population", ylab = "Population")
boxplot(data$prison, main = "Boxplot of Prison", ylab = "Prison")
boxplot(data$pov, main = "Boxplot of Poverty", ylab = "Poverty")


'''
Question 2

Replicate the original study by Donohue and Levitt (2001), who used fairly detailed
linear regression models 2 That is, regress the murder rate y_murd on the relevant
abortion index a_murd as well as all control variables contained in the data. In addition,
please include state fixed effects, and a linear time trend. Estimate this model via OLS.
What do you conclude from the results? Does the abortion rate affect the murder rate?
Are the results significant?
'''

#linear regression
lr <- lm(y ~ d + t + s +., data=controls)
summary(lr)
# coefficient of d is negative which proves that increase in abortion rates decrease crime. Additionally,
# it is statistically very significant as shown in the summary with the no. of stars
# For the state fixed effects, where the coefficients are positive, it means the crime rates
# have increased. The significance varies from state to state.
# For the linear time trend, we observe that the crime rates have increased 
# over time. Linear time trend does show a high statistical significance

'''
Question 3

One of the politicians remembers his undergraduate econometrics class and argues that
we should always add as many regressors as we plausibly can in order to control for
as many confounders as possible. He asks you to add the following variables into your
linear regression model. Do your estimation results change?
'''

# Using all the possible regressors in order to control for all possible confounders
fit2 <- lm(lpc_murd ~ ., data = OGdata)
summary(fit2)

# Compare it with our earlier estimations
summary(lr)

# Yes, our estimation results change. There is a significant change for gunlaw in that for the 
# dataset with all possible regressors the coefficient is positive but in the modified dataset
# the relationship is negative. Similarly with prison. Rest of the variables share differences
# in the magnitude of their values but with the directions same

'''
Question 4

Another senator proposes the use of a LASSO model to better handle the relatively
large set of control variables. Estimate the specification from Question 2 using LASSO.
What do the estimates say about the effect of abortion rates on murder rates? Do you
have any concerns about this model?
'''

# Lasso
x <- model.matrix(~d + t + s +., data=controls, alpha=1)
dim(x)
# Fit LASSO model
lasso_model <- cv.glmnet(x, y, alpha = 1)  # Use alpha = 1 for LASSO

# Plot the cross-validated mean squared error (MSE) as a function of lambda
plot(lasso_model)
# Extract coefficients from the LASSO model
coef(lasso_model)

# The dots in the coefficients indicate that those are ones which are not significant (or basically zero). 
# We have a lot of independent variables so it makes to sense to use LASSO

lasso_model$lambda.min # Min will set values to zero
optimal_lambda <- lasso_model$lambda.1se # Better to use this as it's better in practice


optimal_model <- glmnet(x, y, alpha = 1, lambda = optimal_lambda)
coef(optimal_model)

# Here, again, our coefficient of d is negative which proves that increase in abortion rates decrease crime.
# No, we don't have any concerns about this model particularly because we have only 13 observations
# for every state fixed effects for 17 regressors. The LASSO models addresses this issue by essentially removing less 
# significant regressors, thereby making our model more fit for the size of our data by selecting only important control variables

'''
Question 5

If the goal is to control for as many confounders as possible, and still obtain a statistically
reliable estimate of the causal effect of abortion on crime, which model/statistical
procedure would you employ here? Can you carefully walk us through the different
conceptual steps involved and the code? Estimate this model and explain the results.
Are your conclusions qualitatively different from the ones obtained by the original
Donohue and Levitt paper?
'''

# In order to find an alternate model while controlling for as many confounders as possible while also
# obtaining a statistically reliable estimate of the causal effect of abortion on crime, let's compare
# Linear regression, LASSO model and The Orthogonal ML model. 

# So we find that between Linear Regressio and LASSO, LASSO would be more suitable because it removes
# the statistically less significant regressors from our dataset. Thereby, giving us a better estimation 
# results.



'''
# orthogonal ML
# install.packages('AER', dependencies = TRUE, repos='http://cran.rstudio.com/')
source("orthoML.R")
dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }

yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }

resids <- orthoPLTE( x=x, d=d, y=y, 
                     dreg=dreg, yreg=yreg, nfold=5)

'''
resids


'''
Question 6

A group of ultra-conservative representatives has already announced to attack your
clients’ initiative. They hired a different firm to make a case that instead of increasing
access to abortions, congress should facilitate access to mobile phones. Rumor has it
that they already prepared an empirical study that shows a significantly negative effect
of mobile phone penetration on crime rates. Can you replicate their analysis, i.e., take
your model from Question 2 and replace the abortion rate with the cellphone penetration
rate? Do you think the conservative representatives have a plausible argument? How
would you help your clients to rebut such an analysis?
'''


cell <- read.csv("us_cellphone.csv")
# center on 1985 and scale by 1997-1985
cellrate <- (5*cell[,2])/(1000*(cell[,3]) )

# The reason why we are dividing by 1000 is because we want to normalize the values of cellphones to be 
# between 0 and 1, so that it can be compared on the same scale as the abortion values

cellrate
## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)

plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")

phone <- cellrate[t+1]
phone
## clearly, cellphones fight crime.
summary(tech <- glm(y ~ phone + s + t+., data=controls))$coef['phone',]
1 - exp(-0.372e-01)
#0.001860362 = -1.860362e-03

# We have implemented a glm model which is generalized linear model that is flexible to how the nature
# of our data is.
# what is happening here is that graph looks like that of a quadratic equation's,
## but we have no other controls that do so. So, we need
## to allow quadratic trends that could be caused by other confounding variables.
## So, we now introduce interaction between the controls such as an interact of the nation-wide phone
## variable with state dummies to allow for state specific tech adoption.

# Quadratic terms
t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]
## Abortion sign has switched direction (and is insignif)!
dim(model.matrix(formula(interact), data=controls))
## we have very few observations relative to number of parameters.

# We find that conservative representative had a seemingly convincing argument
# because of the negative coefficient but as we saw in the graph it had a more 
# quadratic equation pattern to it. Upon introducing an interaction term between
# the state and the mobile phone penetration, we find the coefficient is actually
# positive in reality. If it's positive, well, the group of ultra-conservative
# representatives' argument does not hold any merit. Boom.



'''
Question 7

Finally, think about what else the politicians might learn from the data. What future
steps would you suggest to take in order to improve their legislative initiative?
'''

