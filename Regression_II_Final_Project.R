

##########################################
#         Relevant Libraries             #
##########################################

library(ResourceSelection)
library(dplyr)
library(readr)
library(MASS)
library(nnet)
library(VGAM)
library(pscl)
library(lmtest)
library(psych)
library(caret)

##########################################
#           Dataset Set up               #
##########################################

# set your personal working directory
# setwd("/Users/carinadoyle/Downloads")

midus <- read.csv("MIDUS_III_Final_Exam_Fall2023_data.csv")
head(midus)

##########################################
#         Univariate Analysis            #
##########################################

#smoke
max(midus$smoke)
min(midus$smoke)
median(midus$smoke)
mean(midus$smoke)
sd(midus$smoke)
var(midus$smoke)
range(midus$smoke)
frequency(midus$smoke)

#male
max(midus$male)
min(midus$male)
median(midus$male)
mean(midus$male)
sd(midus$male)
var(midus$male)
range(midus$male)
frequency(midus$male)

#Age
max(midus$age)
min(midus$age)
median(midus$age)
mean(midus$age)
sd(midus$age)
var(midus$age)
range(midus$age)
frequency(midus$age)

#heart
max(midus$heart)
min(midus$heart)
median(midus$heart)
mean(midus$heart)
sd(midus$heart)
var(midus$heart)
range(midus$heart)
frequency(midus$heart)

#cigage
max(midus$cigage)
min(midus$cigage)
median(midus$cigage)
mean(midus$cigage)
sd(midus$cigage)
var(midus$cigage)
range(midus$cigage)
frequency(midus$cigage)

#alcage
max(midus$alcage)
min(midus$alcage)
median(midus$alcage)
mean(midus$alcage)
sd(midus$alcage)
var(midus$alcage)
range(midus$alcage)
frequency(midus$alcage)

#depress
max(midus$depress)
min(midus$depress)
median(midus$depress)
mean(midus$depress)
sd(midus$depress)
var(midus$depress)
range(midus$depress)
frequency(midus$depress)

#bp
max(midus$bp)
min(midus$bp)
median(midus$bp)
mean(midus$bp)
sd(midus$bp)
var(midus$bp)
range(midus$bp)
frequency(midus$bp)

#exercise
max(midus$exercise)
min(midus$exercise)
median(midus$exercise)
mean(midus$exercise)
sd(midus$exercise)
var(midus$exercise)
range(midus$exercise)
frequency(midus$exercise)

#health
max(midus$health)
min(midus$health)
median(midus$health)
mean(midus$health)
sd(midus$health)
var(midus$health)
range(midus$health)
frequency(midus$health)

##########################################
#          Bivariate Analysis            #
##########################################

#heart
variables <- c("smoke", "male", "age", "cigage", "alcage", "depress", "bp", "exercise")
for (variable in variables) {
  contingency_table <- table(midus$heart, midus[[variable]])
  result <- chisq.test(contingency_table)
  
  cat("Chi-square test between 'heart' and", variable, ":\n")
  print(result)
  cat("\n")
}

#health
variables_health <- c("smoke", "male", "age", "cigage", "alcage", "depress", "bp", "exercise")
for (variable in variables_health) {
  contingency_table_health <- table(midus$health, midus[[variable]])
  result_health <- chisq.test(contingency_table_health)
  
  cat("Chi-square test between 'health' and", variable, ":\n")
  print(result_health)
  cat("\n")
}

##########################################
#             Model 1                    #
##########################################

sum(midus$heart==1)
# 474
sum(midus$heart==0)
# 1595

# Full model with all possible IVs
full_model <- glm(heart~age+male+cigage+smoke+alcage+depress+exercise+factor(health), family="binomial", data=midus)
summary(full_model)
# Call:
# glm(formula = heart ~ age + male + cigage + smoke + alcage + 
#       depress + exercise + factor(health), family = "binomial", 
#     data = midus)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8628  -0.7347  -0.5293  -0.3037   2.8666  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     -4.702029   0.447550 -10.506  < 2e-16 ***
#   age              0.052856   0.005491   9.627  < 2e-16 ***
#   male             0.435503   0.119460   3.646 0.000267 ***
#   cigage          -0.047924   0.014293  -3.353 0.000800 ***
#   smoke            0.212155   0.122245   1.735 0.082653 .  
#   alcage          -0.007883   0.015151  -0.520 0.602850    
#   depress          0.130133   0.208834   0.623 0.533193    
#   exercise         0.173072   0.145885   1.186 0.235482    
#   factor(health)2  0.689538   0.130398   5.288 1.24e-07 ***
#   factor(health)3  1.251280   0.166103   7.533 4.95e-14 ***
#   factor(health)4  1.699127   0.233847   7.266 3.70e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2179.0  on 1978  degrees of freedom
# Residual deviance: 1938.6  on 1968  degrees of freedom
# AIC: 1960.6
# 
# Number of Fisher Scoring iterations: 4

# Backwards stepwise model selection to determine IVs to use
backwards <- step(full_model)
# Start:  AIC=1960.57
# heart ~ age + male + cigage + smoke + alcage + depress + exercise + 
#   factor(health)
# 
# Df Deviance    AIC
# - alcage          1   1938.8 1958.8
# - depress         1   1939.0 1959.0
# - exercise        1   1940.0 1960.0
# <none>                1938.6 1960.6
# - smoke           1   1941.6 1961.6
# - cigage          1   1950.4 1970.4
# - male            1   1952.0 1972.0
# - factor(health)  3   2030.2 2046.2
# - age             1   2037.5 2057.5
# 
# Step:  AIC=1958.84
# heart ~ age + male + cigage + smoke + depress + exercise + factor(health)
# 
# Df Deviance    AIC
# - depress         1   1939.2 1957.2
# - exercise        1   1940.3 1958.3
# <none>                1938.8 1958.8
# - smoke           1   1942.0 1960.0
# - cigage          1   1952.3 1970.3
# - male            1   1953.5 1971.5
# - factor(health)  3   2030.5 2044.5
# - age             1   2039.4 2057.4
# 
# Step:  AIC=1957.22
# heart ~ age + male + cigage + smoke + exercise + factor(health)
# 
# Df Deviance    AIC
# - exercise        1   1940.7 1956.7
# <none>                1939.2 1957.2
# - smoke           1   1942.4 1958.4
# - cigage          1   1952.9 1968.9
# - male            1   1953.5 1969.5
# - factor(health)  3   2035.6 2047.6
# - age             1   2039.5 2055.5
# 
# Step:  AIC=1956.74
# heart ~ age + male + cigage + smoke + factor(health)
# 
# Df Deviance    AIC
# <none>                1940.7 1956.7
# - smoke           1   1943.7 1957.7
# - male            1   1954.2 1968.2
# - cigage          1   1954.7 1968.7
# - factor(health)  3   2036.3 2046.3
# - age             1   2040.0 2054.0

# We will be using the IVs smoke, male, cigage, health, and age
fit <- glm(heart ~ age+cigage+smoke+male+factor(health), data=midus, family="binomial")
summary(fit)
# Call:
#   glm(formula = heart ~ age + cigage + smoke + male + factor(health), 
#       family = "binomial", data = midus)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8726  -0.7351  -0.5371  -0.3113   2.8280  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -4.67321    0.40226 -11.617  < 2e-16 ***
#   age              0.05165    0.00536   9.637  < 2e-16 ***
#   cigage          -0.05041    0.01394  -3.615 0.000300 ***
#   smoke            0.20977    0.12196   1.720 0.085443 .  
#   male             0.42316    0.11578   3.655 0.000257 ***
#   factor(health)2  0.68822    0.13022   5.285 1.26e-07 ***
#   factor(health)3  1.25656    0.16407   7.659 1.88e-14 ***
#   factor(health)4  1.71077    0.23061   7.418 1.19e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2179.0  on 1978  degrees of freedom
# Residual deviance: 1940.7  on 1971  degrees of freedom
# AIC: 1956.7
# 
# Number of Fisher Scoring iterations: 4

# Calculating Model 1 coefficients and confidence intervals
exp(cbind(OR = coef(fit), confint(fit)))
#                         OR      2.5 %    97.5 %
# (Intercept)     0.009342241 0.00421127 0.0203995
# age             1.053009817 1.04209925 1.0642385
# cigage          0.950837716 0.92473615 0.9767066
# smoke           1.233394161 0.97245956 1.5690577
# male            1.526782904 1.21748835 1.9171736
# factor(health)2 1.990169656 1.54266388 2.5710338
# factor(health)3 3.513308733 2.54637444 4.8468866
# factor(health)4 5.533205911 3.51710058 8.7014224

# Calculating Model 1 log likelihood
logLik(fit)
# 'log Lik.' -970.3707 (df=8)

# Calculating Model 1 residuals through pearson and deviance calculation
residuals(fit, "pearson")
residuals(fit, "deviance")

par(mfrow= c(2,2))
stdres<-residuals(fit,type='pearson')
stdres_0<-stdres[midus$heart==0]
midus_0<-midus[midus$heart==0,]
graph1<-plot(midus_0$age,stdres_0, ylab='standardized residuals', xlab='age')

stdres<-residuals(fit,type='pearson')
stdres_1<-stdres[midus$heart==1]
midus_1<-midus[midus$heart==1,]
graph2<-plot(midus_1$age,stdres_1, ylab='standardized residuals', xlab='age')

hoslem.test(fit$y, fitted(fit), g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  fit$y, fitted(fit)
# X-squared = 18.923, df = 8, p-value = 0.01528

# Interaction term model: Smoke*Male
fit2 <- glm(heart~age+cigage+factor(health)+smoke+male+smoke*male, data=midus, family="binomial")
summary(fit2)
# Call:
#   glm(formula = heart ~ age + cigage + factor(health) + smoke + 
#         male + smoke * male, family = "binomial", data = midus)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8883  -0.7361  -0.5270  -0.3188   2.7635  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     -4.411960   0.416593 -10.591  < 2e-16 ***
#   age              0.050621   0.005374   9.420  < 2e-16 ***
#   cigage          -0.050480   0.013967  -3.614 0.000301 ***
#   factor(health)2  0.683449   0.130341   5.244 1.58e-07 ***
#   factor(health)3  1.253780   0.164326   7.630 2.35e-14 ***
#   factor(health)4  1.690503   0.231813   7.293 3.04e-13 ***
#   smoke           -0.075796   0.175095  -0.433 0.665099    
#   male             0.055523   0.201484   0.276 0.782876    
#   smoke:male       0.541877   0.243373   2.227 0.025979 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2179.0  on 1978  degrees of freedom
# Residual deviance: 1935.8  on 1970  degrees of freedom
# AIC: 1953.8
# 
# Number of Fisher Scoring iterations: 4

# Calculating Model 2 coefficients and confidence intervals
exp(cbind(OR = coef(fit2), confint(fit2)))
#                        OR       2.5 %     97.5 %
# (Intercept)     0.01213138 0.005316028 0.02724214
# age             1.05192418 1.040996615 1.06316925
# cigage          0.95077249 0.924615469 0.97666886
# factor(health)2 1.98069686 1.534952914 2.55934761
# factor(health)3 3.50356144 2.538014513 4.83583441
# factor(health)4 5.42220565 3.438292621 8.54644372
# smoke           0.92700563 0.659136696 1.31039546
# male            1.05709368 0.711666049 1.56947021
# smoke:male      1.71923151 1.067039656 2.77213430

# Calculating Model 2 log likelihood
logLik(fit2)
# 'log Lik.' -967.8924 (df=9)

# Calculating Model 2 residuals
residuals(fit2, "pearson")
residuals(fit2, "deviance")

par(mfrow= c(2,2))
stdres<-residuals(fit2,type='pearson')
stdres_0<-stdres[midus$heart==0]
midus_0<-midus[midus$heart==0,]
graph1<-plot(midus_0$age,stdres_0, ylab='standardized residuals', xlab='age')

stdres<-residuals(fit2,type='pearson')
stdres_1<-stdres[midus$heart==1]
midus_1<-midus[midus$heart==1,]
graph2<-plot(midus_1$age,stdres_1, ylab='standardized residuals', xlab='age')

hoslem.test(fit2$y, fitted(fit2), g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  fit2$y, fitted(fit2)
# X-squared = 16.204, df = 8, p-value = 0.03955

# Model Comparison

fit0 <- glm(heart~1,family="binomial", data=midus) 
anova(fit0, fit)
anova(fit0, fit2)
anova(fit, fit2)

##########################################
#             Model 2                    #
##########################################

# Poisson Regression and Negative Binomial Regression

# Convert 'health' to a factor variable
midus$health <- as.numeric(midus$health)

# Obtain the descriptive statistics of the outcome variable 'health' and check the assumption
describe(midus$health)
# vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
# X1    1 1979 1.74 0.87      2    1.62 1.48   1   4     3 0.98     0.14 0.02

# There is underdispersion in the data as mean (1.74) is not equal to variance (0.87^2 = 0.7569).
# In a Poisson distribution, the mean is equal to the variance. 
# If the variance is substantially less than the mean, it might indicate that there is less variability 
# in the data than the Poisson model assumes.

# Fit a Poisson regression model
poisson_model <- glm(health ~ depress + alcage + cigage + age + bp,
                     data = midus,
                     family = "poisson")
# Display summary of the Poisson model
summary(poisson_model)
# Call:
#   glm(formula = health ~ depress + alcage + cigage + age + bp, 
#       family = "poisson", data = MIDUS)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  0.323573   0.120255   2.691  0.00713 ** 
#   depress      0.288036   0.056283   5.118 3.09e-07 ***
#   alcage      -0.001130   0.004633  -0.244  0.80732    
#   cigage       0.001421   0.004097   0.347  0.72873    
#   age          0.001148   0.001645   0.698  0.48531    
#   bp           0.240511   0.035330   6.808 9.93e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 789.11  on 1978  degrees of freedom
# Residual deviance: 712.82  on 1973  degrees of freedom
# AIC: 5468.7
# 
# Number of Fisher Scoring iterations: 4

# Showing the list of Odds Ratios and Confidence Intervals for each predictor
exp(cbind(OR = coef(poisson_model), confint(poisson_model)))
#                   OR     2.5 %   97.5 %
# (Intercept) 1.3820570 1.0919724 1.749602
# depress     1.3338048 1.1925821 1.487078
# alcage      0.9988707 0.9896956 1.007819
# cigage      1.0014218 0.9933445 1.009421
# age         1.0011486 0.9979215 1.004378
# bp          1.2718985 1.1869272 1.363249

# It is better to consider fitting a negative binomial model because it 
# has an additional parameter that allows for overdispersion or 
# underdispersion compared to a Poisson distribution.

# Fit a negative binomial regression model
nb_model <- glm.nb(health ~ depress + alcage + cigage + age + bp,
                   data = midus)

# Display summary of the Negative Binomial model
summary(nb_model)
# Call:
#   glm.nb(formula = health ~ depress + alcage + cigage + age + bp, 
#          data = MIDUS, init.theta = 80735.98539, link = log)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  0.323573   0.120257   2.691  0.00713 ** 
#   depress      0.288036   0.056284   5.118 3.10e-07 ***
#   alcage      -0.001130   0.004633  -0.244  0.80732    
#   cigage       0.001421   0.004097   0.347  0.72873    
#   age          0.001148   0.001645   0.698  0.48532    
#   bp           0.240511   0.035330   6.807 9.93e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Negative Binomial(80735.99) family taken to be 1)
# 
# Null deviance: 789.09  on 1978  degrees of freedom
# Residual deviance: 712.80  on 1973  degrees of freedom
# AIC: 5470.8
# 
# Number of Fisher Scoring iterations: 1
# 
# 
# Theta:  80736 
# Std. Err.:  272686 
# Warning while fitting theta: iteration limit reached 
# 
# 2 x log-likelihood:  -5456.758 

# Showing the list of Odds Ratios and Confidence Intervals for each predictor
exp(cbind(OR = coef(nb_model), confint(nb_model)))
#                   OR     2.5 %   97.5 %
# (Intercept) 1.3820566 1.0919695 1.749606
# depress     1.3338049 1.1925804 1.487080
# alcage      0.9988707 0.9896955 1.007819
# cigage      1.0014218 0.9933444 1.009421
# age         1.0011486 0.9979215 1.004378
# bp          1.2718985 1.1869264 1.363250

# Comparison of the Poisson model and the Negative Binomial Model

lrtest(poisson_model, nb_model)
# Likelihood ratio test
# 
# Model 1: health ~ depress + alcage + cigage + age + bp
# Model 2: health ~ depress + alcage + cigage + age + bp
# #Df  LogLik Df Chisq Pr(>Chisq)
# 1   6 -2728.4                    
# 2   7 -2728.4  1 0.026      0.872

# Comparison of the two models using AIC and BIC
list(c(AIC(poisson_model), AIC(nb_model)))
# 5468.732 5470.758

list(c(BIC(poisson_model), BIC(nb_model)))
# 5502.274 5509.890


# Test for the overdispersion parameter
LL <- as.numeric(2 * (logLik(poisson_model) - logLik(nb_model)))
pchisq(LL, df = 4 - 3, lower.tail = FALSE)

###################################################################################

# Ordered Multinomial Logistic Regression

# In this case study, the variable health seems to have ordered categories 
# (e.g., "excellent," "good/very good," "fair," "poor"). 
# This suggests that an ordered logistic regression (proportional odds model) is appropriate.

# Make sure 'health' is a factor with appropriate levels
midus$health <- factor(midus$health, levels = c(1, 2, 3, 4))

# Create a train/test split because we are bulding a predictive model
set.seed(123)
trainIndex <- createDataPartition(midus$health, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- midus[ trainIndex,]
test_data  <- midus[-trainIndex,]

# Fit the full multinomial logistic regression model 
full_model <- vglm(health ~ age + male + heart + cigage + smoke + alcage + depress + bp + exercise,
                   family = cumulative(parallel = TRUE), data = train_data)

# Display summary of the full multinomial logistic regression model
summary(full_model)
# Call:
#   vglm(formula = health ~ age + male + heart + cigage + smoke + 
#          alcage + depress + bp + exercise, family = cumulative(parallel = TRUE), 
#        data = train_data)
# 
# Coefficients: 
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1  0.569776   0.387282   1.471 0.141233    
# (Intercept):2  2.322078   0.392527   5.916 3.30e-09 ***
#   (Intercept):3  3.850562   0.406713   9.468  < 2e-16 ***
#   age            0.008351   0.005156   1.620 0.105267    
#   male           0.111295   0.109991   1.012 0.311607    
#   heart         -0.939901   0.125592  -7.484 7.22e-14 ***
#   cigage        -0.009787   0.012154  -0.805 0.420661    
#   smoke         -0.403188   0.110638  -3.644 0.000268 ***
#   alcage        -0.007069   0.015306  -0.462 0.644172    
#   depress       -1.153520   0.197713  -5.834 5.40e-09 ***
#   bp            -0.861565   0.109700  -7.854 4.04e-15 ***
#   exercise       0.154799   0.137348   1.127 0.259721    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Names of linear predictors: logitlink(P[Y<=1]), logitlink(P[Y<=2]), logitlink(P[Y<=3])
# 
# Residual deviance: 2945.291 on 4149 degrees of freedom
# 
# Log-likelihood: -1472.645 on 4149 degrees of freedom
# 
# Number of Fisher scoring iterations: 5 
# 
# No Hauck-Donner effect found in any of the estimates
# 
# 
# Exponentiated coefficients:
#   age      male     heart    cigage     smoke    alcage   depress        bp  exercise 
# 1.0083864 1.1177247 0.3906666 0.9902604 0.6681867 0.9929556 0.3155241 0.4225003 1.1674230 

# Step 1: Fit the main effects multinomial logistic regression  model:
main_effects_model <- vglm(health ~ depress + alcage + cigage + age + bp,
                           family = cumulative(parallel = TRUE),
                           data = train_data)

# Display summary of the main effects model
summary(main_effects_model)
# Call:
# vglm(formula = health ~ depress + alcage + cigage + age + bp, 
#     family = cumulative(parallel = TRUE), data = train_data)
# 
# Coefficients: 
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1  0.7199626  0.3631308   1.983   0.0474 *  
# (Intercept):2  2.3950986  0.3687792   6.495 8.32e-11 ***
# (Intercept):3  3.8751572  0.3835518  10.103  < 2e-16 ***
# depress       -1.1772353  0.1951932  -6.031 1.63e-09 ***
# alcage        -0.0000571  0.0149181  -0.004   0.9969    
# cigage        -0.0074305  0.0118744  -0.626   0.5315    
# age           -0.0013925  0.0049352  -0.282   0.7778    
# bp            -0.9849711  0.1076593  -9.149  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Names of linear predictors: logitlink(P[Y<=1]), logitlink(P[Y<=2]), logitlink(P[Y<=3])
# 
# Residual deviance: 3018.905 on 4153 degrees of freedom
# 
# Log-likelihood: -1509.453 on 4153 degrees of freedom
# 
# Number of Fisher scoring iterations: 5 
# 
# No Hauck-Donner effect found in any of the estimates
# 
# 
# Exponentiated coefficients:
#   depress    alcage    cigage       age        bp 
# 0.3081294 0.9999429 0.9925971 0.9986085 0.3734500

# Step 2: Testing the Assumption about Response Probabilities

# Predict probabilities for the test dataset for the full model
probabilities_full <- predict(full_model, newdata = test_data, type = "response")

# Snippet of the probabilities obtained for the full model with all predictors
head(probabilities_full)
# 1         2          3          4
# 1 0.0835046 0.2609842 0.36339550 0.29211566
# 2 0.6325402 0.2759578 0.07012680 0.02137521
# 3 0.7128300 0.2218844 0.05036458 0.01492096
# 4 0.6046459 0.2935340 0.07782570 0.02399441
# 5 0.6941518 0.2348795 0.05467242 0.01629628
# 6 0.5887831 0.3032076 0.08242158 0.02558769

# Sum of predicted probabilities for each row
row_sums_full <- rowSums(probabilities_full)
# Check if the sum is approximately 1 for each row
all(round(row_sums_full, 3) == 1)

# Similarly, we test for the main effects model.

# Predict probabilities for the test dataset of the main effects model
probabilities_main <- predict(main_effects_model, newdata = test_data, type = "response")

# Sum of predicted probabilities for each row
row_sums_full <- rowSums(probabilities_main)

# Check if the sum is approximately 1 for each row
all(round(row_sums_full, 3) == 1)

# This checks if the model's predicted probabilities for each health category add up to approximately 1, 
# ensuring the model's reliability in estimating the likelihood of different health outcomes.
# Here, it is true for both the full and main effects models.

# Step 3: predict probabilities for those with diagnosed high blood pressure if 
# (1) age = 64 and depress = 0, and 
# (2) if age = 64 and depress = 1. 

# Report the difference in probabilities per group (i.e., excellent, good, fair, poor).

# Assuming 'test_data$health' contains the true categories for the test set
# Select specific conditions for comparison
conditions_depress_0 <- data.frame(age = 64, male = 0, heart = 0, cigage = mean(test_data$cigage),
                                   smoke = 0, alcage = mean(test_data$alcage), depress = 0,
                                   bp = mean(test_data$bp), exercise = mean(test_data$exercise))

conditions_depress_1 <- data.frame(age = 64, male = 0, heart = 0, cigage = mean(test_data$cigage),
                                   smoke = 0, alcage = mean(test_data$alcage), depress = 1,
                                   bp = mean(test_data$bp), exercise = mean(test_data$exercise))

# Predict probabilities for the specified conditions
predicted_probs_depress_0 <- predict(full_model, newdata = conditions_depress_0, type = "response")
predicted_probs_depress_1 <- predict(full_model, newdata = conditions_depress_1, type = "response")

# Display the difference in probabilities per group
diff_probs <- predicted_probs_depress_1 - predicted_probs_depress_0
head(diff_probs)
# 1         2         3          4
# 1 -0.2789205 0.1152181 0.1149011 0.04880128

# For the category "Excellent," the probability decreases by approximately 0.28.
# For the category "Good," the probability increases by approximately 0.12.
# For the category "Fair," the probability increases by approximately 0.11.
# For the category "Poor," the probability increases by approximately 0.05.

# CONCLUSION:
# Among individuals diagnosed with high blood pressure at age 64, those with depression 
# are less likely to rate their health as "Excellent" 
# and more likely to rate it as "Good," "Fair," or "Poor" compared to those without depression.


# Different comparison techniques to show that the full model is better:

# Likelihood ratio test for comparing main effects and full models
lrtest(main_effects_model, full_model)
# Likelihood ratio test
# 
# Model 1: health ~ depress + alcage + cigage + age + bp
# Model 2: health ~ age + male + heart + cigage + smoke + alcage + depress + 
#   bp + exercise
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1 4153 -1509.5                         
# 2 4149 -1472.7 -4 73.614  3.912e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# INTERPRETATION:
# The likelihood ratio test compares two models: 
# Model 1 with main effects (depress, alcage, cigage, age, bp) and 
# Model 2 with additional variables (male, heart, smoke, exercise). 
# The test yields a chi-squared statistic of 73.614 with 4 degrees of freedom 
# and a highly significant p-value (3.912e-15), indicating that the full model (Model 2) 
# significantly improves the fit over the main effects model (Model 1).


# Comparison of the two models using ANOVA:
anova(full_model, test = "LRT")
# Analysis of Deviance Table (Type II tests)
# 
# Model: 'cumulative', 'VGAMordinal', 'VGAMcategorical'
# 
# Links: 'logitlink'
# 
# Response: health
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# age       1    2.607      4150     2947.9 0.1064257    
# male      1    1.017      4150     2946.3 0.3133447    
# heart     1   55.459      4150     3000.8 9.541e-14 ***
# cigage    1    0.599      4150     2945.9 0.4390490    
# smoke     1   13.370      4150     2958.7 0.0002557 ***
# alcage    1    0.203      4150     2945.5 0.6520298    
# depress   1   31.628      4150     2976.9 1.867e-08 ***
# bp        1   63.163      4150     3008.4 1.903e-15 ***
# exercise  1    1.295      4150     2946.6 0.2551403    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(main_effects_model, test = "LRT")
# Analysis of Deviance Table (Type II tests)
# 
# Model: 'cumulative', 'VGAMordinal', 'VGAMcategorical'
# 
# Links: 'logitlink'
# 
# Response: health
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# depress  1   33.497      4154     3052.4 7.136e-09 ***
# alcage   1    0.000      4154     3018.9    0.9970    
# cigage   1    0.370      4154     3019.3    0.5432    
# age      1    0.080      4154     3019.0    0.7777    
# bp       1   86.238      4154     3105.1 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# INTERPRETATION:

# The ANOVA test comparing the full and main effects models indicates significant improvement 
# in model fit with the inclusion of additional variables 
# (male, heart, smoke, exercise) in the full model.
# Each variable's contribution is assessed, revealing 
# that heart, smoke, depress, bp, and exercise significantly contribute (p < 0.05) 
# to explaining variance in the health outcome.
# This suggests that considering these additional factors enhances 
# the model's predictive ability for self-rated health in midlife individuals.

# Comparison of the two models via visualization:
plot(full_model)

plot(main_effects_model)

# Compare AIC and BIC of the full and main effects models:
AIC(full_model)
# 2969.291
BIC(full_model)
# 3032.109

AIC(main_effects_model)
# 3034.905
BIC(main_effects_model)
# 3076.784

# INTERPRETATION:

# The AIC and BIC values for the full model are 2969.291 and 3032.109, respectively.
# The AIC and BIC values for the main effects model are 3034.905 and 3076.784, respectively.
# Lower AIC and BIC values indicate better model fit. In this comparison, the full model exhibits 
# lower AIC and BIC, suggesting it has a better balance between goodness of fit and 
# model complexity compared to the main effects model.