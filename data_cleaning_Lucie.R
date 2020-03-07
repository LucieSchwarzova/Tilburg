### Title:    Stats & Methodology project
### Group:    38
### Authors:  Lucie, Jude, Twins
### Created:  2020-02-29
### Modified: xxxx-xx-xx

### 3 Parts
# 1. Data Preprocessing
# 2. Inference
# 3. Prediction

########## 1. Data Preprocessing ##########
#####---------------------------------------------------------------------------#####
setwd("C:/Users/lulus/group_project/code")

### ENVIRONMENT PREPARATION ###

## Clear the workspace:
rm(list = ls(all = TRUE))
cat("\014") # clear the console - found on stackoverflow.com

source("studentFunctions.R")
source("studentSubroutines.R")

set.seed(123) # set random seed to generate same samples

## Load libraries
library(mice) # for missing data treatement
library(MASS) 
library(MLmetrics)
library(mitools)

## Set relative paths
dataDir  <- "../data/" 
fileName <- "wvs_data.rds" 
plotDir <- "../plots/"
## Load data:
data <- readRDS(paste0(dataDir, fileName))

## Check the data, basic statistics and structure of the objects in dataset
head(data)
summary(data)
str(data)


#####---------------------------------------------------------------------------#####
### SUBSET OF DATA
## CHOOSE VARIABLES FOR THE MODELS AND CREATE SUBSET

data1 <- data[ , c("V2", "V8", "V10", "V11","V23","V57", "V58", "V59", "V235", "V238","V239", "V240","V242","V248")]
data1
# We created a subset for prediction 
# We created two sub-datasets, because since we will use Multiple Imputation, we dont want to use non-related variables for replacing the missing values with the fitted one

#####---------------------------------------------------------------------------#####
### 1. 1 MISSING DATA

## According to the documentation, value of -5 means Missing or Inepropriate responses
## All the values that "should" be there are replace with MI method
## In this subset, it concerns values -5, -4,-3,-2 and -1

## Therefore, we need to convert these data into NA
count_missing <- colSums(is.na(data1))
count_missing # we need to convert those wierd values

data1[data1 == -5] <- NA
data1[data1 == -4] <- NA
data1[data1 == -3] <- NA
data1[data1 == -2] <- NA
data1[data1 == -1] <- NA

## Now, we take a look on the number/percentage of missing data for each variable in the dataset:
count_missing <- colSums(is.na(data1)) # is the value inside bfi NA? false = 0, true = 1 --> I can sum it immediately
count_missing # The number of missing variables

proportion_missing <- colMeans(is.na(data1)) # proportions of missing variables = mean from zero(s) and one(s)
proportion_missing 


##########################################################


## Summarization of what we found:
range(proportion_missing) # We can see that there is at least one variable, for which we observe all the cases
# maximum is 0.043 --> for some variable, we dont observe 4.3% of all cases
mean(proportion_missing)
median(proportion_missing)

### Patterns in missing data

## Find missing data patterns:
miss_pat <- md.pattern(data1) 
miss_pat <- as.data.frame(miss_pat)
miss_pat

# each row tells how how much missing data pattern we have
nrow(miss_pat) # 43 different patterns
# first column = how many people has the same missing data pattern
# row names = how many observation for each missing data pattern
# last column = for each missing data pattern, how many variables are missing

# last row shows the count of missing values per variable - we remove the count in this way:
miss_pat[nrow(miss_pat), ] 
# As we can see, the last one is the total missing - we dont need this number, since its important to report missing values variable-wise!
miss_pat[nrow(miss_pat), -ncol(miss_pat)] # we removes last row (vector)
##? uložit do promìnné?s

### Extract the number of missing variables for each data pattern
miss_pat[ , ncol(miss_pat)] # last column - for each missing data pattern, how many variables are missing
miss_pat[-nrow(miss_pat), ncol(miss_pat)] # again, we get rid of the total count

########################################################

### UNIVARIATE OUTLIERS
## Before we start to treat missing data, we can take a look on univariate outliers
## Some of the univariate outliers might be treated as NA, therefore we should replace them afterwads.

### Detect univariate outliers
# For detecting univariate outliers, we will use the bpOut function from studentFunction
# We check for outliers in variables, where It makes sense!
# First let´s see the structure of our variables
str(data1)
# for gender, state number and are you chief at home it doesnt make sense look for outliers
outliers = lapply(data1[ , c("V10", "V11","V23","V57", "V58", "V59", "V238","V239", "V242","V248")], FUN = bpOutliers)
#Tukey's boxplot
# find outliers in NUMERIC columns
outliers$V10$possible
outliers$V10$probable

outliers$V11$possible
outliers$V11$probable

outliers$V23$possible
outliers$V23$probable

outliers$V57$possible
outliers$V57$probable

outliers$V58$possible
outliers$V58$probable # some probable outliers
# we save this outliers into variable to process them further
indices_out_v58 <- outliers$V58$probable # this is set of indices of outliers

outliers$V59$possible
outliers$V59$probable

outliers$V238$possible
outliers$V238$probable

outliers$V239$possible
outliers$V239$probable

outliers$V242$possible
outliers$V242$probable

outliers$V248$possible
outliers$V248$probable

### Let´s take a look on probable outliers in V58 variables (number of children)
# we will use boxplot method
?boxplot()
boxplot(data1$V58,na.action = NULL, horizontal = TRUE, range = 3)
# how many children should be treated as outliers?
# more than x children are se to be outliers
# set the range then!!
# now we have to treat such data
# obecne jsou outliers vic jak 5 tady, zmenit!


# we can set outliers as missing data and then treat missing data with originally missing data

# How to replace those value with NA?
## Function to winsorize univariate outliers from 
## OR we can set them as NA and then impute them with multiple imputation method

data1$V58

length(indices_out_v58) # 276 outliers detected within variable v58
length(indices_out_v58)/length(data1$V58) # approximately 2% of data within this variables are outliers

data1$V58[indices_out_v58] <- NA
data1$V58 # we replace outliers with NA values on given indices

cm_V58 <- colSums(is.na(data1))
cm_V58 # just checking the number of missing values




########################################################


##### COVARIANCE COVERAGE
## Compute covariance coverage:
cc <- md.pairs(data1)$rr / nrow(data1) # rm is givving 16 - number of pople which respond A2 but missing A1
cc

## Summarize coverages:
range(cc)

## Check that coverage exceeds some threshold:
eps <- 0.90
all(cc > eps)
# the covariance coverage is more than 90% in all chosen variables

#####---------------------------------------------------------------------------#####
### 1. 2 OUTLIERS
##### DETECT UNIVARIATE OUTLIERS
### This should be done before imputation, because univariate outliers might be treated as mising data, therefore might be replaced in multiple impuatation
###
target_variables <- c("V2", "V10", "V11","V23","V57", "V58", "V59", "V240","V242","V248")


#####---------------------------------------------------------------------------#####
####### 1. 3 MISSING DATA TREATEMENT









#####---------------------------------------------------------------------------#####

####### FIND multivariate outliers
###
###
#### EDA
# mahalanobis
# zase after imputation
# jednou, ale zase kdyz vice nez 50% of multiply imputed data this observation is an otulier, then 
# 

# number of iteratio 10, nebo 20 datasets  - musime zacit se small cisly
# if density plots rika ze imputation converges = to znamena kouknina grafy
#  nesmime pouzivat norm - predictivemint manching? or proportional

# for unordered je tu taky neco