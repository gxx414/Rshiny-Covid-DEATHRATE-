library(shiny)
library(DT)
library(shinycssloaders)
library(visdat)
library(vcd)
library(ggplot2)
library(summarytools)
library(corrgram)
library(rpart)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(car)
library(ggrepel)
library(recipes)
library(glmnet)
library(plotly)


## load data
dat <- read.csv("Ass2Data.csv", header = TRUE, na.strings = c(''))



# identify all possible missing values
dat[dat == -99] <- NA
dat[dat == "--"] <- NA
dat[dat == 'NA'] <- NA



## identify "Not Applicable" categorical missing values
dat$HEALTHCARE_BASIS <- as.character(dat$HEALTHCARE_BASIS)
dat$HEALTHCARE_BASIS[is.na(dat$HEALTHCARE_BASIS)] <- "NONE"
dat$HEALTHCARE_BASIS <- as.factor(dat$HEALTHCARE_BASIS)


## identify "Not Applicable" numeric missing values
dat$HEALTHCARE_COST_SHADOW <- as.numeric(is.na(dat$HEALTHCARE_COST))
dat$HEALTHCARE_COST <- as.character(dat$HEALTHCARE_COST)
dat$HEALTHCARE_COST[is.na(dat$HEALTHCARE_COST)] <- 0
dat$HEALTHCARE_COST <- as.numeric(dat$HEALTHCARE_COST)

## type data
dat$COUNTRY = as.character(dat$COUNTRY)
dat$POPULATION = as.numeric(as.character(dat$POPULATION))
dat$AGE25PROP = as.numeric(as.character(dat$AGE25PROP))
dat$AGEMEDIAN = as.numeric(as.character(dat$AGEMEDIAN))
dat$AGE55PROP = as.numeric(as.character(dat$AGE55PROP))
dat$POPDENSITY = as.numeric(as.character(dat$POPDENSITY))
dat$GDP2019 = as.numeric(as.character(dat$GDP2019))
dat$INFANTMORT = as.numeric(as.character(dat$INFANTMORT))
dat$DOC10 = as.numeric(as.character(dat$DOC10))
dat$VAXRATE = as.numeric(as.character(dat$VAXRATE))


# classify numeric variables and categorical variables
choices = colnames(as.data.frame(dat))
choicesA = choices[c(2,12)]
choicesB = choices[c(3:11,13,14)] 

# to calculate the ratio of missingness of a vector
pmiss <- function(x){ sum(is.na(x))/length(x)*100 }
vratio <- apply(dat, MARGIN= 2,FUN=pmiss)










