####-----------------------------------------------------------------------------####
#   title: "IEOR 142 Group Project -- Time Depenencies"                             #
#   author: "Elias Castro Hernandez, and Darren Fang"                               #
#   date: "December 2017"                                                           # 
#   purpose: Perform Time Series analysis and other time dependant processes        #
####-----------------------------------------------------------------------------####


#### Load Libraries/Packages ####
library(dplyr)             # Basic Data Manipulation
library(ggplot2)           # Plotting
library(GGally)            # ggplot2 Feature Extension
library(tidyverse)         # Data Cleanup and Manipulation
library(plotly)            # Time Series Plotting
library(reshape2)          # Alternive melting to dplyr
# Time Series Related Libraries
library(ggfortify)         # Time Series
library(lubridate)         # Additional Time Series Package
library(utils)             # Multiple Time Series Plot
library(xts)               # Time-Stamp Data Manipulation
library(KFAS)              # Time-Series Averages


#### Reading and exploration of data ####
setwd('C:/Users/Alaina Castro/Desktop/elias_to_delete/macro_data')
mo_AM <- read.csv("Monthly_Ames_and_Macro_Data.csv")

# Minor cleaning of data
str(mo_AM) # check data type, and ordering of "Date"
mo_AM <- mo_AM %>% mutate(Date = mdy(Date))


#### Appearance settings ####
theme_set(theme_bw())  # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation


#### Global Functions: Time Series ####

# Out of sample R^2
OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
  }

# R^2 with a particular baseline
BaselineR2 <- function(predictions, truth, baseline) {
  SSE <- sum((truth - predictions)^2)
  SST <- sum((truth - baseline)^2)
  r2 <- 1 - SSE/SST
  return(r2)
  }

# plots time series data and super imposes an average trend line
tsPlot<- function(df, x, y, xlab, ylab, caption, lineColor, dotColor, byTime, title, subtitle){
  
  # type coercion
  as.Date(df$Date, "%m/d/%Y")
  
  # required manipulations for plotting
  tsP <- ggplot(df, aes(x=x, y=y)) +
    geom_smooth(colour = "grey", size = 5, alpha = .5) +   
    geom_point(aes(colour = dotColor)) +
    stat_summary(fun.y = mean, geom="line", colour = lineColor, size = 1.25)
  
  # aesthetics for plotting  
  tsP + scale_x_date(date_breaks = byTime, date_labels = "%b-%Y") + 
    labs(title=title, 
         subtitle=subtitle, 
         caption= caption, 
         y=ylab, x= xlab) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),
          panel.grid.minor = element_blank()) +
    theme(legend.title=element_blank())
}

# Plots variable(s) over time. If more than one, variables are stacked
edaPlots <- function(df, id, byTime, xlab, ylab, title, subtitle){
  # type coercion
  change <- melt(df, id = id)
  
  # Plots a stack of variable(s) change(s) over time
  edaP <- qplot(Date, value, data = change, geom = "line", group = variable, colour = variable) +
    facet_grid(variable ~ ., scale = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.1),
          panel.grid.minor = element_blank()) +                      # Rotate x-axis text
    theme(axis.text.y.right = element_text(angle = 180, vjust=0.1),
          panel.grid.minor = element_blank())                        # Rotate y-axis text
  
  edaP + scale_x_date(date_breaks = byTime, date_labels = "%b-%Y") +
    labs(title = title, 
         subtitle = subtitle,
         caption = "Region: Ames, Iowa, 2006-2010",
         y = ylab, x = xlab)  
}
#### ----------------------------- ####



#################################################
#### Ames Macroeconomic Time Series Analysis ####
#################################################

## EDA: Response and variables over time ##
df <- mo_AM
id <- "Date"
byTime <- "1 year" # must be lower case
xlab <- "Year"
ylab <- "Values"
title <- "Variable Changes Over Time"
subtitle <- "Monthly Macroecomic Facors (Ames, Iowa)"

# Funtion plots EDA
edaPlots(df, id, byTime, xlab, ylab, title, subtitle)
## ------------------------------------- ##


## Time Series Plot of inital data ##
df1 <- mo_AM
x1 <-initial.tsp$Date
y1 <- initial.tsp$SalePrice
xlab1 <- "Date"
ylab1 <- "Sale Price"
caption1 <- "Region: Ames, Iowa, 2006-2010"
lineColor1 <- "red"
dotColor1 <- rnorm(2931) # number of observations 
byTime1 = "1 year"  # must be in lower case
title1 <- "Time Series Model"
subtitle1 <- "Baseline Formulation"

# Function plots time series data and average trend
tsPlot(df1, x1, y1, xlab1, ylab1, caption1, lineColor1, dotColor1, byTime1, title1, subtitle1)
tsp <- tsPlot(df1, x1, y1, xlab1, ylab1, caption1, lineColor1, dotColor1, byTime1, title1, subtitle1) +
  theme(legend.position="none") +        # remove legend
  theme(axis.title.x = element_blank())+ # remove x-axis
  theme(plot.caption = element_blank())  # remove caption
## ------------------------------- ##


## Data Split: from Jan/2006 to Dec/2008 as testing data ##
am.Train <- mo_AM %>% filter(year(Date) < 2009)
am.Test <- mo_AM %>% filter(year(Date) > 2009)
## ----------------------------------------------------- ##


## Linear Trend Model (LTM) - Time Series Analysis ##
# LTM.1: Training data -- Make a new column for the time period
salesTrainLM <- am.Train %>% mutate(TimePeriod = seq_len(n()))

# LTM.2: Build linear trend model
modLM <- lm(SalePrice~TimePeriod, data=salesTrainLM)
# numerical summary
summary(modLM)

# LTM.3: Plot linear trend model
df2 <- salesTrainLM       # note prefix has changed to salesTrainLM
x2 <-salesTrainLM$Date
y2 <- salesTrainLM$SalePrice
xlab2 <- "Date"
ylab2 <- "Sale Price"
caption2 <- "Region: Ames, Iowa, 2006-2010"
lineColor2 <- "gold"
dotColor2 <- rnorm(1941) # number of observations  in salesTrainLM
byTime2 = "1 year"  # must be in lower case
title2 <- "Time Series Model"
subtitle2 = "Linear Trend Model"

# Plots LTM
tsPlot(df2, x2, y2, xlab2, ylab2, caption2, lineColor2, dotColor2, byTime2, title2, subtitle2)
ltp <- tsPlot(df2, x2, y2, xlab2, ylab2, caption2, lineColor2, dotColor2, byTime2, title2, subtitle2) +
  theme(legend.position="none") +        # remove legend
  theme(axis.title.y = element_blank())+ # remove y-axis
  theme(axis.ticks.y = element_blank())+ # remove y-tick marks
  theme(axis.text.y = element_blank()) + # remove y-axis-text
  theme(axis.title.x = element_blank())+ # remove x-axis
  theme(plot.caption = element_blank())  # remove caption
## ----------------------------------------------- ##


## side-by-side comparison of plots ##
require(gridExtra)
grid.arrange(tsp, ltp, ncol=2)
## -------------------------------- ##


## Random Walk Model (RWM) - Time Series Analysis ##
# RWM.1: Training data -- Make a new column for the time period, include Sales Yersterday
salesTrainRW <- am.Train %>% mutate(SalesYesterday = c(NA, head(SalePrice, -1)))

# RWM.2: Random Walk model
df3 <- salesTrainRW       # note prefix has changed to salesTrainRF
x3 <-salesTrainRW$Date
y3 <- salesTrainRW$SalePrice
xlab3 <- "Date"
ylab3 <- "Sale Price"
caption3 <- "Region: Ames, Iowa, 2006-2010"
lineColor3 <- "green"
dotColor3 <- rnorm(1941) # number of observations  in salesTrainRW
byTime3 = "1 year"  # must be in lower case
title3 <- "Time Series Model"
subtitle3 = "Randoma Walk Model"

tsPlot(df3, x3, y3, xlab3, ylab3, caption3, lineColor3, dotColor3, byTime3, title3, subtitle3)
rwp <- tsPlot(df3, x3, y3, xlab3, ylab3, caption3, lineColor3, dotColor3, byTime3, title3, subtitle3) +
  theme(legend.position="none") +        # remove legend
  theme(axis.title.y = element_blank())+ # remove y-axis
  theme(axis.ticks.y = element_blank())+ # remove y-tick marks
  theme(axis.text.y = element_blank()) + # remove y-axis-text
  theme(axis.title.x = element_blank())+ # remove x-axis
  theme(plot.caption = element_blank())  # remove caption
## ---------------------------------------------- ##


## side-by-side comparison of plots ##
require(gridExtra)
grid.arrange(tsp, ltp, rwp, ncol=3)
## -------------------------------- ##
