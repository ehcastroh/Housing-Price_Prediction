####-----------------------------------------------------------------------------####
#   title: "IEOR 142 Group Project -- EDA of Macroecomic Data and Ames Response"    #
#   author: "Elias Castro Hernandez, and Darren Fang"                               #
#   date: "December 2017"                                                           # 
#   purpose: Perform Exploratory Data Analysis (EDA) on Macroecomic/Ames            #
####-----------------------------------------------------------------------------####


#### Load Libraries/Packages ####
library(tidyverse)  # Tibbles
library(dplyr)      # Basic data manipulation
library(ggplot2)    # Plotting
library(GGally)     # ggplot2 Feature Extension
library(tidyverse)  # Data Cleanup and Manipulation
library(stringr)    # String Manipulation
library(graphics)   # Additional graphics package
library(ggExtra)    # install.packages("ggExtra")
library(corrgram)   # Correlation plots
library(readr)      # Writing Data


#### Reading data ####
setwd('C:/Users/Alaina Castro/Desktop/elias_to_delete/macro_data')
mo_AM <- read.csv("Monthly_Ames_and_Macro_Data.csv")
mo_macro  <- read.csv("Ames Macro Monthly Predictors.csv")


#### Global Functions: Correlation ####

# Computes and Plots Correlation 
scatmatPlot <- function(df,title){
  ggscatmat(emp_data, alpha = .8) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  }

# Visual Correlation (the fuller the circle, the stronger the relationship)
corrogram <- function(df){
  tall.df <- as.data.frame(df)
  corrgram(tall.df, type = "data", order=TRUE, lower.panel=panel.ellipse,
           upper.panel=panel.pie, text.panel=panel.txt,
           diag.panel=panel.minmax,
           main="CorroGram\nLog Monthly Macroeconomic Ames Data")
  }

# function plots density plots
dens_Plot <- function(df, x, xlab, ylab, title, subtitle){
  xf <- as.factor(x)
  df <-as.tibble(df)
  temp <- ggplot(df, aes(xf)) + 
    geom_density(aes(fill=factor(xf)), alpha=0.7)
  
  blend(temp, xlab, ylab, title, subtitle) # function call 
  }

# function plots histgram, boxplot, and density
feat_select <- function(df, x, y, xlab, ylab, by_type, by_axis, title, subtitle){
  df <- as.tibble(df)
  temp <- ggplot(df, aes(x, y)) + 
    geom_count() + 
    geom_smooth(method="lm", se=F) +
    labs(title = title, subtitle = subtitle, y = ylab, x = xlab) 
  
  ggMarginal(temp, type = by_type, fill="transparent", margin = by_axis,
             colour = "blue", xparams = list(colour = "orange")) 
  }

# function plots bar graphs
bar_Plot <- function(df, x, y, xlab, ylab, title, subtitle){
  xf <- as.factor(x)
  df <-as.tibble(df)
  temp <- ggplot(df, aes(x=xf, y=y, fill=xf)) + 
    geom_bar(stat="identity") 
  
  blend(temp, xlab, ylab, title, subtitle)
  }

blend <- function(df, xlab, ylab, title, subtitle){
  # cleans up plots
  df + labs(title = title, subtitle = subtitle, x = xlab, y = ylab) + 
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),
          panel.grid.minor = element_blank()) +
    theme(legend.position="none")
}
#### ----------------------------- ####



###########################################
####  Exploratory Data Analysis (EDA)  ####
###########################################

theme_set(theme_bw())  # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation

### Rudimentary numerical analysis ###
# numerical examination
summary(mo_AM)

# considering the log price
log.mo_AM <- read_csv("Log-Monthly_Ames_and_Macro_Data.csv")


## Plot of log relationship ##
# attributes for density plot

title <- "Distribution of Log Sales Price"
subtitle <- "Density"
df <- log.mo_AM
x <- log.mo_AM$Date
y <- log.mo_AM$LogSalePrice
xlab <- "Date"
ylab <- "Log Sale Price (log $)"

# Plots Density
denP <- dens_Plot(df, x, xlab, ylab, title, subtitle)

# Attributes for box plot
title <- "Log Sale Price"
subtitle <- "Box Plot"

# Plots Bars
barP <- bar_Plot(df, x, y, xlab, ylab, title, subtitle)

# Side-by-Side Plot
require(gridExtra)
grid.arrange(denP, barP, ncol=2)
## ------------------------ ##



# summary for new log prices
summary(log.mo_AM)
### ------------------------------ ###


### Scatter plot Correlation matrix, and Corrograms for selected groups of vars ###
# employment (related predictors grouped) and home price
emp_data <- mo_AM[c(2,3:11)]
emp_title = "Employment Related Variables"
scatmatPlot(emp_data,emp_title)
corrogram(emp_data)

# wage related (related predictors grouped) metrics and home price
wage_data <- mo_AM[c(2,12:18)]
wage_title = "Wage Related Variables"
scatmatPlot(wage_data,wage_title)
corrogram(wage_data)

# corrogram for entire log.mo_AM data set
corrogram(log.mo_AM)
### --------------------------------------------------------------------------- ###


### Histogram, boxplot, and density plots for 5 variables of interest ###

# repated value treated as pseudo global
ylab <- "Log Sale Price ($)" 
by_axis <- "x" # ggmarginal plot orientation\
df <- log.mo_AM
y <- log.mo_AM$LogSalePrice

## Examing 5 variables of interest ##
# 1: Unemployment
title <- "Effect of Unemployment \non Sale Price"
xlab <- "Unemployment (%)"
by_type <- "histogram"
x <- log.mo_AM$Unemployment

d1a <- dens_Plot(df, x, xlab, ylab, title, by_type)
f1b <- feat_select(df, x, y, xlab, ylab, by_type, by_axis, title, by_type)


# 2: AE Good Producing
title <- "Effect of Good Poduction (all sectors) \non Sale Price"
xlab <- "Goods Produced (all sectors)"
x <- log.mo_AM$AEGoodProducing
by_type <- "density"

d2a <- dens_Plot(df, x, xlab, ylab, title, by_type)
f2b <- feat_select(df, x, y, xlab, ylab, by_type, by_axis, title, by_type)


# 3: Average weekly earnings
title = "Effect of Weekly Wages \non Sale Price"
xlab = "Average Weekley Earnings"
x <- log.mo_AM$Avg_Weekly_Earnings
by_type = "boxplot"

d3a <- dens_Plot(df, x, xlab, ylab, title, by_type)
f3b <- feat_select(df, x, y, xlab, ylab, by_type, by_axis, title, by_type)


# 4: Size of Labor force
title = "Effect of Labor Force (Active) \non Sale Price"
xlab = "Labor Force (count)"
by_type = "density"
x <- log.mo_AM$LaborForce

d4a <- dens_Plot(df, x, xlab, ylab, title, by_type)
f4b <- feat_select(df, x, y, xlab, ylab, by_type, by_axis, title, by_type)


# 5: Number of patents awarded in county
title = "Effect of Number of Patents Awarded \non Sale Price"
xlab = "Patents Awarded (count)"
by_type = "histogram"
x <- log.mo_AM$NPAISC

d5a <- dens_Plot(df, x, xlab, ylab, title, by_type)
f5b <-feat_select(df, x, y, xlab, ylab, by_type, by_axis, title, by_type)

# Plot side-by-side
require(gridExtra)
# Pairwise plot per predictor
grid.arrange(d1a,f1b, ncol = 2)
grid.arrange(d2a,f2b, ncol = 2)
grid.arrange(d3a,f3b, ncol = 2)
grid.arrange(d4a,f4b, ncol = 2)
grid.arrange(d5a,f5b, ncol = 2)

# Pairwise plot, all predictors of interest
grid.arrange(d1a,f1b,
             d2a,f2b,
             d3a,f3b,
             d4a,f4b,
             d5a,f5b, ncol=2)
### ----------------------------------------------------------------- ###