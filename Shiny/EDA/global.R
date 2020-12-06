library(shiny)
library(DT)
library(ggplot2)
library(lubridate)
library(vcd)
library(GGally)
library(shinyjs)
library(corrgram)
library(visdat)
library(scales)
library(dplyr)
library("RColorBrewer") 
library(reshape2)

library(shinyWidgets)
library(shinyjs)
library(lubridate)

# Loading Data
dat <- read.csv("Ass1Data.csv")
# Factor -> Date datatype
dat$Date <- ymd(dat$Date)

# Setting levels for Ordinal Columns
dat$Price <- ordered(dat$Price, levels = c("Cheap","Costly","Extravagant"))
dat$Priority <- ordered(dat$Priority, levels = c("Low","Medium","High"))
dat$Speed <- ordered(dat$Speed, levels = c("Slow","Medium","Fast"))
dat$Duration <- ordered(dat$Duration, levels = c("Short","Long","Very Long"))
dat$Temp <- ordered(dat$Temp, levels = c("Cold","Warm","Hot"))

# Retrieving colnames
choices_all <- colnames(dat)
# Colnames excluding 'Y'
choices <- choices_all[-1]
# Choosing Categorical Columns
choices_cat <- choices[2:13]
choices_cat <- choices_cat[-2]
# Choosing Numerical Columns
choices_num <- colnames(dat[15:44])