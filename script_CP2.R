## Reproducible Research - course project 2
## by: github.com/wim-
## This script:
##    1) loads the required packages
##    2) reads the data, creates a subset containing the variables ofinterest to the analysis
##    3) cleans the data for the purpose of the analysis

## set project specific variables
setwd("./DataScience/05 Reproducible Research/course project 2")
Sys.setlocale(category = "LC_TIME", locale = "C")

#####################################
## step 1: load packages           ##
#####################################
if (!require("dplyr")) {
      install.packages("dplyr")
      library(dplyr)
}
if (!require("lubridate")) {
      install.packages("lubridate")
      library(lubridate)
}
if (!require("tidyr")) {
      install.packages("tidyr")
      library(tidyr)
}
if (!require("lattice")) {
      install.packages("lattice")
      library(lattice)
}
if (!require("ggplot2")) {
      install.packages("ggplot2")
      library(ggplot2)
}

#####################################
## step 2: read data               ##
#####################################
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
sourcefile <- "./data/repdata-data-StormData.csv.bz2"

if(!file.exists(sourcefile)){
      if(!file.exists("./data")){
            dir.create("./data")
      }
      download.file(fileURL, sourcefile)
      datedownloaded <- now ()
      write.table(datedownloaded,
                file="./data/sourcefile_datedownloaded.txt",
                row.names = FALSE,
                col.names = FALSE
                )
}


if(!file.exists("./data/data_subset.csv")){
      my_data <-
            read.csv(bzfile(sourcefile)) %>%
            tbl_df() %>%
            select(REFNUM,
                   EVTYPE,
                   FATALITIES, INJURIES,
                   PROPDMG, PROPDMGEXP,
                   CROPDMG, CROPDMGEXP
                   )
      write.table(my_data,
                  file = "./data/data_subset.csv",
                  sep = ",",
                  row.names = FALSE
                )
}

if(!"my_data" %in% ls()){
      my_data <-
            read.table("./data/data_subset.csv",
                       header = TRUE,
                       sep = ",") %>%
            tbl_df()
      datedownloaded <-
            read.table("./data/sourcefile_datedownloaded.txt", sep=",")[1,1] %>%
            ymd_hms()
}


rm(fileURL,sourcefile)

#####################################
## step 3: clean data              ##
#####################################
# group the different items per event type
# count both the fatalities and injuries
# take top x procent
# tidy up dat set to put on graph

health <- select(my_data,
                 EVTYPE,
                 FATALITIES,
                 INJURIES) %>%
      group_by(EVTYPE) %>%
      summarise(fatalities = sum(FATALITIES),
                injuries = sum(INJURIES),
                total = fatalities + injuries) %>%
      filter(total > quantile(.$total, probs = 0.99)) %>%
      arrange(desc(total)) %>%
      select(-total) %>%
      gather(impact, number, -EVTYPE)
      

              
# the *EXP variable contains a lot of different possible indicators
# these need to be transformed in order to make sense of the total damages

exp <- function(n){
      options(warn=-1) # suppres warnings while calling the function
      exp <- 1
      if(n %in% c('1')){exp <- 10^1}
      if(n %in% c('2','h','H')){exp <- 10^2}
      if(n %in% c('3','k','K')){exp <- 10^3}
      if(n %in% c('4')){exp <- 10^4}
      if(n %in% c('5')){exp <- 10^5}
      if(n %in% c('6','m','M')){exp <- 10^6}
      if(n %in% c('7')){exp <- 10^7}
      if(n %in% c('8')){exp <- 10^8}
      if(n %in% c('9','b','B')){exp <- 10^9}
      return(exp)
}

# i <- 1
# while(i <= length(my_data$EVTYPE)){
#       my_data$PROPDMGEXP2[i] <- exp(my_data$PROPDMGEXP[i])
#       my_data$CROPDMGEXP2[i] <- exp(my_data$CROPDMGEXP[i])
#       i <- i + 1
# }
      

# add collumns with proper damages amount
# group the different items per event type
# total the damages for both properties and damages
# take top x procent
# tidy up dat set to put on graph

economic <- select(my_data,
                   EVTYPE,  
                   PROPDMG,
                   PROPDMGEXP,
                   CROPDMG,
                   CROPDMGEXP) %>%
      mutate(TotPROPDMG = PROPDMG * exp(PROPDMGEXP),
             TotCROPDMG = CROPDMG * exp(CROPDMGEXP)) %>%
      group_by(EVTYPE) %>%
      summarise(PropertyDamage = sum(TotPROPDMG),
                CropDamage = sum(TotCROPDMG),
                TotDMG = PropertyDamage + CropDamage) %>%
      filter(TotDMG > quantile(.$TotDMG, probs = 0.99)) %>%
      arrange(desc(TotDMG)) %>%
      select(-TotDMG) %>%
      gather(type, USD, -EVTYPE)
      

      

#####################################
## step 4: data analysis           ##
#####################################
ggplot(health, aes(x = reorder(EVTYPE,number), y = number, fill = impact)) +
      geom_bar(stat = "identity") +
      labs(title = "Top percentile most harmful events",
           x = "Type of events", 
           y = "Number of people impacted") +
      coord_flip()

ggplot(economic, aes(x = reorder(EVTYPE,USD), y = USD, fill = type)) +
      geom_bar(stat = "identity") +
      labs(title = "Top percentile largest economic consequences",
           x = "Type of events", 
           y = "Total (in USD)") +
      coord_flip()

#####################################
## step x:                         ##
#####################################

my_data2 <- filter(my_data, REFNUM<=100)

levels(my_data$PROPDMGEXP)
levels(my_data$CROPDMGEXP)

filter(my_data, PROPDMG == 0) %>%
      group_by(PROPDMGEXP) %>%
      summarise(total = n())

a <- filter(economic, CROPDMG != 0) %>%
      group_by(TotCROPDMG) %>%
      summarise(total = n())

filter(my_data, CROPDMG == 0) %>%
      group_by(CROPDMGEXP) %>%
      summarise(total = n())

filter(my_data, CROPDMG != 0) %>%
      group_by(CROPDMGEXP) %>%
      summarise(total = n())

quantile(pack_sum$count, probs = 0.99)