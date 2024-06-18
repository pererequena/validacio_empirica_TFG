################################################################################

# This is the script to load the three datasets for the numeric exchange

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc();

# Loading dependencies
library(readxl,warn.conflicts = F, quietly = T)
library(httr,warn.conflicts = F, quietly = T)
library(tidyverse)
source("dependencies.R")



################
# Loading data #
################

# Gurcay et al. 
d1 = read.csv("gurcay_data.csv", stringsAsFactors=F) %>% 
  mutate(
    pre_influence = est1
    , post_influence = est2
    , question = question.no
    , trial=paste0("gurcay",group, "-",question)
    , truth=true.values
    , dataset="gurcay"
  ) %>% 
  subset(condition!="C" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )

# Becker et al.
d2 = read.csv("becker_data.csv", stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
    , post_influence = response_3
    , question = task
    , trial=paste0("becker", group_number, "-",question)
    , dataset="becker"
  ) %>% 
  subset(network=="Decentralized" & #Només s'escullen els casos en què el tipus de connexió és "descentralitzada"??
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )

# Lorenz et al. 
d3 <- read_excel("lorenz_data.xlsx") %>%
  mutate(
    pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date,Question)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Delphi"
  ) %>% #Només s'escullen els casos en què el tipus de connexió és "descentralitzada"
  subset(network=="Decentralized") %>%
  group_by(trial, task) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )

## Binding the datasets together
d = rbind( d1
           ,d2
           ,d3
) %>%
  group_by(trial) %>%
  mutate(
    mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    , improve = abs(mu2-truth) < abs(mu1-truth)
    , worse = abs(mu2-truth) > abs(mu1-truth)
  )

