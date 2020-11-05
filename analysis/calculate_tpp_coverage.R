# Program Information  ----------------------------------------------------

# Program:     Calculate TPP coverage
# Author:      Emily Nightingale 
# Description: Script to calculate TPP coverage per MSOA according to household
#              size of TPP-registered patients and ONS population estimates per MSOA

# Edits:      [AS: Adding comments for local run] 

# Housekeeping  -----------------------------------------------------------

##-- Load packages 
pacman::p_load("tidyverse", "data.table", "dtplyr", "zoo")

##-- Redirect output to a log file 
sink("./analysis/logfiles/calculate_tpp_coverage.txt, type = output")

# Load Data ---------------------------------------------------------------

# * input_coverage.csv 
#   - household ID, size and MSOA for all TPP-registered patients
# * msoa_pop.csv 
#   - total population estimates per MSOA
#   - population estimates by single year age

##-- Create a vector with the different datasets to be read in (local only)
# args <- c("./output/input_coverage.csv","./data/msoa_pop.csv") 
##-- Allow arguments entered from the command line (leave for server run)
args = commandArgs(trailingOnly=TRUE)

##-- TPP  population
input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  mutate(msoa = as.factor(msoa))

##-- MSOA reference population as denominator 
msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") %>%
  rename(msoa = `Area Codes`,
         msoa_pop = `All Ages`) %>%
  rowwise() %>%
  mutate(`70+` = sum(`70`:`90+`)) %>%
  select(msoa, msoa_pop, `70+`) 

##-- Count TPP patients per MSOA and merge in denominator 
#    merge generates error messages due to variable types, this is ok 
input %>%
  group_by(msoa) %>%
  count(name = "tpp_pop") %>%
  full_join(msoa_pop) %>%
  mutate(tpp_cov = tpp_pop*100/msoa_pop) -> tpp_cov

##-- Output datasets 

saveRDS(tpp_cov, file = "./data/tpp_msoa_coverage.rds")
write.csv(tpp_cov, "./data/tpp_msoa_coverage.csv", row.names = FALSE)

sink() 
