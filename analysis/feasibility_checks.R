# Program Information  ----------------------------------------------------

# Program:     Feasibility checks 
# Author:      Anna Schultze (parts of code from E Nightingale)
# Description: Script for basic care home sense checks 
# Edits:      

# Housekeeping  -----------------------------------------------------------

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(knitr)
library(lubridate)

# Read in Data  -----------------------------------------------------------
## (local, comment out server)
## running through project.yaml will make the script 'see' a different wd
# args <- c("../output/input.csv","../data/tpp_msoa_coverage.csv") 
## (server, comment out locally)
args = commandArgs(trailingOnly=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "")
msoa_coverage <- fread(args[2], data.table = FALSE, na.strings = "")

# Basic Descriptives ------------------------------------------------------

# number of individuals within care homes of different types 
table1a <- input  %>% 
  select(care_home_type, household_id)  %>% 
  mutate(care_home_type = as.factor(care_home_type))  %>% 
  mutate(care_home_type = fct_recode(care_home_type, "Care Home" = "PC", "Nursing Home" = "PN", "Care or Nursing Home" = "PS")) %>% 
  tabyl(care_home_type)  %>% 
  mutate(percent = round(percent,4)*100)  %>% 
  adorn_totals()
  
knitr::kable(table1a, col.names = c("Type of Care Home", "N", "Percent"))
write.table(table1a, file = "./outfiles/table1a.txt", sep = "\t")

# number of care homes of different types 
table1b <- input  %>% 
  select(care_home_type, household_id)  %>% 
  mutate(care_home_type = as.factor(care_home_type))  %>% 
  mutate(care_home_type = fct_recode(care_home_type, "Care Home" = "PC", "Nursing Home" = "PN", "Care or Nursing Home" = "PS")) %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  tabyl(care_home_type)  %>% 
  mutate(percent = round(percent,4)*100)  %>% 
  adorn_totals() 

knitr::kable(table1b, col.names = c("Type of Care Home", "N", "Percent"))
write.table(table1b, file = "./outfiles/table1b.txt", sep = "\t")

# Data Quality Checks  ----------------------------------------------------

# reformat and rename msoa level coverage 
msoa_coverage_reduced <- msoa_coverage  %>% 
  select(msoa, tpp_cov)  %>% 
  mutate(tpp_cov = as.numeric(tpp_cov))  %>% 
  rename(msoa_coverage = tpp_cov)

# merge with the input data 
ch_coverage_all <- input  %>% 
  left_join(msoa_coverage_reduced) 

# check the distribution of the coverage variables to make sure this is reasonable 
print("Distribtuion of TPP and MSOA coverage")
summary(ch_coverage_all$tpp_coverage)
summary(ch_coverage_all$msoa_coverage)

# number of care homes with missing MSOA 
missing_check <- input  %>% 
  select(household_id, msoa)  %>% 
  dplyr::filter(is.na(msoa))  %>% 
  distinct(household_id)  %>% 
  nrow()

print("number of care homes with missing MSOA")
missing_check

# number of care homes with missing TPP coverage (by care home) 
  missing_check2 <- input  %>% 
    dplyr::select(household_id, tpp_coverage) %>% 
    dplyr::filter(is.na(tpp_coverage)) %>% 
    distinct(household_id) %>% 
    nrow() 

print("number of care homes with missing TPP coverage estimate")
missing_check2

# histogram - number of care homes with different levels of TPP coverage 
figure1a <- ch_coverage_all  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  ggplot(aes(x = tpp_coverage)) + 
  geom_histogram(fill = "lightsteelblue2", col = ("lightsteelblue4")) + 
  labs(x = "TPP coverage", 
       y = "Number of care homes", 
       title = "Number of care homes according to TPP coverage") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")
  )

png(filename = "./outfiles/figure1a.png")
figure1a
dev.off()

# histogram - number of MSOAs with different levels of MSOA coverage 
figure1b <- ch_coverage_all  %>% 
  distinct(msoa, .keep_all = TRUE)  %>% 
  ggplot(aes(x = msoa_coverage)) + 
  geom_histogram(fill = "lightsteelblue2", col = ("lightsteelblue4")) + 
  labs(x = "MSOA coverage", 
       y = "Number of MSOAs", 
       title = "Number of MSOAs according to MSOA coverage") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")
  )

png(filename = "./outfiles/figure1b.png")
figure1b
dev.off()

# histogram - number of care homes with different levels of MSOA coverage 
figure1c <- ch_coverage_all  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  ggplot(aes(x = msoa_coverage)) + 
  geom_histogram(fill = "lightsteelblue2", col = ("lightsteelblue4")) + 
  labs(x = "MSOA coverage", 
       y = "Number of care homes", 
       title = "Number of care homes according to MSOA coverage") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")
  )

png(filename = "./outfiles/figure1c.png")
figure1c
dev.off()

# Correlation of Coverage Estimates ---------------------------------------
# data management, create categorical tpp coverage estimate  
ch_coverage <- input  %>% 
  select(household_id, household_size, care_home_type, tpp_coverage)  %>% 
  mutate(tpp_coverage_cat = case_when(
    tpp_coverage >= 95 ~ "95 - 100", 
    tpp_coverage <  95 & tpp_coverage >= 90 ~ "90 - 94", 
    tpp_coverage <  90 & tpp_coverage >= 80 ~ "80 - 89",
    tpp_coverage <  80 & tpp_coverage >= 70 ~ "70 - 79", 
    tpp_coverage <  70 & tpp_coverage >= 60 ~ "60 - 69", 
    tpp_coverage <  60 & tpp_coverage >= 50 ~ "50 - 59",
    tpp_coverage <  50 ~ "0 - 49")) %>% 
  mutate(nontpp_coverage_cat = case_when(
    tpp_coverage <  50 ~ "50 - 100 ", 
    tpp_coverage <  60 & tpp_coverage >= 50 ~ "40 - 49", 
    tpp_coverage <  70 & tpp_coverage >= 60 ~ "30 - 39",
    tpp_coverage <  80 & tpp_coverage >= 70 ~ "20 - 29", 
    tpp_coverage <  90 & tpp_coverage >= 80 ~ "10 - 19", 
    tpp_coverage <  95 & tpp_coverage >= 90 ~ "5 - 9",
    tpp_coverage >= 95 ~ "0 - 4")) 

# number of care homes with different % of residents covered by TPP software
# care homes by coverage
table2a <- ch_coverage  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  tabyl(tpp_coverage_cat)  %>% 
  adorn_pct_formatting() %>% 
  adorn_totals()

# individuals by coverage 
table2b <- ch_coverage  %>% 
  tabyl(tpp_coverage_cat)  %>% 
  adorn_pct_formatting() %>% 
  adorn_totals() 

table2 <- table2a %>%  
  left_join(table2b, by  = "tpp_coverage_cat")

knitr::kable(table2, col.names = c("TPP coverage", "Number of Care Homes", "Percentage of Care Homes", "Number of Residents", "Percentage of Residents"))
write.table(table2, file = "./outfiles/table2.txt", sep = "\t")

# number of care homes with different % of residents covered by non TPP software
## d leon requested inverse of above for clarity 

# care homes by coverage
table3a <- ch_coverage  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  tabyl(nontpp_coverage_cat)  %>% 
  adorn_pct_formatting() %>% 
  adorn_totals()

# individuals by coverage 
table3b <- ch_coverage  %>% 
  tabyl(nontpp_coverage_cat)  %>% 
  adorn_pct_formatting() %>% 
  adorn_totals() 

table3 <- table3a %>%  
  left_join(table3b, by  = "nontpp_coverage_cat")

knitr::kable(table3, col.names = c("Non TPP coverage", "Number of Care Homes", "Percentage of Care Homes", "Number of Residents", "Percentage of Residents"))
write.table(table3, file = "./outfiles/table3.txt", sep = "\t")

# plot tpp vs msoa coverage 
# for some reason right and bottom border line not removed?? troubleshoot with team
figure2 <- ch_coverage_all  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  ggplot(aes(x = msoa_coverage, y = tpp_coverage)) +
  geom_point(shape = 21, color = "lightsteelblue2", fill = "lightsteelblue2") + 
  scale_x_continuous(limits=c(0,100)) + 
  scale_y_continuous(limits=c(0,100)) + 
  labs(x = "% MSOA coverage", 
       y = "% care home coverage", 
       title = "% TPP coverage in care homes vs. MSOA coverage") +  
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./outfiles/figure2.png")
figure2
dev.off()


# Date of death in TPP vs. ONS --------------------------------------------
## care home analyses are planning on calculating excess deaths; 
## however, date of death is only available from ONS from 2019. 
## in order to establish whether date of death in TPP can be used instead, 
## we're undertaking a quick check to see the discrepancies between these variables

date_check <- input  %>%  
  mutate(tpp_death_date = as.numeric(ymd(tpp_death_date)))  %>% 
  mutate(ons_covid_death_date = as.numeric(ymd(ons_covid_death_date)))  %>% 
  mutate(date_comparison = case_when(
    is.na(ons_covid_death_date) & !is.na(tpp_death_date) ~ "Death in ONS but not in TPP", 
    is.na(tpp_death_date) & !is.na(ons_covid_death_date) ~ "Death in TPP but not in ONS", 
    !is.na(tpp_death_date) & !is.na(ons_covid_death_date) ~ "Death in both TPP and ONS"))  %>% 
  mutate(date_difference = tpp_death_date - ons_covid_death_date, na.rm = TRUE) 

table4 <- date_check  %>% 
  tabyl(date_comparison)  %>% 
  adorn_totals()  %>% 
  adorn_pct_formatting() 

knitr::kable(table4, col.names = c("Death Date Agreement", "N", "%", "Non missing %"))
write.table(table4, file = "./outfiles/table4.txt", sep = "\t")

print("Difference between death date in TPP and ONS, where this exists")
summary(date_check$date_difference)

