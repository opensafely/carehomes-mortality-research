# Program Information  ----------------------------------------------------

# Program:     run calculate_tpp_coverage.txt 
# Author:      Anna Schultze 
# Description: run calculate_tpp_coverage.txt
#              slightly awkward set-up, as I want to print both input code and all output (incl. messages)
#              this will be archived with next version of project.yaml, which will automatically create logs 
# Edits:      

# create directories 
# (server only, comment out locally due to wd discrepancy)

# run the calculate_tpp_coverage

logfile <- file("./analysis/logfiles/calculate_tpp_coverage.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

source("./analysis/calculate_tpp_coverage.R", echo=TRUE, max.deparse.length=10000)

sink() 
sink(type="message")
