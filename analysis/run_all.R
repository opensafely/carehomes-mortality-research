# Program Information  ----------------------------------------------------

# Program:     run all 
# Author:      Anna Schultze 
# Description: run all r scripts in order and print console output to logs 
# Edits:      

# generate the coverage 

logfile <- file("./logfiles/calculate_tpp_coverage.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

source("calculate_tpp_coverage.R", echo=TRUE, max.deparse.length=10000)

sink() 
sink(type="message")


# run the feasibility checks 

logfile <- file("./logfiles/feasibility_checks.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

source("feasibility_checks.R", echo=TRUE, max.deparse.length=10000)

sink() 
sink(type="message")
