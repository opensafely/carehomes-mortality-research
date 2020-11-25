# Program Information  ----------------------------------------------------

# Program:     run feasibility_checks 
# Author:      Anna Schultze 
# Description: run all r scripts in order and print console output to logs 
# Edits:

# run the feasibility checks 

logfile <- file("./analysis/logfiles/feasibility_checks.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

source("./analysis/feasibility_checks.R", echo=TRUE, max.deparse.length=10000)

sink() 
sink(type="message")
