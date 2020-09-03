import delimited `c(pwd)'/output/input.csv, clear
cd  `c(pwd)'/analysis
set more off 


/* Feasibility Tables=========================================================*/

capture mkdir outfiles 
capture mkdir logfiles 

global outdir "outfiles" 
global logdir "logfiles"

do "00_Feasibility.do"

/* Analyses===================================================================*/

