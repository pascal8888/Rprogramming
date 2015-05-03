rm(list = setdiff(ls(), lsf.str())) #Removes all workspace objects except for functions.
rm(list=ls()) #Removes ALL workspace objects
options(error=recover) #Run in workspace to have R break in call stack on error - resets when workspace closed/opened.
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252") #in case sort returns funky results make sure my locale is correct
