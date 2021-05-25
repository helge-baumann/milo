####################################
# Mindestlohnevaluation: Analyse der EVS-Daten
# Helge Emmler
# R-Version: 4.0.3
# Letztes Update: 14.05.2021
####################################

# Präambel
if (!("pacman" %in% installed.packages()[, 1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, stringr, tidyr, ggplot2, gridExtra, kableExtra, Hmisc)

# Subdateien ausführen
source("./functions/functions.R", encoding="UTF-8")

n <- 1:length(dir("./Steps"))
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

# Session Info----
dir.create("./session_Info/", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_", format(Sys.time(), "%y%m%d"), ".txt")
)



