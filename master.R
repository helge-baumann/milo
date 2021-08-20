####################################
# Mindestlohnevaluation: Analyse der EVS-Daten
# Helge Emmler
# R-Version: 4.1.0
# Letztes Update: 20.08.2021
####################################

# Präambel
if (!("pacman" %in% installed.packages()[, 1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, stringr, tidyr, ggplot2, gridExtra, kableExtra, Hmisc, purrr)

# Subdateien ausführen
# in den Summenfunktionen wurden Stundenlöhne 0 ausgeklammert
# in den Personensummen wurden HH-Gewichte als Näherungsgewichte für Personengewichte integriert
source("./functions/functions.R", encoding="UTF-8")

n <- 1:7 # wenn alles durchlaufen soll: 1: length(dir("./Steps"))
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

# Session Info----
dir.create("./session_Info/", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_", format(Sys.time(), "%y%m%d"), ".txt")
)



