####################################
# Mindestlohnevaluation: Deskriptive Analyse der EVS-Daten
# Helge Emmler, Toralf Pusch
# R-Version: 4.1.1
# Letztes Update: 22.10.2021
####################################

# Präambel----
if (!("pacman" %in% installed.packages()[, 1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, stringr, tidyr, Hmisc, purrr, openxlsx)

# Outputordner einrichten (Tabellen- und Datenexporte)
dir.create(paste0("Output/Tabellen für Berichte/", Sys.Date()), showWarnings=F)
dir.create(paste0("./Output/Daten/", Sys.Date()), showWarnings=F)

# Subdateien ausführen----

# Selbst geschriebene Funktionen laden
source("./functions/functions.R", encoding="UTF-8")

# alle R-Skripte der Reihe nach ausführen (Unterordner "Steps")
n <- 1:length(dir("./Steps")[str_detect(dir("./Steps"), "[:punct:]R$")])
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

# Session Info----
dir.create("./session_Info/", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_", format(Sys.time(), "%y%m%d"), ".txt")
)



