# Manipulierte Daten abspeichern

dir.create("./Output/Daten", showWarnings=F)
write_dta(evs13, paste0("./Output/Daten/", Sys.Date(), "_EVS2013.dta"))
write_dta(evs18, paste0("./Output/Daten/", Sys.Date(), "_EVS2018.dta"))

# save Workspace
#save.image(file=paste0("./Output/WS_", Sys.Date(), ".RData"))
          