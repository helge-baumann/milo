# Manipulierte Daten abspeichern

dir.create(paste0("./Output/Daten/", Sys.Date()), showWarnings=F)
write_dta(evs13, 
          paste0("./Output/Daten/", Sys.Date(), "/", Sys.Date(), 
                 "_EVS2013.dta"))
write_dta(evs18, 
          paste0("./Output/Daten/", Sys.Date(), "/", Sys.Date(), 
                 "_EVS2018.dta"))
write_dta(EVS, 
          paste0("./Output/Daten/", Sys.Date(), "/", Sys.Date(), 
                 "_EVS2013_2018.dta"))
write_dta(EVS_defl, 
          paste0("./Output/Daten/", Sys.Date(), "/", Sys.Date(), 
                 "_EVS2013_2018_deflationiert.dta"))
