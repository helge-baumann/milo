# Verteilung der Haushalte

# Verteilung der Haushalte insgesamt, nach Erwerbstätigen, 
  # mit abhängig Beschäftigten.

# Tabelle 4.1 (Fallzahlen)----
wb <- createWorkbook()
addWorksheet(wb, "Tab 4.1")

Tab_4_1 <- 
  EVS_defl %>%
  # Anzahl Erwerbstätige im Haushalt
  mutate(ET_hh = rowSums(!is.na(.[paste0("ET_", 1:6)])),
         AN_hh = rowSums(!is.na(.[paste0("AN_", 1:6)])),
         stundenlohn_n = rowSums(!is.na(.[paste0("stunde_", 1:6)]))) %>%
  #group_by(welle, AN_hh) %>% count()
  group_by(welle) %>%
  summarise(
    # Zeile 1
    `Alle Haushalte__N` = round(sum(!is.na(ID)), digits=0), 
    `Alle Haushalte__%` := round(100, digits=1),  
    # Zeile 2
    `HH mit Erwerbstätigen__N` = round(sum(ET_hh > 0), digits=0), 
    `HH mit Erwerbstätigen__%` = 
      round(
        sum((ET_hh > 0)*gewicht)/sum(!is.na(ET_hh)*gewicht)*100,
        digits=1), 
    # Zeile 3
    `HH mit abhängig Beschäftigten__N` = 
      round(sum(AN_hh > 0), digits=0), 
    `HH mit abhängig Beschäftigten__%` = 
      round(wtd.mean((AN_hh > 0)*gewicht)*100, digits=1), 
    # Zeile 4
    `HH mit abhängig Beschäftigten mit Milo-Berechtigung__N` = 
      round(sum(!is.na(milo_hh), na.rm=T), digits=0), 
    `HH mit abhängig Beschäftigten mit Milo-Berechtigung__%` = 
      round(wtd.mean(!is.na(milo_hh), gewicht)*100, digits=1)
  ) %>% 
  pivot_longer(
    -welle,
    names_to = c('.value', 'Wert'), 
    names_sep="__") 

writeData(wb, "Tab 4.1", names(Tab_4_1)[3:ncol(Tab_4_1)], startCol=1, startRow=3)
writeData(wb, "Tab 4.1", t(Tab_4_1[,1:2]), startCol=2, startRow=1, colNames=F)
writeData(wb, "Tab 4.1", t(Tab_4_1[,3:ncol(Tab_4_1)]), startCol=2, startRow=3, 
          colNames=F)
writeData(wb, "Tab 4.1", 
paste0("Achtung: Anteil der Haushalte mit Mindestlohnberechtigung: ",
       "Haushalte mit gültiger Stundenlohnberechnung / ",
       "alle Haushalte in EVS * 100"), 
startCol=2, startRow=ncol(Tab_4_1)+2, colNames=F)
addStyle(wb, "Tab 4.1", style_anmerkung, ncol(Tab_4_1)+2, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 4.1", 
         style = createStyle(numFmt = "0.0"), 
         rows = 3:(nrow(Tab_4_1)+2), 
         cols = which(Tab_4_1$Wert == "%")+1, 
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_4-1_Fallzahlen.xlsx"), 
             overwrite=T)
  

# Tabelle 4.2: Einkommen und Löhne, nur HH mit abh. Besch. Mit Milo-Berechtigung

Tab_4_2 <- EVS_defl %>%
  filter(!is.na(milo_hh))%>%
  mutate(
    # Mittleres Bruttomonatseinkommen 
    monat_mean = rowMeans(.[paste0("monat_", 1:6, "_winsor")], na.rm=T),
    # Anzahl Personen mit Bruttomonatslöhnen
    monat_n = rowSums(!is.na(.[paste0("monat_", 1:6, "_winsor")])),
    # summe der vereinbarten Wochenarbeitszeit im Haushalt
    az_mean = rowMeans(.[paste0("az_", 1:6, "_winsor")], na.rm=T),
    # Anzahl Personen mit AZ-Angabe pro Haushalt
    az_n = rowSums(!is.na(.[paste0("az_", 1:6, "_winsor")])),
    # Mittlerer Stundenlohn
    stunde_mean = rowMeans(.[paste0("stunde_", 1:6, "_winsor")], na.rm=T),
    # Anzahl Personen mit Stundenlöhnen
    stunde_n = rowSums(!is.na(.[paste0("stunde_", 1:6, "_winsor")])),
    # Anzahl Personen mit Stundenlohn unter Milo + 1 Euro
    milo = if_else(welle == "2013", 8.5*0.985+1, 8.84+1),
    milo_n = rowSums(.[paste0("stunde_", 1:6, "_winsor")] < milo, na.rm=T),
    deflation = if_else(welle == "2013", 1/defl$d2013[13], 1)
    ) %>%
  group_by(welle) %>%
  summarise(
    # Haushaltsebene
    Haushalt_Bruttohaushaltseinkommen__Mittelwert = 
      round(wtd.mean(brutto_winsor/3, gewicht), digits=2), 
    Haushalt_Bruttohaushaltseinkommen__N = 
      round(sum(!is.na(brutto_winsor)), digits=0), 
    Haushalt_Arbeitseinkommen__Mittelwert = 
      round(wtd.mean(lohn_summe_winsor/3, gewicht), digits=2), 
    Haushalt_Arbeitseinkommen__N = 
      round(sum(!is.na(lohn_summe_winsor)), digits=0), 
    Haushalt_Haushaltsnettoeinkommen__Mittelwert = 
      round(wtd.mean(netto_oecd_winsor*oecd_weight/3, gewicht), digits=2), 
    Haushalt_Haushaltsnettoeinkommen__N = 
      round(sum(!is.na(netto_oecd_winsor*oecd_weight)), digits=0), 
    Haushalt_Haushaltsnettoäquivalenzeinkommen__Mittelwert = 
      round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=2), 
    Haushalt_Haushaltsnettoäquivalenzeinkommen__N = 
      round(sum(!is.na(netto_oecd_winsor)), digits=0), 
    
    # Personenebene
    Personen_Bruttomonatslohn__Mittelwert = 
      round(wtd.mean(monat_mean*deflation, monat_n*gewicht), digits=2), 
    Personen_Bruttomonatslohn__N = 
      round(sum(monat_n, na.rm=T), digits=0), 
    Personen_Vereinbarte_Wochenarbeitszeit__Mittelwert = 
      round(wtd.mean(az_mean, az_n*gewicht), digits=2), 
    Personen_Vereinbarte_Wochenarbeitszeit__N = 
      round(sum(az_n, na.rm=T), digits=0), 
    Personen_Stundenlohn__Mittelwert = 
      round(wtd.mean(stunde_mean*deflation, stunde_n*gewicht), digits=2), 
    Personen_Stundenlohn__N = 
      round(sum(stunde_n, na.rm=T), digits=0), 
    Personen_Anteil_Mindestlohn__Mittelwert = 
      round(sum(milo_n*gewicht)/sum(stunde_n*gewicht)*100, digits=2), 
    Personen_Anteil_Mindestlohn__N = 
      round(sum(stunde_n, na.rm=T), digits=0), 
    # Ebene des Haushaltsvorstands
    Haushaltsvorstand_Bruttomonatslohn__Mittelwert =
      round(wtd.mean(monat_1_winsor*deflation, gewicht), digits=2), 
    Haushaltsvorstand_Bruttomonatslohn__N =
      round(sum(!is.na(monat_1_winsor)), digits=0), 
    Haushaltsvorstand_Vereinbarte_Wochenarbeitszeit__Mittelwert =
      round(wtd.mean(az_1_winsor, gewicht), digits=2), 
    Haushaltsvorstand_Vereinbarte_Wochenarbeitszeit__N =
      round(sum(!is.na(az_1_winsor)), digits=0), 
    Haushaltsvorstand_Stundenlohn__Mittelwert =
      round(wtd.mean(stunde_1_winsor*deflation, gewicht), digits=2), 
    Haushaltsvorstand_Stundenlohn__N =
      round(sum(!is.na(stunde_1_winsor)), digits=0), 
    Haushaltsvorstand_Anteil_Mindestlohn__Mittelwert =
      round(sum((stunde_1_winsor < milo)*gewicht, na.rm=T)/
      sum((!is.na(stunde_1_winsor))*gewicht, na.rm=T)*100, digits=2), 
    Haushaltsvorstand_Anteil_Mindestlohn__N =
      round(sum(!is.na(stunde_1_winsor)), digits=0), 
  ) %>% 
  pivot_longer(
    -welle,
    names_to = c('.value', 'Wert'), 
    names_sep="__") 

wb <- createWorkbook()
addWorksheet(wb, "Tab 4.2")

writeData(wb, "Tab 4.2", 
          sapply(str_split(names(Tab_4_2), fixed("_")), 
                 function(x) x[1])[3:ncol(Tab_4_2)], 
          startCol=1, startRow=3)
writeData(wb, "Tab 4.2", 
          unlist(sapply(str_split(names(Tab_4_2), fixed("_")), 
                        function(x) paste(x[-1], collapse=" "))[3:ncol(Tab_4_2)]),
          startCol=2, startRow=3)
writeData(wb, "Tab 4.2", t(Tab_4_2[,1:2]), startCol=3, startRow=1, colNames=F)
writeData(wb, "Tab 4.2", t(Tab_4_2[,3:ncol(Tab_4_2)]), startCol=3, startRow=3, 
          colNames=F)
writeData(wb, "Tab 4.2", 
          paste0("Achtung: Nur Haushalte mit gültiger Stundenlohnberechnung"), 
          startCol=3, startRow=ncol(Tab_4_2)+2, colNames=F)
writeData(wb, "Tab 4.2", 
          paste0("Achtung: ",
                 "Werte der Haushaltsebene monatsweise! "), 
          startCol=3, startRow=ncol(Tab_4_2)+3, colNames=F)
writeData(wb, "Tab 4.2", 
          paste0("Achtung: ",
                 "Personenwerte als Anteile über alle Personen hinweg! "), 
          startCol=3, startRow=ncol(Tab_4_2)+4, colNames=F)
writeData(wb, "Tab 4.2", 
          paste0("Achtung: ",
                 "N bei den Anteilen entspricht dem Nenner! "), 
          startCol=3, startRow=ncol(Tab_4_2)+5, colNames=F)


addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+2, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+3, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+4, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+5, 3)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 4.2", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(!str_detect(names(Tab_4_2), "Anteil")), 
         cols = which(Tab_4_2$Wert == "Mittelwert")+2, 
         gridExpand = T)
addStyle(wb, sheet = "Tab 4.2", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(str_detect(names(Tab_4_2), "Anteil")), 
         cols = which(Tab_4_2$Wert == "Mittelwert")+2, 
         gridExpand = T)

saveWorkbook(wb, paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                        "Tab_4-2_Einkommen und Löhne ",
                        "(nur Haushalte mit Milo-Berechtigung).xlsx"), 
             overwrite=T)

