# Verteilung der Haushalte

# Verteilung der Haushalte insgesamt, nach Erwerbstätigen, 
  # mit abhängig Beschäftigten.

# Tabelle 4.1 (Fallzahlen)----
wb <- createWorkbook()
addWorksheet(wb, "Tab 4.1")

Tab_4_1 <- EVS %>%
  # Anzahl Erwerbstätige im Haushalt
  mutate(ET_hh = rowSums(!is.na(.[paste0("ET_", 1:6)])),
         AN_hh = rowSums(!is.na(.[paste0("ET_", 1:6)]))) %>%
  group_by(welle) %>%
  summarise(
    # Zeile 1
    `Alle Haushalte__N` = sum(!is.na(ID)),  `Alle Haushalte__%` := 100, 
    # Zeile 2
    `HH mit Erwerbstätigen__N` = sum(ET_hh > 0), `HH mit Erwerbstätigen__%` = 
      sum((ET_hh > 0)*gewicht)/sum(!is.na(ET_hh)*gewicht)*100,
    # Zeile 3
    `HH mit abhängig Beschäftigten__N` = sum(ET_hh > 0), 
    `HH mit abhängig Beschäftigten__%` = 
      sum((ET_hh > 0)*gewicht)/sum(!is.na(ET_hh)*gewicht)*100,
    # Zeile 4
    `HH mit abhängig Beschäftigten mit Milo-Berechtigung__N` = 
      sum(!is.na(milo_hh), na.rm=T), 
    `HH mit abhängig Beschäftigten mit Milo-Berechtigung__%` = 
      sum(!is.na(milo_hh)*gewicht, na.rm=T)/
      sum(!is.na(AN_hh)*gewicht)*100
  ) %>% 
  pivot_longer(
    -welle,
    names_to = c('.value', 'Wert'), 
    names_sep="__") 

writeData(wb, "Tab 4.1", names(Tab_4_1)[3:ncol(Tab_4_1)], startCol=1, startRow=3)
writeData(wb, "Tab 4.1", t(Tab_4_1[,1:2]), startCol=2, startRow=1, colNames=F)
writeData(wb, "Tab 4.1", t(Tab_4_1[,3:ncol(Tab_4_1)]), startCol=2, startRow=3, 
          colNames=F, borders="surrounding")
writeData(wb, "Tab 4.1", 
paste0("Achtung: Anteil der Haushalte mit Mindestlohnberechtigung: ",
       "Haushalte mit gültiger Stundenlohnberechnung / ",
       "alle Haushalte in EVS * 100"), 
startCol=2, startRow=ncol(Tab_4_1)+2, colNames=F)
addStyle(wb, "Tab 4.1", style_anmerkung, ncol(Tab_4_1)+2, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_4-1_Fallzahlen.xlsx"), 
             overwrite=T)
  

# Tabelle 4.2: Einkommen und Löhne, nur HH mit abhängig Besch. Mit Milo-Berechtigung

# Rowsums vorher bilden!!
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
    milo_n = rowSums(.[paste0("stunde_", 1:6, "_winsor")] < milo, na.rm=T)
    ) %>%
  group_by(welle) %>%
  summarise(
    # Haushaltsebene
    Haushalt_Bruttohaushaltseinkommen__Mittelwert = 
      wtd.mean(brutto_winsor/3, gewicht),
    Haushalt_Bruttohaushaltseinkommen__N = 
      sum(!is.na(brutto_winsor)),
    Haushalt_Haushaltsnettäquivalenzeinkommen__Mittelwert = 
      wtd.mean(netto_oecd_winsor/3, gewicht),
    Haushalt_Haushaltsnettäquivalenzeinkommen__N = 
      sum(!is.na(netto_oecd_winsor)),
    Haushalt_Arbeitseinkommen__Mittelwert = 
      wtd.mean(lohn_summe_winsor/3, gewicht),
    Haushalt_Arbeitseinkommen__N = 
      sum(!is.na(lohn_summe_winsor)),
    # Personenebene
    Personen_Bruttomonatslohn__Mittelwert = 
      wtd.mean(monat_mean, monat_n*gewicht),
    Personen_Bruttomonatslohn__N = 
      sum(monat_n, na.rm=T),
    Personen_Vereinbarte_Wochenarbeitszeit__Mittelwert = 
      wtd.mean(az_mean, az_n*gewicht),
    Personen_Vereinbarte_Wochenarbeitszeit__N = 
      sum(az_n, na.rm=T),
    Personen_Stundenlohn__Mittelwert = 
      wtd.mean(stunde_mean, stunde_n*gewicht),
    Personen_Stundenlohn__N = 
      sum(stunde_n, na.rm=T),
    Personen_Anteil_Mindestlohn__Mittelwert = 
      sum(milo_n*gewicht)/sum(stunde_n*gewicht),
    Personen_Anteil_Mindestlohn__N = 
      sum(stunde_n, na.rm=T),
    # Ebene des Haushaltsvorstands
    Haushaltsvorstand_Bruttomonatslohn__Mittelwert =
      wtd.mean(monat_1_winsor, gewicht), 
    Haushaltsvorstand_Bruttomonatslohn__N =
      sum(!is.na(monat_1_winsor)), 
    Haushaltsvorstand_Vereinbarte_Wochenarbeitszeit__Mittelwert =
      wtd.mean(az_1_winsor, gewicht), 
    Haushaltsvorstand_Vereinbarte_Wochenarbeitszeit__N =
      sum(!is.na(az_1_winsor)), 
    Haushaltsvorstand_Stundenlohn__Mittelwert =
      wtd.mean(stunde_1_winsor, gewicht), 
    Haushaltsvorstand_Stundenlohn__N =
      sum(!is.na(stunde_1_winsor)), 
    Haushaltsvorstand_Anteil_Mindestlohn__Mittelwert =
      sum((stunde_1_winsor < milo)*gewicht, na.rm=T)/
      sum((!is.na(stunde_1_winsor))*gewicht, na.rm=T),
    Haushaltsvorstand_Anteil_Mindestlohn__N =
      sum(!is.na(stunde_1_winsor))
    
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
          colNames=F, borders="surrounding")
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
                 "Arbeitszeiten für alle Personen im Haushalt, nicht nur abh. Beschäftigte! "), 
          startCol=3, startRow=ncol(Tab_4_2)+5, colNames=F)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+2, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+3, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+4, 3)
addStyle(wb, "Tab 4.2", style_anmerkung, ncol(Tab_4_2)+5, 3)

saveWorkbook(wb, paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                        "Tab_4-2_Einkommen und Löhne ",
                        "(nur Haushalte mit Milo-Berechtigung).xlsx"), 
             overwrite=T)

