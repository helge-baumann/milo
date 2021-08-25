
# Löhne generieren----

for (i in 1:6) { # für alle Personen im Haushalt, 1-6
   
  # EVS 2013
  evs13 <- evs13 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1948:1994 & # Alter: 18 bis 65
            #Altersbegrenzung von DIW Studie bei 64, Renteneintritt aber über Vollendung 65, 
            #danach wenig Stundenlöhne in EVS
            get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
            get(paste0("EF", 153, "U", i)) == 0 & # ohne Personen mit Mutterschaftsgeld
            get(paste0("EF", 158, "U", i)) == 0 & # ohne Personen mit Elterngeld
            get(paste0("EF", 7 + i, "U8")) != 3 & # keine mithelfenden Familienangehörigen
            #get(paste0("EF", 7 + i, "U13")) == 1 & # vollständige Angaben
            get(paste0("EF", 7 + i,  "U14")) %in% c(1, 2) & # Arbeitnehmer
            get(paste0("EF", 7 + i, "U16")) > 0 & # Angabe zur Arbeitszeit
            get(paste0("EF", 7 + i, "U12")) != 2 & # kein überwiegender Lebensunterhalt Altersteilzeit   
            get(paste0("EF", 119, "U", i)) == 0 & # keine Angabe bei Altersteilzeit Entgelt          
            get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          # Deflationierung auf 2015er Wert (VPI Destatis 2015 100%, 2013 98,6) *100/98.6
          # EF118Ui           Einnahmen aus Nebenerwerbstätigkeit Person i wird nicht berücksichtigt
          # EF119Ui           Altersteilzeitentgelt Person i wird nicht berücksichtigt
          # berücksichtigt Grundlohn 109, Sonderzahlungen 110, Bonus 113, Essengeldzuschuss 114, Werkwohnung 120u121, 128 Nahrungsmittel
          rowSums(.[paste0("EF", c(109:110, 113:114, 120:121, 128), "U", i)]),  NA_real_ ),
      # Bruttomonatslohn
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlohn
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlohn
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U16")),
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 & 
          get(paste0("EF", 7 + i, "U14")) %in% c(1, 2), # Arbeitnehmer
        1,  NA_real_    ),
      "ET_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 & 
          get(paste0("EF", 7 + i, "U14")) %in% 1:5, # Erwerbstätige
        1,  NA_real_    )
    )
  
  # Labels setzen
  attributes(evs13[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs13[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs13[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs13[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)
  
  
  # EVS 2018
  evs18 <- evs18 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 & # Alter: 18 bis 65
            #Altersbegrenzung von DIW Studie bei 64, Renteneintritt aber über Vollendung 65, danach wenig Stundenlöhne in EVS
            get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
            get(paste0("EF", 153, "U", i)) == 0 & # ohne Personen mit Mutterschaftsgeld
            get(paste0("EF", 158, "U", i)) == 0 & # ohne Personen mit Elterngeld
            #get(paste0("EF", 7 + i, "U9")) != 5 & # keine Beamten
            #get(paste0("EF", 7 + i, "U14")) == 1 & # vollständige Angaben 
            #(Stata: gen mark = 1 if EF109U1 > 0 & EF8U17 > 0 & EF8U14 != 1 & (EF8U15 == 1 | EF8U15 == 2)
            #tab mark -> ) ca. 300 Stundenlohnn-Angaben für Arbeitnehmer Person Nr. 1 möglich
            get(paste0("EF", 7 + i, "U15")) %in% c(1, 2) & # Arbeitnehmer
            get(paste0("EF", 7 + i, "U17")) > 0 & # Arbeitszeit
            get(paste0("EF", 7 + i, "U12")) != 2 & # kein überwiegender Lebensunterhalt Altersteilzeit   
            get(paste0("EF", 119, "U", i)) == 0 & # keine Angabe bei Altersteilzeit Entgelt          
            get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          # Deflationierung auf 2015er Wert (VPI Destatis 2015 100%, 2013 98,6)
          # EF118Ui           Einnahmen aus Nebenerwerbstätigkeit Person i wird nicht berücksichtigt
          # EF119Ui           Altersteilzeitentgelt Person i wird nicht berücksichtigt
          # berücksichtigt Grundlohn 109, Sonderzahlungen 110, Bonus 113, Essengeldzuschuss 114, Werkwohnung 120u121, 128 Nahrungsmittel
          rowSums(.[paste0("EF", c(109:110, 113:114, 120:121, 128), "U", i)]),  NA_real_ ),
      # Bruttomonatslöhne
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlöhne
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlöhne
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U17")),
      "AN_{i}" := if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 & 
            get(paste0("EF", 7 + i, "U15")) %in% c(1, 2), # Arbeitnehmer
          1,  NA_real_    ),
      "ET_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 & 
          get(paste0("EF", 7 + i, "U15")) %in% 1:5, # Erwerbstätige
        1,  NA_real_    )
      
    )
  
  # Labels setzen
  attributes(evs18[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs18[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs18[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs18[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)
  
}

# Winsorizing----


evs13 <- evs13 %>%
  mutate(netto_oecd = EF62/oecd_weight, brutto=EF60) %>%
  mutate(across(
    starts_with(c("lohn", "monat", "stunde", "konsum", "c_imp", "cons", 
                  "save_imp", "save", "netto_oecd", "brutto")), 
    .fns = list(winsor = ~winsor(.))),
    lohn_summe =  rowSums(.[paste0("lohn_", 1:6)], na.rm=T),
    lohn_summe_winsor = winsor(lohn_summe),
    lohn_anteil = if_else(lohn_summe > 0, lohn_summe/brutto*100, NA_real_),
    lohn_anteil_winsor = winsor(lohn_anteil)
    ) 

evs18 <- evs18 %>%
  mutate(netto_oecd = EF62/oecd_weight, brutto=EF60) %>%
  mutate(across(
    starts_with(c("lohn", "monat", "stunde", "konsum", "c_imp", "cons", 
                  "save_imp", "save", "netto_oecd", "brutto")), 
    .fns = list(winsor = ~winsor(.))),
    lohn_summe =  rowSums(.[paste0("lohn_", 1:6)], na.rm=T),
    lohn_summe_winsor = winsor(lohn_summe),
    lohn_anteil = if_else(lohn_summe > 0, lohn_summe/brutto*100, NA_real_),
    lohn_anteil_winsor = winsor(lohn_anteil)
  ) 

# Plausibilitätscheck
ws <- names(evs13)[str_detect(names(evs13), "winsor") ]
ws_o <- str_remove_all(ws, "_winsor")

winsor_13 <- evs13 %>% 
  select(ws, ws_o) %>%
  summarise_all(.funs = list(mmin = ~ min(x = ., na.rm=T),
                             mmedian = ~ median(x =., na.rm=T),
                             mmean = ~mean(x=., na.rm=T),
                             mmax = ~ max(x = ., na.rm=T))) %>%
  pivot_longer(
    everything(),
    names_to = c('.value', 'Wert'), 
    names_sep="_m") 
 
names(winsor_13)[!str_detect(names(winsor_13), "winsor")] <- 
  paste0(
    names(winsor_13)[!str_detect(names(winsor_13), "winsor")], 
    "_winsor_ohne")

winsor_13 <- winsor_13 %>%
  pivot_longer(
    -Wert_winsor_ohne,
    names_to = c('.value', 'Wert2'), 
    names_sep="_winso") %>%
  select(ws_o) %>%
  t()

winsor_18 <- evs18 %>% 
  select(ws, ws_o) %>%
  summarise_all(.funs = list(mmin = ~ min(x = ., na.rm=T),
                             mmedian = ~ median(x =., na.rm=T),
                             mmean = ~mean(x=., na.rm=T),
                             mmax = ~ max(x = ., na.rm=T))) %>%
  pivot_longer(
    everything(),
    names_to = c('.value', 'Wert'), 
    names_sep="_m") 

names(winsor_18)[!str_detect(names(winsor_18), "winsor")] <- 
  paste0(names(winsor_18)[!str_detect(names(winsor_18), "winsor")], "_winsor_ohne")

winsor_18 <- winsor_18 %>%
  pivot_longer(
    -Wert_winsor_ohne,
    names_to = c('.value', 'Wert2'), 
    names_sep="_winso") %>%
  select(ws_o) %>%
  t()

wb <- createWorkbook()
addWorksheet(wb, "Winsorizing")
writeData(wb, "Winsorizing", 
          "Vergleich winsorizeder Löhne mit nicht-winsorizeden Löhnen, 2013 und 2018",
          startCol=1, startRow=1)
writeData(wb, "Winsorizing", winsor_13, startCol=1, startRow=6, rowNames=T, colNames=F)
writeData(wb, "Winsorizing", winsor_18, startCol=2+ncol(winsor_13), startRow=6, colNames=F)
writeData(wb, "Winsorizing", 
          t(c(rep("2013", ncol(winsor_13)), rep("2018", ncol(winsor_18)))), 
          startRow=3, startCol=2, colNames=F)
writeData(wb, "Winsorizing", 
          t(rep(rep(c("Min", "Median", "Mean", "Max"), each=2), 2)), 
          startRow=4, startCol=2, colNames=F)
writeData(wb, "Winsorizing", 
          t(rep(c("mit Winsor", "ohne Winsor"), 8)), 
          startRow=5, startCol=2, colNames=F)
saveWorkbook(wb, "./Output/Tabellen für Berichte/Winsor_Vergleich.xlsx", overwrite=T)
