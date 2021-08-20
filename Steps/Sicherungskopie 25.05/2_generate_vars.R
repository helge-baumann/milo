# Berechnung von Bruttolöhnen (Quartal, Monat, Woche, Stunde)

for (i in 1:6) { # für alle Personen im Haushalt, 1-6
  
  # EVS 2013
  evs13 <- evs13 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1949:1994 & # Alter: 18 bis 64
          get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
          #get(paste0("EF", 7 + i, "U8")) != 4 & # keine Beamten
          get(paste0("EF", 7 + i, "U13")) == 1 & # vollständige Angaben
          get(paste0("EF", 7 + i, "U14")) %in% c(1, 2) & # Arbeitnehmer
          get(paste0("EF", 7 + i, "U16")) > 0 & # Angabe zur Arbeitszeit
          get(paste0("EF", 119, "U", i)) == 0 , # keine Angabe bei Altersteilzeit Entgelt          
          #get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          # Deflationierung auf 2015er Wert (VPI Destatis 2015 100%, 2013 98,6)
          # EF118Ui           Einnahmen aus Nebenerwerbstätigkeit Person i wird nicht berücksichtigt
          # EF119Ui           Altersteilzeitentgelt Person i wird nicht berücksichtigt
          rowSums(.[paste0("EF", c(109:110, 113:114, 120:121, 127, 128), "U", i)])*100/98.6,
          NA_real_
        ),
      # Bruttomonatslohn
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlohn
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlohn
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U16"))
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
          get(paste0("EF", 7 + i, "U3")) %in% 1954:1999 & # Alter: 18 bis 64
          get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
          #get(paste0("EF", 7 + i, "U9")) != 5 & # keine Beamten
          get(paste0("EF", 7 + i, "U14")) == 1 & # vollständige Angaben 
            #(Stata: gen mark = 1 if EF109U1 > 0 & EF8U17 > 0 & EF8U14 != 1 & (EF8U15 == 1 | EF8U15 == 2)
            #tab mark -> ) ca. 300 Stundenlohnn-Angaben für Arbeitnehmer Person Nr. 1 möglich
          get(paste0("EF", 7 + i, "U15")) %in% c(1, 2) & # Arbeitnehmer
          get(paste0("EF", 7 + i, "U17")) > 0 & # Arbeitszeit
          get(paste0("EF", 119, "U", i)) == 0 , # keine Angabe bei Altersteilzeit Entgelt          
          #get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          # Deflationierung auf 2015er Wert (VPI Destatis 2015 100%, 2013 98,6)
          # EF118Ui           Einnahmen aus Nebenerwerbstätigkeit Person i wird nicht berücksichtigt
          # EF119Ui           Altersteilzeitentgelt Person i wird nicht berücksichtigt
          rowSums(.[paste0("EF", c(109:110, 113:114, 120:121, 127, 128), "U", i)]),
          NA_real_
        ),
      # Bruttomonatslöhne
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlöhne
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlöhne
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U17"))
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