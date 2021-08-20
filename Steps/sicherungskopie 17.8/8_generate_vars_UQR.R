
for (i in 1:6) { # für alle Personen im Haushalt, 1-6
  
#"Form der Erwerbstätigkeit" wurde in Welle 2008 nicht vergleichbar abgefragt wie in Wellen 2013 und 2018
#Stattdessen findet sich soziale Stellung, dadurch dürften aber viele Studenten- und Rentnerjobs nicht erfasst sein
#Möglichkeit mit dem Problem umzugehen: Studenten und Rentner werden neben Haupterwerbstätigkeit kaum noch Nebenjobs haben
#Müsste man aber streng genommen noch prüfen
#Dann einfach Arbeitnehmer Variable bilden aus Soziale Stellung 
  #sowie sonstige außer Selbständige, für die sich ein Stundenlohn berechnen lässt aus den Informationen zu Lohnbestandteilen

# EVS 2008
  evs08 <- evs08 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1900:1990 & # Alter: 18 bis ..
            #Altersbegrenzung von SOEP Studie ist letztlich willkürlich, auch Rentner arbeiten häufig nebenbei
            #dazu häufig noch im gering entlohnten Minijob
            get(paste0("EF", 92, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
            get(paste0("EF", 7 + i, "U8")) %in% c(3, 4, 5, 6, 7, 8, 10, 12) & # Arbeitnehmer definiert als Nicht selbstständige
            #ACHTUNG! IN EVS 2008 lassen sich auszubildende nicht identifizieren, diese sind bei den Angestellten und Arbeitern mit drin
            #Mögliche Approximation: bis 22 Jährige ohne Berufsabschluss raussortieren, Azubis in zwischen 18 u. 22 Jahren machen 78% der Fälle in SOEP aus
            #ALTERNATIVE: WIR LASSEN DIE AZUBIS DRIN, Problem: dann verzerrte Schätzung ML Effekt unten
            get(paste0("EF", 7 + i, "U14")) > 0 & # Angabe zur Arbeitszeit
            #Achtung: hier anders codiert (4 anstelle 2 in den folgenden Wellen)
            get(paste0("EF", 7 + i, "U12")) != 4 & # kein überwiegender Lebensunterhalt Altersteilzeit   
            get(paste0("EF", 99, "U", i)) == 0 & # keine Angabe bei Altersteilzeit Entgelt          
            get(paste0("EF", 89, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          rowSums(.[paste0("EF", c(89:90, 93:94, 100:101, 107, 108), "U", i)]),
          NA_real_
        ),
      # Bruttomonatslohn
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlohn
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlohn
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U14")),
      #AZUBI APPROXIMATION
      "Azubi_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1986:2008 & # Alter bis 22..
            #Altersbegrenzung von SOEP Studie ist letztlich willkürlich, auch Rentner arbeiten häufig nebenbei
            #dazu häufig noch im gering entlohnten Minijob
            get(paste0("EF", 7 + i, "U7")) %in% c(1), # kein Berufsabschluss
          1,  NA_real_    )
    )
  
  # Labels setzen
  attributes(evs08[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs08[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs08[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs08[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)


  # EVS 2013
  evs13 <- evs13 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1900:1994 & # Alter: 18 bis ..
            #Altersbegrenzung von SOEP Studie ist letztlich willkürlich, auch Rentner arbeiten häufig nebenbei
            #dazu häufig noch im gering entlohnten Minijob
            get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
            #get(paste0("EF", 7 + i, "U8")) != 4 & # keine Beamten
            #get(paste0("EF", 7 + i, "U13")) == 1 & # vollständige Angaben
            get(paste0("EF", 7 + i, "U8")) %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) & # Arbeitnehmer definiert als Nicht selbstständige
            #get(paste0("EF", , "U14")) %in% c(1, 2) & # Form der Erwerbstätigkeit Arbeitnehmer ab EVS neu abgefragt, hier aber nicht verwendet
            #um konsistent zu EVS 2008 zu sein (wichtig bei Studenten und Rentner Jobs, die ließen sich ab EVS 2013 besser abgrenzen)
            # wegen Konsistenz hier nicht ausgewählt (2008 nicht verfügbar) get(paste0("EF", 7 + i, "U14")) %in% c(0, 1, 2; 3) & # AZUBIS (3) BLEIBEN DRIN; SONST INKONSISTENZ MIT WELLE 2008
            get(paste0("EF", 7 + i, "U16")) > 0 & # Angabe zur Arbeitszeit
            get(paste0("EF", 7 + i, "U12")) != 2 & # kein überwiegender Lebensunterhalt Altersteilzeit   
            get(paste0("EF", 119, "U", i)) == 0 & # keine Angabe bei Altersteilzeit Entgelt          
            get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
          # Addition der Lohnbestandteile (Quartal)
          # Deflationierung auf 2015er Wert (VPI Destatis 2015 100%, 2013 98,6) *100/98.6
          # EF118Ui           Einnahmen aus Nebenerwerbstätigkeit Person i wird nicht berücksichtigt
          # EF119Ui           Altersteilzeitentgelt Person i wird nicht berücksichtigt
          rowSums(.[paste0("EF", c(109:110, 113:114, 120:121, 127, 128), "U", i)]),
          NA_real_
        ),
      # Bruttomonatslohn
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlohn
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlohn
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U16")),
      #AZUBI APPROXIMATION
      "Azubi_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1991:2013 & # Alter bis 22..
            get(paste0("EF", 7 + i, "U7")) %in% c(1), # kein Berufsabschluss
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
          get(paste0("EF", 7 + i, "U3")) %in% 1900:1999 & # Alter: 18 bis ..
            #Altersbegrenzung von SOEP Studie ist letztlich willkürlich, auch Rentner arbeiten häufig nebenbei
            #dazu häufig noch im gering entlohnten Minijob
            get(paste0("EF", 112, "U", i)) == 0 & # ohne Personen mit Abfindungen etc.
            #get(paste0("EF", 7 + i, "U9")) != 5 & # keine Beamten
            #get(paste0("EF", 7 + i, "U14")) == 1 & # vollständige Angaben 
            #(Stata: gen mark = 1 if EF109U1 > 0 & EF8U17 > 0 & EF8U14 != 1 & (EF8U15 == 1 | EF8U15 == 2)
            #tab mark -> ) ca. 300 Stundenlohn-Angaben für Arbeitnehmer Person Nr. 1 möglich
            get(paste0("EF", 7 + i, "U9")) %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) & # Arbeitnehmer definiert als Nicht selbstständige 
            # Azubis, Bundesfreiwilligendienst bleiben drin, sonst Inkonsistenz
            # wegen Konsistenz hier nicht ausgewählt (2008 nicht verfügbar) get(paste0("EF", 7 + i, "U15")) %in% c(1, 2) & # Arbeitnehmer # Form der Erwerbstätigkeit Arbeitnehmer ab EVS neu abgefragt, hier aber nicht verwendet
            get(paste0("EF", 7 + i, "U17")) > 0 & # Arbeitszeit
            get(paste0("EF", 7 + i, "U12")) != 2 & # kein überwiegender Lebensunterhalt Altersteilzeit   
                        get(paste0("EF", 119, "U", i)) == 0 & # keine Angabe bei Altersteilzeit Entgelt          
            get(paste0("EF", 109, "U", i)) > 0, # Nur Personen mit positivem Grundlohn
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
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U17")),
      #AZUBI APPROXIMATION
      "Azubi_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1996:2018 & # Alter bis 22
            get(paste0("EF", 7 + i, "U7")) %in% c(1), # kein Berufsabschluss
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

dir.create("./Output/Daten", showWarnings=F)
write_dta(evs08, paste0("./Output/Daten/", "UQR_EVS2008.dta"))
write_dta(evs13, paste0("./Output/Daten/", "UQR_EVS2013.dta"))
write_dta(evs18, paste0("./Output/Daten/", "UQR_EVS2018.dta"))

# save Workspace
#save.image(file=paste0("./Output/WS_", Sys.Date(), ".RData"))
