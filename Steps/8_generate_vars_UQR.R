

# Laden der EVS 2008 und Umwandlung in tibble()
evs08_uqr <- read_dta( "I:/2021 MLK E-012/EVS Daten Imputation/evs_2008_imputiert.dta")
evs08_uqr <- as_tibble(evs08_uqr)
  
# Laden der EVS 2013 und Umwandlung in tibble()
# Rohdaten: ("./Daten/EVS/suf_evs_2013_aagshb_gf3_slr/daten/evs_2013.dta")
# wurden ersetzt durch imputierte Daten
evs13_uqr <- read_dta( "I:/2021 MLK E-012/EVS Daten Imputation/evs_2013_imputiert.dta")
evs13_uqr <- as_tibble(evs13_uqr)
  
# Laden der EVS 2018 und Umwandlung in tibble()
# Es werden die Daten nach dem Update von April eingelesen
# Rohdaten: ("./Daten/EVS/suf_evs_2018_aagshb_gf3_slr/daten/evs_2018.dta")
# wurden ersetzt durch imputierte Daten
evs18_uqr <- read_dta( "I:/2021 MLK E-012/EVS Daten Imputation/evs_2018_imputiert.dta")
evs18_uqr <- as_tibble(evs18_uqr)

  
# EVS 2008
    
#"Form der Erwerbstätigkeit" wurde in Welle 2008 nicht vergleichbar abgefragt wie in Wellen 2013 und 2018
#Stattdessen findet sich soziale Stellung, dadurch dürften aber viele Studenten- und Rentnerjobs nicht erfasst sein
#Möglichkeit mit dem Problem umzugehen: Studenten und Rentner werden neben Haupterwerbstätigkeit kaum noch Nebenjobs haben
#Müsste man aber streng genommen noch prüfen
#Dann einfach Arbeitnehmer Variable bilden aus Soziale Stellung 
  #sowie sonstige außer Selbständige, für die sich ein Stundenlohn berechnen lässt aus den Informationen zu Lohnbestandteilen

for (i in 1:6) { # für alle Personen im Haushalt, 1-6
  
  evs08_uqr <- evs08_uqr %>%
      # Basislohn definieren
      # Wenn Mini/Midi-Job UND kein Grundlohn und kein Beamte --> Lohn aus Nebenverdienst
      mutate(
        "basislohn_{i}" := case_when(
          get(paste0("EF", 89, "U", i)) > 0 ~ get(paste0("EF", 89, "U", i)),
          get(paste0("EF", 89, "U", i)) == 0 &
            get(paste0("EF", 7 + i, "U", 13)) %in% 3:4 &
            get(paste0("EF", 7 + i, "U", 8)) != 4 ~ get(paste0("EF", 98, "U", i))
        )
      ) %>%
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
            # Nur Personen mit positivem Grundlohn (siehe "basislohn")
            get(paste0("basislohn_", i)) > 0,
          # Addition der Lohnbestandteile (Quartal)
          # EF98Ui Einnahmen aus Nebenerwerbstätigkeit nur Minijob Grundlohn 0
          # Basislohn (109 / 118), Sonderzahlungen 110, Bonus 113,
          # Essengeldzuschuss 114, Werkswohnung 120u121, 128 Nahrungsmittel, 
          # 129 Getränke, 131 Speisen und Getränke außer Haus, 132 Übernachtungen
          rowSums(.[c(
            paste0("EF", c(90, 93:94, 100:101, 107:108, 110:111), "U", i),
            paste0("basislohn_", i)
          )]), NA_real_
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
          1,  NA_real_    ),
      # Ist die Person ein Arbeitnehmer zwischen 18 und 65?
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1943:1989 &
          get(paste0("EF", 7 + i, "U13")) %in% c(1, 2, 3, 4) & 
          get(paste0("EF", 7 + i, "U8"))  %in% c(3, 4, 5, 6, 7, 8, 10, 12),
        1, NA_real_       )
      )
  
  # Labels setzen
  attributes(evs08_uqr[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs08_uqr[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs08_uqr[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs08_uqr[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)
}

  
  
# EVS 2013
  
for (i in 1:6) { # für alle Personen im Haushalt, 1-6

  evs13_uqr <- evs13_uqr %>%
    # Wenn Mini/Midi-Job UND kein Grundlohn und kein Beamte --> Lohn aus Nebenverdienst
    mutate(
      "basislohn_{i}" := case_when(
        get(paste0("EF", 109, "U", i)) > 0 ~ get(paste0("EF", 109, "U", i)),
        get(paste0("EF", 109, "U", i)) == 0 &
          get(paste0("EF", 7 + i, "U", 17)) %in% 1:2 &
          get(paste0("EF", 7 + i, "U", 8)) != 4 ~ get(paste0("EF", 118, "U", i))
      )
    ) %>%
    # weitere Lohnvariablen    
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
            # Nur Personen mit positivem Grundlohn (siehe "basislohn")
            get(paste0("basislohn_", i)) > 0,
          rowSums(.[c(
            paste0("EF", c(110, 113:114, 120:121, 128:129, 131:132), "U", i),
            paste0("basislohn_", i)
          )]), NA_real_
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
          1,  NA_real_    ),
      # Ist die Person ein Arbeitnehmer zwischen 18 und 65?
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1948:1994 &
          get(paste0("EF", 7 + i, "U14")) %in% c(1, 2),
        1, NA_real_      )
    )
      
  # Labels setzen
  attributes(evs13_uqr[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs13_uqr[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs13_uqr[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs13_uqr[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)
}
  
  
# EVS 2018
  
for (i in 1:6) { # für alle Personen im Haushalt, 1-6
    
  evs18_uqr <- evs18_uqr %>%
    # Wenn Mini/Midi-Job UND kein Grundlohn und kein Beamte --> Lohn aus Nebenverdienst
    mutate(
      "basislohn_{i}" := case_when(
        get(paste0("EF", 109, "U", i)) > 0 ~ get(paste0("EF", 109, "U", i)),
        get(paste0("EF", 109, "U", i)) == 0 &
          get(paste0("EF", 7 + i, "U", 17)) %in% 1:2 &
          get(paste0("EF", 7 + i, "U", 8)) != 4 ~ get(paste0("EF", 118, "U", i))
      )
    ) %>%
    # weitere Lohnvariablen    
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
            # Nur Personen mit positivem Grundlohn (siehe "basislohn")
            get(paste0("basislohn_", i)) > 0,
          # Addition der Lohnbestandteile (Quartal)
          # EF118Ui Einnahmen aus Nebenerwerbstätigkeit nur Minijob (Grundlohn 0)
          # Basislohn (109 / 118), Sonderzahlungen 110, Bonus 113,
          # Essengeldzuschuss 114, Werkswohnung 120u121, 128 Nahrungsmittel,
          # 129 Getränke, 131 Speisen und Getränke außer Haus, 132 Übernachtungen
          rowSums(.[c(
            paste0("EF", c(110, 113:114, 120:121, 128:129, 131:132), "U", i),
            paste0("basislohn_", i)
          )]), NA_real_
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
          1,  NA_real_    ),
      # Person i Arrbeitnehmer:in 18 bis 65?
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 &
          get(paste0("EF", 7 + i, "U15")) %in% c(1, 2), # Arbeitnehmer
        1, NA_real_      )
    )
  
  # Labels setzen
  attributes(evs18_uqr[[paste0("lohn_", i)]])$label <- 
    paste0("Quartalslohn Person ", i)
  attributes(evs18_uqr[[paste0("monat_", i)]])$label <- 
    paste0("Monatslohn Person ", i)
  attributes(evs18_uqr[[paste0("woche_", i)]])$label <- 
    paste0("Wochenlohn Person ", i)
  attributes(evs18_uqr[[paste0("stunde_", i)]])$label <- 
    paste0("Bruttostundenlohn Person ", i)
  
}


write_dta(evs08_uqr, paste0("I:/2021 MLK E-012/EVS Daten Imputation/", "UQR_EVS2008.dta"))
write_dta(evs13_uqr, paste0("I:/2021 MLK E-012/EVS Daten Imputation/", "UQR_EVS2013.dta"))
write_dta(evs18_uqr, paste0("I:/2021 MLK E-012/EVS Daten Imputation/", "UQR_EVS2018.dta"))

# save Workspace
#save.image(file=paste0("./Output/WS_", Sys.Date(), ".RData"))
