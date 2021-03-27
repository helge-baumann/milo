# Berechnung von Bruttolöhnen (Quartal, Monat, Woche, Stunde)

for (i in 1:6) { # für alle Personen im Haushalt, 1-6
  
  # EVS 2013
  evs13 <- evs13 %>%
    mutate(
      "lohn_{i}" :=
        if_else(
          get(paste0("EF", 7 + i, "U3")) %in% 1949:1994 & # Alter: 18 bis 64
          get(paste0("EF", 7 + i, "U8")) != 4 & # keine Beamten
          get(paste0("EF", 7 + i, "U13")) == 1 & # vollständige Angaben
            get(paste0("EF", 7 + i, "U14")) %in% c(1, 2) & # Arbeitnehmer
            get(paste0("EF", 7 + i, "U16")) > 0, # Angabe zur Arbeitszeit
          # Addition der Lohnbestandteile (Quartal)
          rowSums(.[paste0("EF", c(109:110, 112:114, 118:121, 127, 128), "U", i)]),
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
          get(paste0("EF", 7 + i, "U9")) != 5 & # keine Beamten
          get(paste0("EF", 7 + i, "U14")) == 1 & # vollständige Angaben
            get(paste0("EF", 7 + i, "U15")) %in% c(1, 2) & # Arbeitnehmer
            get(paste0("EF", 7 + i, "U17")) > 0, # Arbeitszeit
          # Addition der Lohnbestandteile (Quartal)
          rowSums(.[paste0("EF", c(109:110, 112:114, 118:121, 127, 128), "U", i)]),
          NA_real_
        ),
      # Bruttomonatslöhne
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlöhne
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlöhne
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U17")),
      # Bruttomonatslöhne (alternative Berechnung mit Grundlohnmonaten)
      "monat_{i}_alt" := if_else(get(paste0("M0011110_0", i)) > 0, 
                                 get(paste0("lohn_", i)) / 
                                   get(paste0("M0011110_0", i)), NA_real_),
      # Bruttowochenlöhne (alternative Berechnung mit Grundlohnmonaten)
      "woche_{i}_alt" := get(paste0("monat_", i, "_alt")) / 4.34524,
      # Bruttomonatslöhne (alternative Berechnung mit Grundlohnmonaten)
      "stunde_{i}_alt" := get(paste0("woche_", i, "_alt")) / 
        get(paste0("EF", 7 + i, "U17"))
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
  attributes(evs18[[paste0("monat_", i, "_alt")]])$label <- 
    paste0("Monatslohn (alternativ) Person ", i)
  attributes(evs18[[paste0("woche_", i, "_alt")]])$label <- 
    paste0("Wochenlohn (alternativ) Person ", i)
  attributes(evs18[[paste0("stunde_", i, "_alt")]])$label <- 
    paste0("Bruttostundenlohn (alternativ) Person ", i)
  
}