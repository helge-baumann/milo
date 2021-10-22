# Personen-Variablen generieren (Löhne, Arbeitszeiten, Beschäftigung etc.)

for (i in 1:6) { # für alle Personen im Haushalt, 1-6

  # Basislohn definieren
  evs13 <- 
    evs13 %>%
    # Wenn Mini/Midi-Job UND kein Grundlohn --> Lohn aus Nebenverdienst
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
      # Quartalslohn (zahlreiche Bedingungen)
      "lohn_{i}" :=
        if_else(
          # Alter: 18 bis 65
          # Altersbegrenzung von DIW Studie bei 64,
          # Renteneintritt aber über Vollendung 65,
          # danach wenig Stundenlöhne in EVS
          get(paste0("EF", 7 + i, "U3")) %in% 1948:1994 &
            # ohne Personen mit Abfindungen etc.
            get(paste0("EF", 112, "U", i)) == 0 &
            # ohne Personen mit Mutterschaftsgeld
            get(paste0("EF", 153, "U", i)) == 0 &
            # ohne Personen mit Elterngeld
            get(paste0("EF", 158, "U", i)) == 0 &
            # keine mithelfenden Familienangehörigen
            get(paste0("EF", 7 + i, "U8")) != 3 &
            # Arbeitnehmer (befristet und unbefristet)
            get(paste0("EF", 7 + i, "U14")) %in% c(1, 2) &
            # Angabe zur Arbeitszeit (Arbeitszeit > 0)
            get(paste0("EF", 7 + i, "U16")) > 0 &
            # kein überwiegender Lebensunterhalt Altersteilzeit
            get(paste0("EF", 7 + i, "U12")) != 2 &
            # keine Angabe bei Altersteilzeit-Entgelt
            get(paste0("EF", 119, "U", i)) == 0 &
            # Nur Personen mit positivem Grundlohn (siehe "basislohn")
            get(paste0("basislohn_", i)) > 0,
          # Addition der Lohnbestandteile (Quartal)
          # EF118Ui Einnahmen aus Nebenerwerbstätigkeit nur Minijob Grundlohn 0
          # Basislohn (109 / 118), Sonderzahlungen 110, Bonus 113,
          # Essengeldzuschuss 114, Werkswohnung 120u121, 128 Nahrungsmittel
          rowSums(.[c(
            paste0("EF", c(110, 113:114, 120:121, 128), "U", i),
            paste0("basislohn_", i)
          )]), NA_real_
        ),
      # Bruttomonatslohn
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlohn
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlohn
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U16")),
      # Vereinbarte Wochenarbeitszeit (0 = trifft nicht zu / keine Angabe)
      "az_{i}" := case_when(
        get(paste0("EF", 7 + i, "U16")) > 0  & !is.na(get(paste0("lohn_", i))) ~
        get(paste0("EF", 7 + i, "U16"))
      ),
      # Ist die Person ein Arbeitnehmer zwischen 18 und 65?
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1948:1994 &
          get(paste0("EF", 7 + i, "U14")) %in% c(1, 2),
        1, NA_real_
      ),
      # Ist die Person i erwerbstätig?
      "ET_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1948:1994 &
          get(paste0("EF", 7 + i, "U14")) %in% 1:5,
        1, NA_real_
      ),
      "sex_{i}" := case_when(
        !is.na(get(paste0("lohn_", i))) ~ get(paste0("EF", 7 + i, "U2"))
      ),
      "vollzeit_{i}" := case_when(
        get(paste0("az_", i)) < 30 & !is.na(get(paste0("lohn_", i))) ~ 0,
        get(paste0("az_", i)) >= 30 & !is.na(get(paste0("lohn_", i))) ~ 1
      ),
      "minijob_{i}" := case_when(
        get(paste0("EF", 7 + i, "U", 17)) %in% 1:2 & !is.na(get(paste0("lohn_", i))) ~ 1
      )
    )

  # Labels setzen
  attributes(evs13[[paste0("basislohn_", i)]])$label <-
    paste0("Grundlohn Person ", i)
  attributes(evs13[[paste0("lohn_", i)]])$label <-
    paste0("Quartalslohn Person ", i)
  attributes(evs13[[paste0("monat_", i)]])$label <-
    paste0("Monatslohn Person ", i)
  attributes(evs13[[paste0("woche_", i)]])$label <-
    paste0("Wochenlohn Person ", i)
  attributes(evs13[[paste0("stunde_", i)]])$label <-
    paste0("Bruttostundenlohn Person ", i)
  attributes(evs13[[paste0("stunde_", i)]])$label <-
    paste0("Wochenarbeitszeit Person ", i)
  attributes(evs13[[paste0("ET_", i)]])$label <-
    paste0("Person ", i, " ist erwerbstätig")
  attributes(evs13[[paste0("AN_", i)]])$label <-
    paste0("Person ", i, " ist Arbeitnehmer:in")
  attributes(evs13[[paste0("sex_", i)]])$label <-
    paste0("Person ", i, ": Geschlecht (nur für abh. Beschäftigte)")
  attributes(evs13[[paste0("vollzeit_", i)]])$label <-
    paste0("Person ", i, ": Teilzeit (0) / Vollzeit (1) (nur für abh. Beschäftigte)")
  attributes(evs13[[paste0("minijob_", i)]])$label <-
    paste0("Person ", i, ": Minijob (nur für abh. Beschäftigte)")

  # EVS 2018 
  # Achtung: Einige Variablen mit gleichen Namen, aber anderem Inhalt als 2013
  evs18 <- 
    evs18 %>%
    # Wenn Mini/Midi-Job UND kein Grundlohn --> Lohn aus Nebenverdienst
    mutate(
      "basislohn_{i}" := case_when(
        get(paste0("EF", 109, "U", i)) > 0 ~ get(paste0("EF", 109, "U", i)),
        get(paste0("EF", 109, "U", i)) == 0 &
          get(paste0("EF", 7 + i, "U", 18)) %in% 1:2 &
          get(paste0("EF", 7 + i, "U", 9)) != 5 ~ get(paste0("EF", 118, "U", i))
      )
    ) %>%
    # weitere Lohnvariablen
    mutate(
      # Quartalslohn (zahlreiche Bedingungen)
      "lohn_{i}" :=
        if_else(
          # Alter: 18 bis 65
          # Altersbegrenzung von DIW Studie bei 64, 
          # Renteneintritt aber über Vollendung 65,
          # danach wenig Stundenlöhne in EVS
          get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 &
            # ohne Personen mit Abfindungen etc.
            get(paste0("EF", 112, "U", i)) == 0 &
            # ohne Personen mit Mutterschaftsgeld
            get(paste0("EF", 153, "U", i)) == 0 &
            # ohne Personen mit Elterngeld
            get(paste0("EF", 158, "U", i)) == 0 &
            # Arbeitnehmer (befristet und unbefristet)
            get(paste0("EF", 7 + i, "U15")) %in% c(1, 2) &
            # Arbeitszeit > 0
            get(paste0("EF", 7 + i, "U17")) > 0 &
            # kein überwiegender Lebensunterhalt Altersteilzeit
            get(paste0("EF", 7 + i, "U13")) != 2 &
            # keine Angabe bei Altersteilzeit Entgelt
            get(paste0("EF", 119, "U", i)) == 0 &
            # Nur Personen mit positivem Grundlohn (siehe basislohn)
            get(paste0("basislohn_", i)) > 0,
          # Addition der Lohnbestandteile (Quartal)
          # EF118Ui Einnahmen aus Nebenerwerbstätigkeit nur Minijob (Grundlohn 0)
          # Basislohn (109 / 118), Sonderzahlungen 110, Bonus 113,
          # Essengeldzuschuss 114, Werkswohnung 120u121, 128 Nahrungsmittel
          rowSums(.[c(
            paste0("EF", c(110, 113:114, 120:121, 128), "U", i),
            paste0("basislohn_", i)
          )]), NA_real_
        ),
      # Bruttomonatslöhne
      "monat_{i}" := get(paste0("lohn_", i)) / 3,
      # Bruttowochenlöhne
      "woche_{i}" := get(paste0("monat_", i)) / 4.33,
      # Bruttostundenlöhne
      "stunde_{i}" := get(paste0("woche_", i)) / get(paste0("EF", 7 + i, "U17")),
      # Vereinbarte Wochenarbeitszeit (0 = trifft nicht zu / keine Angabe)
      "az_{i}" := case_when(
        get(paste0("EF", 7 + i, "U17")) > 0 & !is.na(get(paste0("lohn_", i))) ~
      get(paste0("EF", 7 + i, "U17"))),
      # Person i Arrbeitnehmer:in 18 bis 65?
      "AN_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 &
          get(paste0("EF", 7 + i, "U15")) %in% c(1, 2), # Arbeitnehmer
        1, NA_real_
      ),
      # Person i erwerbstätig?
      "ET_{i}" := if_else(
        get(paste0("EF", 7 + i, "U3")) %in% 1953:1999 &
          get(paste0("EF", 7 + i, "U15")) %in% 1:5, # Erwerbstätige
        1, NA_real_
      ),
      "sex_{i}" := case_when(
        !is.na(get(paste0("lohn_", i))) ~ get(paste0("EF", 7 + i, "U2"))
      ),
      "vollzeit_{i}" := case_when(
        get(paste0("az_", i)) < 30 & !is.na(get(paste0("lohn_", i))) ~ 0,
        get(paste0("az_", i)) >= 30 & !is.na(get(paste0("lohn_", i))) ~ 1
      ),
      "minijob_{i}" := case_when(
        get(paste0("EF", 7 + i, "U", 18)) %in% 1:2 & !is.na(get(paste0("lohn_", i))) ~ 1
      )
    )

  # Labels setzen
  attributes(evs18[[paste0("basislohn_", i)]])$label <-
    paste0("Grundlohn Person ", i)
  attributes(evs18[[paste0("lohn_", i)]])$label <-
    paste0("Quartalslohn Person ", i)
  attributes(evs18[[paste0("monat_", i)]])$label <-
    paste0("Monatslohn Person ", i)
  attributes(evs18[[paste0("woche_", i)]])$label <-
    paste0("Wochenlohn Person ", i)
  attributes(evs18[[paste0("stunde_", i)]])$label <-
    paste0("Bruttostundenlohn Person ", i)
  attributes(evs18[[paste0("stunde_", i)]])$label <-
    paste0("Wochenarbeitszeit Person ", i)
  attributes(evs18[[paste0("ET_", i)]])$label <-
    paste0("Person ", i, " ist erwerbstätig")
  attributes(evs18[[paste0("AN_", i)]])$label <-
    paste0("Person ", i, " ist Arbeitnehmer:in")
  attributes(evs18[[paste0("sex_", i)]])$label <-
    paste0("Person ", i, ": Geschlecht (nur für abh. Beschäftigte)")
  attributes(evs18[[paste0("vollzeit_", i)]])$label <-
    paste0("Person ", i, ": Teilzeit (0) / Vollzeit (1) (nur für abh. Beschäftigte)")
  attributes(evs18[[paste0("minijob_", i)]])$label <-
    paste0("Person ", i, ": Minijob (nur für abh. Beschäftigte)")

}
