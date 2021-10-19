# Rohdaten laden

# Laden der EVS 2008: vorerst obsolet (08.10.2021)
  # In den Daten für 2008 fehlen zentrale Variablen für die Vergleichbarkeit. 

# Laden der EVS 2013 und Umwandlung in tibble()
 # Rohdaten: ("./Daten/EVS/suf_evs_2013_aagshb_gf3_slr/daten/evs_2013.dta")
 # wurden ersetzt durch imputierte Daten
evs13 <- read_dta( "./Daten/EVS/evs_2013_imputiert.dta")
evs13 <- as_tibble(evs13)

# Laden der EVS 2018 und Umwandlung in tibble()
  # Es werden die Daten nach dem Update von April eingelesen
  # Rohdaten: ("./Daten/EVS/suf_evs_2018_aagshb_gf3_slr/daten/evs_2018.dta")
  # wurden ersetzt durch imputierte Daten
evs18 <- read_dta( "./Daten/EVS/evs_2018_imputiert.dta")
evs18 <- as_tibble(evs18)

# Mergen mit Monatslöhnen: entfernt nach Absprache mit MLK (08.10.2021)

# Deflationierung (nur als Hilfsdatensatz)
defl <- read.csv2("Daten/Deflationierung.csv", header=F)
