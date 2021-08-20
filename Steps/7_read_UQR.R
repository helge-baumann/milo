# In den Daten für 2008 fehlen zentrale Variablen für die Vergleichbarkeit. 
evs08 <- read_dta( "./Daten/EVS/suf_evs_2008_aagshb_gf3_slr/daten/evs_2008.dta")
evs08 <- as_tibble(evs08)


# Laden der EVS 2013 und Umwandlung in tibble()
evs13 <- read_dta( "./Daten/EVS/suf_evs_2013_aagshb_gf3_slr/daten/evs_2013.dta")
evs13 <- as_tibble(evs13)

# Laden der EVS 2018 und Umwandlung in tibble()
# Es werden die Daten nach dem Update von April eingelesen
evs18 <- read_dta("./Daten/EVS/suf_evs_2018_aagshb_gf3_slr/daten/evs_2018.dta")
evs18 <- as_tibble(evs18)

# 2018 Angaben zu Monatslöhnen mergen (externe Datei)
monat <- 
  read.csv2("./Daten/EVS/suf_evs_2018_aagshb_gf3_slr/daten/monatsangaben.csv")

evs18 <- evs18 %>%
  left_join(monat, by=c("EF2U2" = "ef2u2"))

