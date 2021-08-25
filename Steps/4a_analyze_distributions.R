# Verteilung der Haushalte

# Verteilung der Haushalte insgesamt, nach Erwerbstätigen, mit abhängig Beschäftigten.
# Für eine saubere Analyse und Auswertung packen wir jetzt mal 
# die EVS 2013 und 2018 zusammen

evs13_red <- evs13 %>%
  mutate(gewicht = EF107*(nrow(evs13)/sum(evs13$EF107)), 
         westost = EF1, bland = EF2U1, ID = EF2U2,
         welle = "2013") %>%
  select(ID, welle, gewicht, westost, bland, EF38,
         starts_with(c("konsum", "cons", "c_imp", "save", "lohn", "monat", 
                       "stunde", "unterster", "milo_hh", "netto", "brutto", "hhtype",
                       "oecd", "AN", "ET")))
evs18_red <- evs18 %>%
  mutate(gewicht = EF107*(nrow(evs13)/sum(evs13$EF107)), 
         westost = EF1, bland = EF2U1, ID = EF2U2, welle = "2018") %>%
  select(ID, welle, gewicht, westost, bland, EF38,
         starts_with(c("konsum", "cons", "c_imp", "save", "lohn", "monat", 
                       "stunde", "unterster", "milo_hh", "netto", "brutto", "hhtype",
                       "oecd", "AN", "ET")))

EVS <- rbind(evs13_red, evs18_red)

# Verteilung der Gruppen

EVS %>%
  mutate(
    ET_hh = rowSums(!is.na(.[paste0("ET_", 1:6)])),
    AN_hh = rowSums(!is.na(.[paste0("AN_", 1:6)]))               
  ) %>%
  group_by(welle) %>%
  summarise(
    N__0 = sum(!is.na(ID)), N__1 = sum(gewicht), 
    N_Erwerbstätige__0 = sum(ET_hh > 0), N_Erwerbstätige__1 = sum((ET_hh > 0)*gewicht),
    N_Arbeitnehmer__0 = sum(AN_hh > 0), N_Arbeitnehmer__1 = sum((AN_hh > 0)*gewicht),
    N_Lohnempfänger__0 = sum(lohn_empfänger > 0), N_Lohnempfänger__1 = sum((lohn_empfänger > 0)*gewicht)
  ) %>%
  pivot_longer(
    -welle,
    names_to = c('.value', 'Wert'), 
    names_sep="__") %>%
  mutate(Wert = if_else(Wert == 0, "ungewichtet", "gewichtet")) %>%
  write.csv2("Output/Tabellen für Berichte/Verteilungen_Haushalte.csv")

# Verteilung der Mindestlohnhaushalte, Paarhaushalte etc. 
EVS %>%
  mutate(hhtyp = case_when(
    EF38 %in% 1:2 ~ "Single", EF38 %in% 3:5 ~ "Alleinerziehende", 
    EF38 %in% c(9:10, 21:22) ~ "Paar ohne Kinder",
    EF38 %in% c(11:20, 23:28) ~ "Paar mit Kindern",
    EF38 == 99 ~ "Sonstiger Haushalt"
  )) %>%
  group_by(welle, milo_hh_dyn, hhtyp) %>%
  summarise(n = sum(gewicht)) %>%
  mutate(p=n/sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = hhtyp, values_from=-c(welle, milo_hh_dyn, hhtyp)) %>%
  write.csv2("Output/Tabellen für Berichte/Verteilungen_Haushaltstypen.csv")
    