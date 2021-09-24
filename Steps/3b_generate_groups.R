# Analyse Verteilung von Löhnen, Haushalte und Personen
# Zuweisung Mindestlohnhaushalte

# Anteil Personen in Einkommens-Kategorien
P <- data.frame(
  "Lohn" = c(
    "unter 8 Euro", "8 bis unter 8,50 Euro", "8,50 bis unter 8,84 Euro",
    "8,84 bis unter 9,50 Euro", "9,50 bis unter 12 Euro", "12 Euro und mehr"
  ),
  "EVS_2013" = c(
    sum_pers(evs13, "stunde", lower = 0, upper = 8 / 100 * 98.6, w=evs13$EF107),
    sum_pers(evs13, "stunde", lower = 8 / 100 * 98.6, upper = 8.5 / 100 * 98.6, w=evs13$EF107),
    sum_pers(evs13, "stunde", lower = 8.5 / 100 * 98.6, upper = 8.84 / 100 * 98.6, w=evs13$EF107),
    sum_pers(evs13, "stunde", lower = 8.84 / 100 * 98.6, upper = 9.5 / 100 * 98.6, w=evs13$EF107),
    sum_pers(evs13, "stunde", lower = 9.5 / 100 * 98.6, upper = 12 / 100 * 98.6, w=evs13$EF107),
    sum_pers(evs13, "stunde", lower = 12 / 100 * 98.6, upper = Inf, w=evs13$EF107)
  ),
  "EVS_2018" = c(
    sum_pers(evs18, "stunde", lower = 0, upper = 8, w=evs18$EF107),
    sum_pers(evs18, "stunde", lower = 8, upper = 8.5, w=evs18$EF107),
    sum_pers(evs18, "stunde", lower = 8.5, upper = 8.84, w=evs18$EF107),
    sum_pers(evs18, "stunde", lower = 8.84, upper = 9.5, w=evs18$EF107),
    sum_pers(evs18, "stunde", lower = 9.5, upper = 12, w=evs18$EF107),
    sum_pers(evs18, "stunde", lower = 12, upper = Inf, w=evs18$EF107)
  )
)



# Anteil Haushalte in Einkommens-Kategorien 
HH <- data.frame(
  "Lohn" = c(
    "unter 8 Euro", "8 bis unter 8,50 Euro", "8,50 bis unter 8,84 Euro",
    "8,84 bis unter 9,50 Euro", "9,50 bis unter 12 Euro", "12 Euro und mehr"
  ),
  "EVS_2013" = c(
    sum_hh(evs13, "stunde", lower = 0, upper = 8 / 100 * 98.6, w=evs13$EF107),
    sum_hh(evs13, "stunde", lower = 8 / 100 * 98.6, upper = 8.5 / 100 * 98.6, w=evs13$EF107),
    sum_hh(evs13, "stunde", lower = 8.5 / 100 * 98.6, upper = 8.84 / 100 * 98.6, w=evs13$EF107),
    sum_hh(evs13, "stunde", lower = 8.84 / 100 * 98.6, upper = 9.5 / 100 * 98.6, w=evs13$EF107),
    sum_hh(evs13, "stunde", lower = 9.5 / 100 * 98.6, upper = 12 / 100 * 98.6, w=evs13$EF107),
    sum_hh(evs13, "stunde", lower = 12 / 100 * 98.6, upper = Inf, w=evs13$EF107)
  ),
  "EVS_2018" = c(
    sum_hh(evs18, "stunde", lower = 0, upper = 8, w=evs18$EF107),
    sum_hh(evs18, "stunde", lower = 8, upper = 8.5, w=evs18$EF107),
    sum_hh(evs18, "stunde", lower = 8.5, upper = 8.84, w=evs18$EF107),
    sum_hh(evs18, "stunde", lower = 8.84, upper = 9.5, w=evs18$EF107),
    sum_hh(evs18, "stunde", lower = 9.5, upper = 12, w=evs18$EF107),
    sum_hh(evs18, "stunde", lower = 12, upper = Inf, w=evs18$EF107)
  )
)

# Gruppen zu Mindestlohnhaushalten zuweisen

# 2013

#Deflationierung der Mindestlohngrenze für 2013 läuft aufs gleiche hinaus wie Deflationierung der Löhne,
#in der Tabelle können wir aber die nominalen Mindestlohngrenzen stehen haben

# Quartile der Stundenlöhne des Jahres 2013
stundenloehne_2013 <- evs13 %>%
  select(paste0("stunde_", 1:6, "_winsor"), EF107) %>%
  pivot_longer(cols=-EF107, names_to="person", values_to="stundenlohn")

#neu: gewichtet
q13 <- wtd.quantile(stundenloehne_2013$stundenlohn, stundenloehne_2013$EF107, 
                    probs=c(.1, .2, .5), na.rm=T)

evs13 <- 
  evs13 %>% 
  mutate(lohn_empfänger = rowSums(!is.na(.[paste0("stunde_", 1:6, "_winsor")])),
         unterster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), min, na.rm=T),
         hoechster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), max, na.rm=T),
         milo_hh_dyn = case_when(
           lohn_empfänger >= 1 & unterster_lohn < q13[1] ~ 
             paste0("(1) Unterster Lohn im HH im 1. Dezil"), 
           lohn_empfänger >= 1 & unterster_lohn >= q13[1] & unterster_lohn < q13[2] ~ 
             paste0("(2) Unterster Lohn im HH zwischen 1. und 2. Dezil"),
           lohn_empfänger >= 1 & unterster_lohn >= q13[2] & unterster_lohn < q13[3] ~ 
             paste0("(3) Unterster Lohn im HH zwischen 2. Dezil und Median"),
           lohn_empfänger >= 1 & unterster_lohn >= q13[3]  ~ 
             paste0("(4) Unterster Lohn im HH gleich oder oberhalb Median")
         ),
         milo_hh_max = case_when(
           lohn_empfänger >= 1 & hoechster_lohn < q13[1] ~ 
             paste0("(1) Höchster Lohn im HH im 1. Dezil"), 
           lohn_empfänger >= 1 & hoechster_lohn >= q13[1] & hoechster_lohn < q13[2] ~ 
             paste0("(2) Höchster Lohn im HH zwischen 1. und 2. Dezil"),
           lohn_empfänger >= 1 & hoechster_lohn >= q13[2] & hoechster_lohn < q13[3] ~ 
             paste0("(3) Höchster Lohn im HH zwischen 2. Dezil und Median"),
           lohn_empfänger >= 1 & hoechster_lohn >= q13[3]  ~ 
             paste0("(4) Höchster Lohn im HH gleich oder oberhalb Median")
         )
         )  

# Quartile der Stundenlöhne des Jahres 2018
stundenloehne_2018 <- evs18 %>%
  select(paste0("stunde_", 1:6, "_winsor"), EF107) %>%
  pivot_longer(cols=-EF107, names_to="person", values_to="stundenlohn")

# neu: gewichtet
q18 <- wtd.quantile(stundenloehne_2018$stundenlohn, stundenloehne_2018$EF107, 
                    probs=c(.1, .2, .5), na.rm=T)

evs18 <- 
  evs18 %>% 
  mutate(lohn_empfänger = rowSums(!is.na(.[paste0("stunde_", 1:6, "_winsor")])),
         unterster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), min, na.rm=T),
         hoechster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), max, na.rm=T),
         milo_hh_dyn = case_when(
           lohn_empfänger >= 1 & unterster_lohn < q18[1] ~ 
             paste0("(1) Unterster Lohn im HH im 1. Dezil"), 
           lohn_empfänger >= 1 & unterster_lohn >= q18[1] & unterster_lohn < q18[2] ~ 
             paste0("(2) Unterster Lohn im HH zwischen 1. und 2. Dezil"),
           lohn_empfänger >= 1 & unterster_lohn >= q18[2] & unterster_lohn < q18[3] ~ 
             paste0("(3) Unterster Lohn im HH zwischen 2. Dezil und Median"),
           lohn_empfänger >= 1 & unterster_lohn >= q18[3]  ~ 
             paste0("(4) Unterster Lohn im HH gleich oder oberhalb Median")
         ),
         milo_hh_max = case_when(
           lohn_empfänger >= 1 & hoechster_lohn < q18[1] ~ 
             paste0("(1) Höchster Lohn im HH im 1. Dezil"), 
           lohn_empfänger >= 1 & hoechster_lohn >= q18[1] & hoechster_lohn < q18[2] ~ 
             paste0("(2) Höchster Lohn im HH zwischen 1. und 2. Dezil"),
           lohn_empfänger >= 1 & hoechster_lohn >= q18[2] & hoechster_lohn < q18[3] ~ 
             paste0("(3) Höchster Lohn im HH zwischen 2. Dezil und Median"),
           lohn_empfänger >= 1 & hoechster_lohn >= q18[3]  ~ 
             paste0("(4) Höchster Lohn im HH gleich oder oberhalb Median")
         )
  )  