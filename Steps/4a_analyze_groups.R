# Analyse Verteilung von Löhnen, Haushalte und Personen
# Zuweisung Mindestlohnhaushalte

# Verteilung der Angaben zu Monatslöhnen
table(is.na(evs18$stunde_1), evs18$M0011110_01) # realistisch; 0 = kein Grundlohn?
summary(evs18$stunde_1[evs18$M0011110_01 == 0])
summary(evs18$stunde_1[evs18$M0011110_01 == 1])
summary(evs18$stunde_1[evs18$M0011110_01 == 2])
summary(evs18$stunde_1[evs18$M0011110_01 == 3])

# Anteil Personen in Einkommens-Kategorien
P <- data.frame(
  "EVS_2013" = c(
    sum_pers(evs13, "stunde", lower = 0, upper = 8), # sum_pers: Siehe functions
    sum_pers(evs13, "stunde", lower = 8, upper = 8.5),
    sum_pers(evs13, "stunde", lower = 8.5, upper = 8.84),
    sum_pers(evs13, "stunde", lower = 8.84, upper = 9.5),
    sum_pers(evs13, "stunde", lower = 9.5, upper = 12),
    sum_pers(evs13, "stunde", lower = 12, upper = Inf)
  ),
  "EVS_2018" = c(
    sum_pers(evs18, "stunde", lower = 0, upper = 8),
    sum_pers(evs18, "stunde", lower = 8, upper = 8.5),
    sum_pers(evs18, "stunde", lower = 8.5, upper = 8.84),
    sum_pers(evs18, "stunde", lower = 8.84, upper = 9.5),
    sum_pers(evs18, "stunde", lower = 9.5, upper = 12),
    sum_pers(evs18, "stunde", lower = 12, upper = Inf)
  )
)

row.names(P) <- c(
  "unter 8 Euro", "8 bis unter 8,50 Euro", "8,50 bis unter 8,84 Euro",
  "8,84 bis unter 9,50 Euro", "9,50 bis unter 12 Euro", "12 Euro und mehr"
)

# Anteil Haushalte in Einkommens-Kategorien 
HH <- data.frame(
  "EVS_2013" = c(
    sum_hh(evs13, "stunde", lower = 0, upper = 8),
    sum_hh(evs13, "stunde", lower = 8, upper = 8.5),
    sum_hh(evs13, "stunde", lower = 8.5, upper = 8.84),
    sum_hh(evs13, "stunde", lower = 8.84, upper = 9.5),
    sum_hh(evs13, "stunde", lower = 9.5, upper = 12),
    sum_hh(evs13, "stunde", lower = 12, upper = Inf)
  ),
  "EVS_2018" = c(
    sum_hh(evs18, "stunde", lower = 0, upper = 8),
    sum_hh(evs18, "stunde", lower = 8, upper = 8.5),
    sum_hh(evs18, "stunde", lower = 8.5, upper = 8.84),
    sum_hh(evs18, "stunde", lower = 8.84, upper = 9.5),
    sum_hh(evs18, "stunde", lower = 9.5, upper = 12),
    sum_hh(evs18, "stunde", lower = 12, upper = Inf)
  )
)

row.names(HH) <- c(
  "unter 8 Euro", "8 bis unter 8,50 Euro", "8,50 bis unter 8,84 Euro",
  "8,84 bis unter 9,50 Euro", "9,50 bis unter 12 Euro", "12 Euro und mehr"
)

# Gruppen zu Mindestlohnhaushalten zuweisen

# 2013
lower_milo_2013 <- 8.5*0.985 # untere Grenze
upper_milo_2013 <- 8.5*0.985+1.5 # mittlere Grenze

evs13 <- evs13 %>% group_by(EF2U2) %>% 
  mutate(lohn_empfänger = 
           sum(!is.na(stunde_1), !is.na(stunde_2), !is.na(stunde_3), 
               !is.na(stunde_4), !is.na(stunde_5), !is.na(stunde_6)),
         unter_milo_empfänger = 
           sum(stunde_1 <= lower_milo_2013, stunde_2 <= lower_milo_2013, 
               stunde_3 <= lower_milo_2013, stunde_4 <= lower_milo_2013, 
               stunde_5 <= lower_milo_2013, stunde_6 <= lower_milo_2013, na.rm=T),
         between_milo_empfänger = 
           sum(stunde_1 > lower_milo_2013 & stunde_1 <= upper_milo_2013,
               stunde_2 > lower_milo_2013 & stunde_2 <= upper_milo_2013,
               stunde_3 > lower_milo_2013 & stunde_3 <= upper_milo_2013, 
               stunde_4 > lower_milo_2013 & stunde_4 <= upper_milo_2013, 
               stunde_5 > lower_milo_2013 & stunde_5 <= upper_milo_2013, 
               stunde_6 > lower_milo_2013 & stunde_6 <= upper_milo_2013, na.rm=T),
         milo_hh = case_when(
           lohn_empfänger >= 1 & unter_milo_empfänger > 0 ~ 
             paste0("(1) Mindestens ein Lohnempfänger im HH unter ", 
                    round(lower_milo_2013, digits=2), " Euro"),
           lohn_empfänger >= 1 & unter_milo_empfänger == 0 & 
             between_milo_empfänger >= 1 ~ 
             paste0("(2) Kein Lohnempfänger im HH unter ", 
                    round(lower_milo_2013, digits=2), ", aber mind. einer unter ", 
                    round(upper_milo_2013, digits=2), " Euro"),
           lohn_empfänger >= 1 & unter_milo_empfänger+between_milo_empfänger == 0 ~ 
             paste0("(3) Alle Lohnempfänger im HH über ", 
                    round(upper_milo_2013, digits=2), " Euro"),
           lohn_empfänger == 0 ~ "(8) HH nicht anspruchsberechtigt"
         )) %>%
  ungroup()

# 2018
lower_milo_2018 <- 8.84 # untere Grenze
upper_milo_2018 <- 8.84+1.5 # mittlere Grenze

evs18 <- evs18 %>% group_by(EF2U2) %>% 
  mutate(lohn_empfänger = 
           sum(!is.na(stunde_1), !is.na(stunde_2), !is.na(stunde_3), 
               !is.na(stunde_4), !is.na(stunde_5), !is.na(stunde_6)),
         unter_milo_empfänger = 
           sum(stunde_1 <= lower_milo_2018, stunde_2 <= lower_milo_2018, 
               stunde_3 <= lower_milo_2018, stunde_4 <= lower_milo_2018, 
               stunde_5 <= lower_milo_2018, stunde_6 <= lower_milo_2018, na.rm=T),
         between_milo_empfänger = 
           sum(stunde_1 > lower_milo_2018 & stunde_1 <= upper_milo_2018,
               stunde_2 > lower_milo_2018 & stunde_2 <= upper_milo_2018,
               stunde_3 > lower_milo_2018 & stunde_3 <= upper_milo_2018, 
               stunde_4 > lower_milo_2018 & stunde_4 <= upper_milo_2018, 
               stunde_5 > lower_milo_2018 & stunde_5 <= upper_milo_2018, 
               stunde_6 > lower_milo_2018 & stunde_6 <= upper_milo_2018, na.rm=T),
         milo_hh = case_when(
           lohn_empfänger >= 1 & unter_milo_empfänger > 0 ~ 
             paste0("(1) Mindestens ein Lohnempfänger im HH unter ", 
                    round(lower_milo_2018, digits=2), " Euro"),
           lohn_empfänger >= 1 & unter_milo_empfänger == 0 & 
             between_milo_empfänger >= 1 ~ 
             paste0("(2) Kein Lohnempfänger im HH unter ", 
                    round(lower_milo_2018, digits=2), ", aber mind. einer unter ", 
                    round(upper_milo_2018, digits=2), " Euro"),
           lohn_empfänger >= 1 & 
             unter_milo_empfänger+between_milo_empfänger == 0 ~ 
             paste0("(3) Alle Lohnempfänger im HH über ", 
                    round(upper_milo_2018, digits=2), " Euro"),
           lohn_empfänger == 0 ~ "(8) HH nicht anspruchsberechtigt"
           
         )) %>%
  ungroup()

  
             
         