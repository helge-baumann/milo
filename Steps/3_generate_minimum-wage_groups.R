# Zuweisung Mindestlohnhaushalte

# Gruppen zu Mindestlohnhaushalten zuweisen

# 2013

# Quartile der Stundenlöhne des Jahres 2013
stundenloehne_2013 <- evs13 %>%
  select(paste0("stunde_", 1:6, "_winsor"), EF107) %>%
  pivot_longer(cols=-EF107, names_to="person", values_to="stundenlohn")

# neu: gewichtet (NICHT  MEHR  benötigt für statische Betrachtung)
q13 <- wtd.quantile(stundenloehne_2013$stundenlohn, stundenloehne_2013$EF107, 
                    probs=c(.1, .2, .5), na.rm=T)

# Gruppen anpassen (binär)
evs13 <- 
  evs13 %>% 
  mutate(lohn_empfänger = rowSums(!is.na(.[paste0("stunde_", 1:6, "_winsor")])),
         # untersten Lohn im Haushalt finden:
         unterster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), min, na.rm=T),
         # wenn alle NA: min gibt Inf aus. Korrigieren:
         unterster_lohn = if_else(unterster_lohn == Inf, NA_real_, unterster_lohn), 
         # Mindestlohngruppen
         milo_hh = case_when(
           lohn_empfänger >= 1 & unterster_lohn < 9.3725 ~ 
            "(1) Unterster Lohn im HH in Mindestlohngruppe", 
           lohn_empfänger >= 1 & unterster_lohn >= 9.3725  ~ 
             paste0("(2) Unterster Lohn im HH oberhalb Mindestlohn")
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
         # untersten Lohn im Haushalt finden:
         unterster_lohn = pmap_dbl(
           list(stunde_1_winsor, stunde_2_winsor, stunde_3_winsor, 
                stunde_4_winsor, stunde_5_winsor, stunde_6_winsor), min, na.rm=T),
         # wenn alle NA: min gibt Inf aus. Korrigieren:
         unterster_lohn = if_else(unterster_lohn == Inf, NA_real_, unterster_lohn), 
         # Mindestlohngruppen
         milo_hh = case_when(
           lohn_empfänger >= 1 & unterster_lohn < 9.84 ~ 
             paste0("(1) Unterster Lohn im HH in Mindestlohngruppe"), 
           lohn_empfänger >= 1 & unterster_lohn >= 9.84 ~ 
             paste0("(2) Unterster Lohn im HH oberhalb Mindestlohn")
         )
  )  

