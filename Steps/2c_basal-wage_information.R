# Welche Typen haben Grundlohninformationen?

write.csv2(
  rbind(
evs13 %>%
  select(
    paste0("EF", 7+1:6, "U", 17), EF2U2,
  ) %>%
  rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
  mutate_at(LETTERS[1:6], as_factor) %>%
  pivot_longer(cols=-EF2U2, values_to="Minijob") %>%
  left_join(
    evs13 %>%
      select(
                paste0("EF", 109, "U", 1:6), EF2U2
              ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
              pivot_longer(-EF2U2, values_to="Grundlohn")) %>%
  left_join(
    evs13 %>%
      select(
        paste0("EF", 118, "U", 1:6), EF2U2
      ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
      pivot_longer(-EF2U2, values_to="Nebenverdienst")) %>%
  left_join(
    evs13 %>%
      select(
        paste0("EF", 7+1:6, "U", 8), EF2U2
      ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
      pivot_longer(-EF2U2, values_to="Beamter")) %>%
  select(-starts_with("name")) %>%
  mutate(Beamter = case_when(Beamter == 4 ~ 1, Beamter != 4 ~ 0)) %>%
  group_by(Minijob, Beamter) %>%
  summarise(
    `Grundlohnangabe gesamt` = sum(Grundlohn > 0),
    `Nebenverdienstangabe gesamt` = sum(Nebenverdienst > 0),
    `Nur Grundlohnangabe` = sum(Grundlohn > 0 & Nebenverdienst == 0),
    `Nur Nebenverdienstangabe` = sum(Grundlohn == 0 & Nebenverdienst > 0),
    Gesamt = sum(!is.na(Minijob))
  ),
  
# Welche Typen haben Grundlohninformationen?

evs18 %>%
  select(
    paste0("EF", 7+1:6, "U", 18), EF2U2,
  ) %>%
  rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
  mutate_at(LETTERS[1:6], as_factor) %>%
  pivot_longer(cols=-EF2U2, values_to="Minijob") %>%
  left_join(
    evs18 %>%
      select(
        paste0("EF", 109, "U", 1:6), EF2U2
      ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
      pivot_longer(-EF2U2, values_to="Grundlohn")) %>%
  left_join(
    evs18 %>%
      select(
        paste0("EF", 118, "U", 1:6), EF2U2
      ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
      pivot_longer(-EF2U2, values_to="Nebenverdienst")) %>%
  left_join(
    evs18 %>%
      select(
        paste0("EF", 7+1:6, "U", 9), EF2U2
      ) %>%
      rename(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6) %>%
      pivot_longer(-EF2U2, values_to="Beamter")) %>%
  select(-starts_with("name")) %>%
  mutate(Beamter = case_when(Beamter == 5 ~ 1, Beamter != 5 ~ 0)) %>%
  group_by(Minijob, Beamter) %>%
  summarise(
    `Grundlohnangabe gesamt` = sum(Grundlohn > 0),
    `Nebenverdienstangabe gesamt` = sum(Nebenverdienst > 0),
    `Nur Grundlohnangabe` = sum(Grundlohn > 0 & Nebenverdienst == 0),
    `Nur Nebenverdienstangabe` = sum(Grundlohn == 0 & Nebenverdienst > 0),
    Gesamt = sum(!is.na(Minijob))
  ) 
) %>%
  ungroup() %>%
  mutate(Jahr = rep(c(2013, 2018), each=9))  %>%
  select (Jahr, everything()), 
paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
       "Tab_2-3_Grundlohninformationen.csv"),
row.names=F
)
  
