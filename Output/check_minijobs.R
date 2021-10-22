check13 <- evs13
check18 <- evs18

for(i in 1:6) {

check13 <- 
  check13 %>%
  mutate(
    welle = "2013",
    "minijob_{i}" := if_else(get(paste0("EF", 7 + i, "U17")) %in% 1:2, 1, 0),
    "alter_{i}" := if_else(get(paste0("EF", 7 + i, "U3")) %in% 1948:1994, 1, 0),
    "abfindung_{i}" := if_else(get(paste0("EF", 112, "U", i)) == 0, 1, 0),
    "mutterschaftsgeld_{i}" := if_else(get(paste0("EF", 153, "U", i)) == 0, 1, 0),
    "elterngeld_{i}" := if_else(get(paste0("EF", 158, "U", i)) == 0, 1, 0),
    "arbeitnehmer_{i}" := if_else(get(paste0("EF", 7 + i, "U14")) %in% 1:2, 1, 0),
    "arbeitszeit_{i}" := if_else(get(paste0("EF", 7 + i, "U16")) > 0, 1, 0),
    "altersteilzeit_{i}" := if_else(get(paste0("EF", 7 + i, "U12")) != 2, 1, 0),
    "altersteilzeitgeld_{i}" := if_else(get(paste0("EF", 119, "U", i)) == 0, 1, 0),
    "basislohn_{i}" := if_else(get(paste0("basislohn_", i)) > 0, 1, 0)
    
  ) 

check18 <- 
  check18 %>%
  mutate(
    welle = "2018",
    "minijob_{i}" := if_else(get(paste0("EF", 7 + i, "U18")) %in% 1:2, 1, 0),
    "alter_{i}" := if_else(get(paste0("EF", 7 + i, "U3")) %in% 1953:1999, 1, 0),
    "abfindung_{i}" := if_else(get(paste0("EF", 112, "U", i)) == 0, 1, 0),
    "mutterschaftsgeld_{i}" := if_else(get(paste0("EF", 153, "U", i)) == 0, 1, 0),
    "elterngeld_{i}" := if_else(get(paste0("EF", 158, "U", i)) == 0, 1, 0),
    "arbeitnehmer_{i}" := if_else(get(paste0("EF", 7 + i, "U15")) %in% 1:2, 1, 0),
    "arbeitszeit_{i}" := if_else(get(paste0("EF", 7 + i, "U17")) > 0, 1, 0),
    "altersteilzeit_{i}" := if_else(get(paste0("EF", 7 + i, "U13")) != 2, 1, 0),
    "altersteilzeitgeld_{i}" := if_else(get(paste0("EF", 119, "U", i)) == 0, 1, 0),
    "basislohn_{i}" := if_else(get(paste0("basislohn_", i)) > 0, 1, 0)
  )
  
}  
  
check13 <- check13 %>% 
  select(starts_with(c("welle", "minijob", "alter", "abfindung", "muttersch", 
                       "eltern", "arbeit", "alters", "basis")))

check18 <- check18 %>% 
  select(starts_with(c("welle", "minijob", "alter", "abfindung", "muttersch", 
                       "eltern", "arbeit", "alters", "basis")))

check <- 
  rbind(check13, check18) %>%
  pivot_longer(cols=-welle, names_to = c('.value', 'Person'), 
               names_sep="_") %>%
  group_by(welle, minijob) %>%
  select(-Person) %>%
  summarise(
    minijobs_gesamt = n(),
    across(alter:basislohn, ~ sum(., na.rm=T))
  ) %>%
  filter(minijob == 1) %>%
    mutate(across(alter:basislohn, ~ ./minijobs_gesamt)) #%>%
    write.csv2("./Output/check_minijobs.csv")

