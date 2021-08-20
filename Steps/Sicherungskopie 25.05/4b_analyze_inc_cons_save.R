# Haushaltsnettoeinkommen

# Konsum+Sparen = Einkommen?

evs13 <- evs13 %>% mutate(consum=EF89/EF62, save=EF105/EF62, gesamt=consum+save)
evs18 <- evs18 %>% mutate(consum=EF89/EF62, save=EF105/EF62, gesamt=consum+save)

ggplot(data = evs13 %>% filter(gesamt > 0 & gesamt <=2)) +
  geom_histogram(aes(x = gesamt), bins = 50, fill = "red") +
  geom_histogram(data = evs18 %>% filter(gesamt > 0 & gesamt <=2), aes(x = gesamt), bins = 50, fill = "blue", alpha = 0.3) +
  aes(y = stat(count) / sum(count)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(x="Konsumquote + Sparquote", y="relative Häufigkeit")

# Namen für Tabellen

HH_income <- evs13 %>% 
  group_by(milo_hh) %>%
  
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  summarise(`2013` = round(wtd.mean(EF62/3, EF107), digits=1))

HH_income$`2018` <- evs18 %>% 
  
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  summarise(`2018` = round(wtd.mean(EF62/3, EF107), digits=1)) %>%
  pull(`2018`)

HH_income$milo_hh <- sort(unique(evs13$milo_hh)) # starke Lohnzuwächse im Milo-Bereich

# Konsum

HH_konsum <- evs13 %>% 
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(consum_share = EF89/EF62*100) %>% 
  summarise(`2013 absolut` = round(wtd.mean(EF89/3, EF107), digits=1), 
            `2013 Konsumquote` = round(wtd.mean(consum_share, EF107)))

HH_konsum$`2018 absolut` <- evs18 %>% 
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(consum_share = EF89/EF62*100) %>% 
  summarise(`2018 absolut` = round(wtd.mean(EF89/3, EF107), digits=1), 
            `2018 Konsumquote` = round(wtd.mean(consum_share, EF107))) %>%
  pull(`2018 absolut`)


HH_konsum$`2018 Konsumquote` <- evs18 %>% 
    group_by(milo_hh) %>% 
    filter(
      EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
        EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
    mutate(consum_share = EF89/EF62*100) %>% 
    summarise(`2018 absolut` = round(wtd.mean(EF89/3, EF107), digits=1), 
              `2018 Konsumquote` = round(wtd.mean(consum_share, EF107))) %>%
  pull(`2018 Konsumquote`)
  
HH_konsum$milo_hh <- sort(unique(evs13$milo_hh))

# Sparen

HH_save <- evs13 %>% 
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  summarise(`2013 absolut` = round(wtd.mean(EF105/3, EF107), digits=1), 
            `2013 Sparquote` = round(wtd.mean(save_share, EF107)))

HH_save$`2018 absolut` <- evs18 %>% 
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  summarise(`2018 absolut` = round(wtd.mean(EF105/3, EF107), digits=1), 
            `2018 Sparquote` = round(wtd.mean(save_share, EF107))) %>%
  pull(`2018 absolut`)


HH_save$`2018 Sparquote` <- evs18 %>% 
  group_by(milo_hh) %>% 
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  summarise(`2018 absolut` = round(wtd.mean(EF105/3, EF107), digits=1), 
            `2018 Sparquote` = round(wtd.mean(save_share, EF107))) %>%
  pull(`2018 Sparquote`)

HH_konsum$milo_hh <- sort(unique(evs13$milo_hh))

# Einkommenszuwächse Bundesland
HH_land <- evs13 %>%
  
  #filter(milo_hh == "(1) Mindestens ein Lohnempfänger im HH unter 8.84 Euro") %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  dezil_g = case_when(dezile %in% paste0("d", 1:2) ~ "untere beide Dezile",
                      dezile %in% paste0("d", 3:10) ~ "obere acht Dezile")) %>%
  group_by(as_factor(EF2U1), dezil_g) %>%
  summarise(`2013` = round(wtd.mean(EF62/3, EF107), digits=1)) %>%
  filter(!is.na(dezil_g))

HH_land$`2018` <- evs18 %>%
  #group_by(as_factor(EF2U1)) %>%
  #filter(milo_hh == "(1) Mindestens ein Lohnempfänger im HH unter 8.84 Euro") %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  dezil_g = case_when(dezile %in% paste0("d", 1:2) ~ "untere beide Dezile",
                      dezile %in% paste0("d", 3:10) ~ "obere acht Dezile")) %>%
  group_by(as_factor(EF2U1), dezil_g) %>%
  summarise(`2018` = round(wtd.mean(EF62/3, EF107), digits=1)) %>%
  filter(!is.na(dezil_g)) %>%
  pull(`2018`)

names(HH_land)[1] <- "Bundesland"

bite <- read.csv2("./Daten/bite.csv", encoding="UTF-8")
names(bite)[1] <- "Bundesland"

HH_land <- HH_land %>%
  left_join(bite) %>%
  mutate(Zuwachs = `2018` - `2013`, Zuwachs_proz = round((`2018` - `2013`)/`2018`*100, digits=1))

ggplot(HH_land %>% filter(dezil_g == "untere beide Dezile"), aes(x=VSE.bite, y=Zuwachs_proz)) + geom_point() + geom_smooth() # kein Zusammenhang

# Konsumquoten nach Dezilen, 2013 und 2018
Dezile <- 
  evs13 %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  consum_share = EF89/EF62*100) %>%
  group_by(dezile) %>%
  summarise(income=wtd.mean(EF62/3, EF107), consum = wtd.mean(EF89/3, EF107), share=wtd.mean(consum_share, EF107))

Dezile <- evs18 %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  consum_share = EF89/EF62*100) %>%
  group_by(dezile) %>%
  summarise(income=wtd.mean(EF62/3, EF107), consum = wtd.mean(EF89/3, EF107), share=wtd.mean(consum_share, EF107)) %>%
  left_join(Dezile, by="dezile") 

# Konsumgüter nach Dezilen
konsumgüter18 <- evs18 %>%
  select(EF107, EF73:EF82, EF84:EF88, milo_hh, EF62) %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  dezil_g = case_when(dezile %in% paste0("d", 1:2) ~ "untere beide Dezile",
                      dezile %in% paste0("d", 3:10) ~ "obere acht Dezile")) %>%
  pivot_longer(cols=c(EF73:EF82, EF84:EF88), names_to="gut", values_to="wert") %>%
  group_by(gut, dezil_g) %>%
  summarise(quote = wtd.mean(wert/EF62, EF107))

konsumgüter <- evs13 %>%
  select(EF107, EF73:EF82, EF84:EF88, milo_hh, EF62) %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(dezile = cut(
    EF62, breaks=wtd.quantile(EF62, EF107, probs=seq(0,1,0.1)),
    labels=paste0("d", 1:10)
  ),
  dezil_g = case_when(dezile %in% paste0("d", 1:2) ~ "untere beide Dezile",
                      dezile %in% paste0("d", 3:10) ~ "obere acht Dezile")) %>%
  pivot_longer(cols=c(EF73:EF82, EF84:EF88), names_to="gut", values_to="wert") %>%
  group_by(gut, dezil_g) %>%
  summarise(quote = wtd.mean(wert/EF62, EF107)) %>%
  left_join(konsumgüter18, by=c("gut", "dezil_g")) %>%
  filter(!is.na(dezil_g))

for(i in 1:nrow(konsumgüter)) {
  
  konsumgüter$gut[i] <- attributes(evs13[[konsumgüter$gut[i]]])$label
  
}

# Konsumgüter nach Haushaltsgröße
konsumgüter18_hh <- evs18 %>%
  select(EF107, EF73:EF82, EF84:EF88, milo_hh, EF62, EF35) %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(hhs = as_factor(EF35)) %>%
  pivot_longer(cols=c(EF73:EF82, EF84:EF88), names_to="gut", values_to="wert") %>%
  group_by(gut, hhs) %>%
  summarise(quote = wtd.mean(wert/EF62, EF107))

konsumgüter_hh <- evs13 %>%
  select(EF107, EF73:EF82, EF84:EF88, milo_hh, EF62, EF35) %>%
  filter(
    EF62 >= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[2] &
      EF62 <= wtd.quantile(EF62, EF107, probs=seq(0,1,0.01))[100]) %>%
  mutate(hhs = as_factor(EF35)) %>%
  pivot_longer(cols=c(EF73:EF82, EF84:EF88), names_to="gut", values_to="wert") %>%
  group_by(gut, hhs) %>%
  summarise(quote = wtd.mean(wert/EF62, EF107))

for(i in 1:nrow(konsumgüter18_hh)) {
  
  konsumgüter18_hh$gut[i] <- attributes(evs18[[konsumgüter18_hh$gut[i]]])$label
  
}
