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
namen_milo <- c(paste0(sort(unique(evs13$milo_hh))[1], " (2018: ", lower_milo_2018, ")"),
                paste0(sort(unique(evs13$milo_hh))[2], " (2018: ", lower_milo_2018, " und ", upper_milo_2018, ")"),
                paste0(sort(unique(evs13$milo_hh))[3], " (2018: ", upper_milo_2018, ")"),
                "HH nicht anspruchsberechtigt")

HH_income <- evs13 %>% group_by(milo_hh) %>% summarise(`2013` = round(mean(EF62), digits=1))

HH_income$`2018` <- evs18 %>% group_by(milo_hh) %>% summarise(`2018` = round(mean(EF62), digits=1)) %>% pull(`2018`)

HH_income$milo_hh <- namen_milo
# Konsum

HH_konsum <- evs13 %>% 
  filter(EF62 > 0) %>%
  mutate(consum_share = EF89/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2013 absolut` = round(mean(EF89), digits=1), `2013 Konsumquote` = round(mean(consum_share)))

HH_konsum$`2018 absolut` <- evs18 %>% 
  filter(EF62 > 0) %>%
  mutate(consum_share = EF89/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2018 absolut` = round(mean(EF89), digits=1), `2018 Konsumquote` = round(mean(consum_share))) %>%
  pull(`2018 absolut`)

HH_konsum$`2018 Konsumquote` <- evs18 %>% 
  filter(EF62 > 0) %>%
  mutate(consum_share = EF89/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2018 absolut` = round(mean(EF89), digits=1), `2018 Konsumquote` = round(mean(consum_share))) %>%
  pull(`2018 Konsumquote`)
HH_konsum$milo_hh <- namen_milo

# Sparen

HH_sparen <- evs13 %>% 
  filter(EF62 > 0) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2013 absolut` = round(mean(EF105), digits=1), `2013 Sparquote` = round(mean(save_share)))

HH_sparen$`2018 absolut` <- evs18 %>% 
  filter(EF62 > 0) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2018 absolut` = round(mean(EF105), digits=1), `2018 Sparquote` = round(mean(save_share))) %>%
  pull(`2018 absolut`)

HH_sparen$`2018 Sparquote` <- evs18 %>% 
  filter(EF62 > 0) %>%
  mutate(save_share = EF105/EF62*100) %>% 
  group_by(milo_hh) %>% 
  summarise(`2018 absolut` = round(mean(EF105), digits=1), `2018 Sparquote` = round(mean(save_share))) %>%
  pull(`2018 Sparquote`)

HH_sparen$milo_hh <- namen_milo
