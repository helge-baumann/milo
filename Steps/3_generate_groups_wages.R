# Verteilung der Bruttostundenlöhne (Export to Excel)

# separater Datensatz Stundenlöhne Longformat (2013)
bsl13 <- evs13 %>%
  select(starts_with("stunde")) %>%
  pivot_longer(
    cols=starts_with("stunde"), names_to="person", values_to="bsl") %>%
  mutate(bsl_g = cut(bsl, breaks=seq(-1, 700, 1), 
                     labels=paste(-1:699, "bis", 0:700))) 
  
# separater Datensatz Stundenlöhne Longformat (2018)
bsl18 <- evs18 %>%
  select(starts_with("stunde")) %>%
  pivot_longer(
    cols=starts_with("stunde"), names_to="person", values_to="bsl") %>%
  mutate(bsl_g = cut(bsl, breaks=seq(-1, 700, 1), 
                     labels=paste(-1:699, "bis", 0:700))) %>%
  filter(!str_detect(person, "alt")) 

# gruppiert
bsl13_g <- bsl13 %>%
  group_by(person, bsl_g) %>%
  count()

bsl18_g <- bsl18 %>%
  group_by(person, bsl_g) %>%
  count()

bsl13_alle <- bsl13 %>%
  mutate(winsor=if_else(str_detect(person, "winsor"), "mit winsor", "ohne winsor")) %>%
  group_by(bsl_g, winsor) %>%
  count() %>%
  pivot_wider(names_from=winsor, values_from=-c(bsl_g, winsor))

bsl18_alle <- bsl18 %>%
  mutate(winsor=if_else(str_detect(person, "winsor"), "mit winsor", "ohne winsor")) %>%
  group_by(bsl_g, winsor) %>%
  count() %>%
  pivot_wider(names_from=winsor, values_from=-c(bsl_g, winsor))

# abspeichern
wb <- createWorkbook()
addWorksheet(wb, "2013")
addWorksheet(wb, "2018")
addWorksheet(wb, "2013 Euro-Schritte")
addWorksheet(wb, "2018 Euro-Schritte")
writeData(wb, "2013", bsl13 %>% filter(!is.na(bsl)))
writeData(wb, "2018", bsl18 %>% filter(!is.na(bsl)))
writeData(wb, "2013 Euro-Schritte", bsl13_alle)
writeData(wb, "2018 Euro-Schritte", bsl18_alle)
saveWorkbook(wb, "./Output/Tabellen für Berichte/Bruttostundenlöhne.xlsx", overwrite=T)
