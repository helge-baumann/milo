# Verteilung der Bruttostundenlöhne (Export to Excel)

# Separater Datensatz Stundenlöhne Longformat (2013)
bsl13 <- 
  evs13 %>%
  select(starts_with("stunde"), "EF2U2") %>%
  pivot_longer(
    cols=starts_with("stunde"), names_to="person", values_to="bsl") %>%
  filter(!is.na(bsl)) %>%
  mutate(bsl_g = cut(bsl, breaks=seq(-1, 700, 1), 
                     labels=paste(-1:699, "bis", 0:700)),
         EF2U2 = as.character(EF2U2)) %>%
  mutate(bsl = round(bsl, digits=2)) %>%
  mutate(person = str_remove(person, "stunde_")) %>%
  separate(person, into=c("person", "winsor"), sep="_", fill="right") %>%
  mutate(winsor = if_else(is.na(winsor), "ohne_winsor", "winsor")) %>%
  pivot_wider(values_from=c(bsl, bsl_g), names_from=winsor)

  
# separater Datensatz Stundenlöhne Longformat (2018)
bsl18 <- evs18 %>%
  select(starts_with("stunde"), "EF2U2") %>%
  pivot_longer(
    cols=starts_with("stunde"), names_to="person", values_to="bsl") %>%
  filter(!is.na(bsl)) %>%
  mutate(bsl_g = cut(bsl, breaks=seq(-1, 700, 1), 
                     labels=paste(-1:699, "bis", 0:700)),
         EF2U2 = as.character(EF2U2)) %>%
  mutate(bsl = round(bsl, digits=2)) %>%
  mutate(person = str_remove(person, "stunde_")) %>%
  separate(person, into=c("person", "winsor"), sep="_", fill="right") %>%
  mutate(winsor = if_else(is.na(winsor), "ohne_winsor", "winsor")) %>%
  pivot_wider(values_from=c(bsl, bsl_g), names_from=winsor)

# alle Personen (ohne Differenzierung, ob Person 1, 2, 3, ...), gruppiert
bsl13_alle <- bsl13 %>%
  pivot_longer(c(bsl_g_ohne_winsor, bsl_g_winsor)) %>%
  group_by(name) %>%
  count(value) %>%
  pivot_wider(names_from = name, values_from = n)


bsl18_alle <- bsl18 %>%
  pivot_longer(c(bsl_g_ohne_winsor, bsl_g_winsor)) %>%
  group_by(name) %>%
  count(value) %>%
  pivot_wider(names_from = name, values_from = n)

# abspeichern
wb <- createWorkbook()
sapply(
 c("2013", "2018", "2013 Euro-Schritte", "2018 Euro-Schritte"),
  function(x) addWorksheet(wb, x))
writeData(wb, "2013", bsl13 )
# Nachkommastellen
addStyle(wb, sheet = "2013", 
         style = createStyle(numFmt = "0.00"), 
         rows = 2:nrow(bsl13)+1, 
         cols = which(sapply(bsl13, class) == "numeric"), 
         gridExpand = T)
writeData(wb, "2018", bsl18 )
addStyle(wb, sheet = "2018", 
         style = createStyle(numFmt = "0.00"), 
         rows = 2:nrow(bsl18)+1, 
         cols = which(sapply(bsl18, class) == "numeric"), 
         gridExpand = T)
writeData(wb, "2013 Euro-Schritte", bsl13_alle)
writeData(wb, "2018 Euro-Schritte", bsl18_alle)
saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
             "Tab_2-2_Bruttostundenlöhne.xlsx"), 
             overwrite=T)
