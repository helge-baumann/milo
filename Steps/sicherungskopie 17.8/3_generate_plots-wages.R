# Verteilung der Bruttostundenlöhne (Export to Excel)

# Plots Bruttostundenlöhne über alle Personen hinweg

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
  group_by(bsl_g) %>%
  count()

bsl18_alle <- bsl18 %>%
  group_by(bsl_g) %>%
  count()
