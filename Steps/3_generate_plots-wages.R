# Verteilung der Bruttostundenlöhne

# Individuell für Personen 1 bis 6 (1 = Haupteinkommensbezieher)
plots_bsl <- list()

for (i in 1:6) {
  
  plots_bsl[[i]] <-
    ggplot(data = evs13) +
    geom_histogram(
      aes_(x = evs13[[paste0("stunde_", i)]]), bins = 50, fill = "red") +
    geom_histogram(
      data = evs18, aes_(x = evs18[[paste0("stunde_", i)]]), 
      bins = 50, fill = "blue", alpha = 0.3) +
    aes(y = stat(count) / sum(count)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "", 
      y = "", 
      title = paste0("Person ", i), 
      subtitle = 
        paste0(" (N = ", sum(!is.na(evs13[[paste0("stunde_", i)]])), " / ", 
               sum(!is.na(evs18[[paste0("stunde_", i)]])), ")")
    ) +
    xlim(0, 100) +
    annotate("rect", xmin = 7.5, xmax = 10, ymin = 0, ymax=0.125,
             alpha = .2, fill = "black", linetype="dashed", color="black") 
  
}

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

# separater Datensatz Stundenlöhne Longformat (2018, alternativ)
bsl18_alt <- evs18 %>%
  select(starts_with("stunde")) %>%
  pivot_longer(
    cols=starts_with("stunde"), names_to="person", values_to="bsl") %>%
  mutate(bsl_g = cut(bsl, breaks=seq(-1, 700, 1), 
                     labels=paste(-1:699, "bis", 0:700))) %>%
  filter(str_detect(person, "alt")) 

# Plot Verteilung 2013
plots_bsl_13 <- 
  bsl13 %>%
  count(bsl_g) %>%
  ggplot() + geom_col(aes(x = bsl_g, y=n), fill = "red") +
  xlim(levels(bsl13$bsl_g)[6:26]) +
  coord_flip() +
  geom_text(aes(x=bsl_g, y=n-100, label=n), size=3, color="white") +
  ylim(0, 3000) +
  labs(x = "", y = "", title = "Bruttostundenlöhne 2013", 
    subtitle =  paste0(" (N = ", sum(!is.na(bsl13$bsl_g)), ")")) 

# Plot 2018
plots_bsl_18 <- 
  bsl18 %>% 
  count(bsl_g) %>%
  ggplot() + geom_col(aes(x = bsl_g, y=n), fill = "blue") +
  xlim(levels(bsl18$bsl_g)[6:26]) +
  coord_flip() +
  geom_text(aes(x=bsl_g, y=n-100, label=n), size=3, color="white") +
  ylim(0, 3000) +
  labs(x = "",  y = "", title = "Bruttostundenlöhne 2018", 
    subtitle =  paste0(" (N = ", sum(!is.na(bsl18$bsl_g)), ")")) 

# Plot 2018 (alternativ)
plots_bsl_18_alt <- 
  bsl18_alt %>%
  count(bsl_g) %>%
  ggplot() + geom_col(aes(x = bsl_g, y=n), fill = "blue") +
  xlim(levels(bsl18$bsl_g)[6:26]) +
  coord_flip() +
  geom_text(aes(x=bsl_g, y=n-100, label=n), size=3, color="white") +
  ylim(0, 3000) +
  labs(x = "", y = "", 
    title = "Bruttostundenlöhne 2018 (alternative Berechnung)", 
    subtitle =  paste0(" (N = ", sum(!is.na(bsl18$bsl_g)), ")")) 


 
