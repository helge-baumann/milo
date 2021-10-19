# Analyse der Haushaltsnettoeinkommen, Konsumausgaben, Sparausgaben

# Tabelle 6.1: 
  # Deskription von Einkommen im Mindestlohnbereich und darüber, Mittelwerte----
  
# Basistabelle  
Tab_6_1 <-  
  EVS_defl %>%
  group_by(welle, milo_hh) %>%
  summarise(
    `(1) Bruttoeinkommen` = 
      round(wtd.mean(brutto_winsor/3, gewicht), digits=2),
    `(2) Nettoeinkommen` =
      round(wtd.mean(netto_oecd_winsor*oecd_weight/3, gewicht), digits=2),
    `(3) Bruttoäquivalenzeinkommen` = 
      round(wtd.mean(brutto_winsor/oecd_weight/3, gewicht), digits=2),
    `(4) Nettoäquivalenzeinkommen` =
      round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=2),
    `(5) Lohnsumme gesamt` = 
      round(wtd.mean(lohn_summe_winsor/3, gewicht), digits=2),
    `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen` = 
      round(wtd.mean(lohn_anteil_winsor, gewicht), digits=1)
  ) %>%
    filter(!is.na(milo_hh)) %>%
  pivot_longer(-c(welle, milo_hh)) %>%
    pivot_wider(names_from=milo_hh) %>% 
  arrange(name) %>% 
  ungroup()
  
# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_1)/2)) {
  
    Tab_6_1 <- Tab_6_1 %>% add_row(welle = "2018/2013", name = Tab_6_1$name[i*2])
  
    for(j in 3:ncol(Tab_6_1)) {
      
    Tab_6_1[nrow(Tab_6_1),j] <- 
      as.numeric((Tab_6_1[i*2,j]-Tab_6_1[(i*2)-1,j])/Tab_6_1[i*2,j]*100)
                     
  }

}

# neu sortieren
Tab_6_1 <- Tab_6_1 %>% arrange(name)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.1")

writeData(wb, "Tab 6.1", Tab_6_1)

writeData(wb, "Tab 6.1", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018 - Wert 2013)/Wert 2018*100"), 
          startCol=2, startRow=nrow(Tab_6_1)+3, colNames=F)
addStyle(wb, "Tab 6.1", style_anmerkung, nrow(Tab_6_1)+3, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-1_Deskription_Einkommen.xlsx"), 
             overwrite=T)

# Tabellen 6.2 bis 6.5: Auswertung nach Kriterien----
Tab_6_2bis5 <- 
  EVS_defl %>%
  mutate(
    Haushaltstypen = as_factor(hhtype),#
    `Anzahl Erwerbstätige im Haushalt` = ET_hh) %>%
  pivot_longer(cols=c(Haushaltstypen, `Anzahl Erwerbstätige im Haushalt`), 
               names_to="Variable", values_to="Wert") %>%
  group_by(welle, milo_hh, Variable, Wert) %>%
   summarise(
     `(1) Bruttoeinkommen` = 
       round(wtd.mean(brutto_winsor/3, gewicht), digits=2),
     `(2) Nettoeinkommen` =
       round(wtd.mean(netto_oecd_winsor*oecd_weight/3, gewicht), digits=2),
     `(3) Bruttoäquivalenzeinkommen` = 
       round(wtd.mean(brutto_winsor/oecd_weight/3, gewicht), digits=2),
     `(4) Nettoäquivalenzeinkommen` =
       round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=2),
     `(5) Lohnsumme gesamt` = 
       round(wtd.mean(lohn_summe_winsor/3, gewicht), digits=2),
     `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen` = 
       round(wtd.mean(lohn_anteil_winsor, gewicht), digits=1)
   ) %>%
  filter(!is.na(milo_hh)) %>%
  pivot_longer(-c(welle, milo_hh, Variable, Wert)) %>%
  arrange(milo_hh, Variable, Wert, name, welle) %>%
  ungroup()
  
# Verhältnis 2018/2013
  for(i in 1:(nrow(Tab_6_2bis5)/2)) { # für jede zweite Zeile 
    
    Tab_6_2bis5 <- 
      Tab_6_2bis5 %>% 
      add_row(
        welle = "2018/2013", 
        milo_hh = Tab_6_2bis5$milo_hh[i*2],
        name = Tab_6_2bis5$name[i*2],
        Variable = Tab_6_2bis5$Variable[i*2],
        Wert = Tab_6_2bis5$Wert[i*2]
        )
      
      Tab_6_2bis5[nrow(Tab_6_2bis5), "value"] <- 
        as.numeric((Tab_6_2bis5[i*2, "value"]-Tab_6_2bis5[(i*2)-1,"value"])/
                     Tab_6_2bis5[i*2, "value"]*100)
      
}
  
# Tabelle splitten
Tab_6_2 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe" &
      Variable == "Haushaltstypen") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = Wert) %>%
  arrange(name, welle)

Tab_6_3 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe" &
      Variable == "Anzahl Erwerbstätige im Haushalt") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = Wert) %>%
  arrange(name, welle)

Tab_6_4 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn" &
      Variable == "Haushaltstypen") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = Wert) %>%
  arrange(name, welle)

Tab_6_5 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn" &
      Variable == "Anzahl Erwerbstätige im Haushalt") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = Wert) %>%
  arrange(name, welle)

rm(Tab_6_2bis5)

# schreiben
wb <- createWorkbook()
addWorksheet(wb, "Tab 6.2")

writeData(wb, "Tab 6.2", Tab_6_2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-2_Haushaltstypen_Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.3")

writeData(wb, "Tab 6.3", Tab_6_3)

writeData(wb, "Tab 6.3", 
          paste0("Achtung: Konstellation Erwerbstätige = 0   ",
                 "in betrachteter Gruppe  per definitionem nicht möglich"), 
          startCol=2, startRow=nrow(Tab_6_3)+3, colNames=F)
addStyle(wb, "Tab 6.3", style_anmerkung, nrow(Tab_6_3)+3, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-3_Erwerbstätige_Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.4")

writeData(wb, "Tab 6.4", Tab_6_4)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-4_Haushaltstypen_ueber-Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.5")

writeData(wb, "Tab 6.5", Tab_6_5)

writeData(wb, "Tab 6.5", 
          paste0("Achtung: Konstellation Erwerbstätige = 0   ",
                 "in betrachteter Gruppe  per definitionem nicht möglich"), 
          startCol=2, startRow=nrow(Tab_6_5)+3, colNames=F)
addStyle(wb, "Tab 6.5", style_anmerkung, nrow(Tab_6_5)+3, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-5_Erwerbstätige_ueber-Mindestlohnbereich.xlsx"), 
             overwrite=T)

# Tabelle 6.6: 
  # Deskription des Konsums (imputiert) und Sparens im Mindestlohn-Bereich 
  # und darüber----

Tab_6_6 <- 
EVS_defl %>%
  group_by(welle, milo_hh) %>%
  summarise(
    `(1) Konsum imputiert` = 
      round(wtd.mean(c_imp_winsor/3, gewicht), digits=2),
    `(2) Konsumquote mit Konsum imputiert` =
      round(wtd.mean((c_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100, gewicht), digits=2),
    `(3) Konsum imputiert äquivalenzgewichtet` = 
      round(wtd.mean(c_imp_winsor/oecd_weight/3, gewicht), digits=2),
    `(4) Sparen imputiert` = 
      round(wtd.mean(save_imp_winsor/3, gewicht), digits=2),
    `(5) Sparquote mit Ersparnis imputiert` =
      round(wtd.mean((save_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100, gewicht), digits=2),
    `(6) Sparen imputiert äquivalenzgewichtet` = 
      round(wtd.mean(save_imp_winsor/oecd_weight/3, gewicht), digits=2),
  ) %>%
  filter(!is.na(milo_hh)) %>%
  pivot_longer(-c(welle, milo_hh)) %>%
  pivot_wider(names_from=milo_hh) %>% 
  arrange(name) %>% 
  ungroup()

# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_6)/2)) {
  
  Tab_6_6 <- Tab_6_6 %>% add_row(welle = "2018/2013", name = Tab_6_6$name[i*2])
  
  for(j in 3:ncol(Tab_6_6)) {
    
    Tab_6_6[nrow(Tab_6_6),j] <- 
      as.numeric((Tab_6_6[i*2,j]-Tab_6_6[(i*2)-1,j])/Tab_6_6[i*2,j]*100)
    
  }
  
}

# neu sortieren
Tab_6_6 <- Tab_6_6 %>% arrange(name)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.6")

writeData(wb, "Tab 6.6", Tab_6_6)

writeData(wb, "Tab 6.6", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018 - Wert 2013)/Wert 2018*100"), 
          startCol=2, startRow=nrow(Tab_6_6)+3, colNames=F)
addStyle(wb, "Tab 6.6", style_anmerkung, nrow(Tab_6_6)+3, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-6_Deskription_Konsum_Sparen.xlsx"), 
             overwrite=T)
   
# Tabelle 6.7: Konsum nach Gütergruppen im Mindestlohn-Bereich und darüber

Tab_6_7 <- 
  EVS_defl %>%
  group_by(welle, milo_hh) %>%
  summarise(
    across(
      starts_with(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                    paste0("konsum", c(5,7,9), "new_winsor"))), 
      .fns = list( 
        ~round(wtd.mean(./3/oecd_weight, gewicht), digits=1),
        ~round(wtd.mean((./3/oecd_weight)/(c_imp_winsor/3/oecd_weight)*100, gewicht), digits=1))#,
      #.names = "{.col}_absolut"
      )
    ) %>%
    rename_at(
      vars(ends_with("_1")), 
              ~str_replace(., ., 
                           sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                                    paste0("konsum", c(5,7,9), "new_winsor")), 
                                  function(x) paste0(attributes(EVS[[x]])$label, "_absolut"))
                            )) %>%
  rename_at(
    vars(ends_with("_2")), 
    ~str_replace(., ., 
                 sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                          paste0("konsum", c(5,7,9), "new_winsor")), 
                        function(x) paste0(attributes(EVS[[x]])$label, "_anteil"))
    )) %>%
  pivot_longer(-c(welle, milo_hh)) %>%
  filter(!is.na(milo_hh)) %>%
  separate(name, sep="_", into=c("name", "Anteil_am_Konsum")) %>%
  pivot_wider(names_from=c(milo_hh, Anteil_am_Konsum), values_from=value) %>%
  ungroup()
  

# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_7)/2)) {
  
  Tab_6_7 <- Tab_6_7 %>% add_row(welle = "2018/2013", name = Tab_6_7$name[i*2])
  
  for(j in 3:ncol(Tab_6_7)) {
    
    Tab_6_7[nrow(Tab_6_7),j] <- 
      as.numeric((Tab_6_7[i*2,j]-Tab_6_7[(i*2)-1,j])/Tab_6_7[i*2,j]*100)
    
  }
  
}

# neu sortieren
Tab_6_7 <- Tab_6_7 %>% arrange(name)

# schreiben
wb <- createWorkbook()
addWorksheet(wb, "Tab 6.7")

writeData(wb, "Tab 6.7", Tab_6_7)

writeData(wb, "Tab 6.7", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018 - Wert 2013)/Wert 2018*100"), 
          startCol=2, startRow=nrow(Tab_6_7)+3, colNames=F)
addStyle(wb, "Tab 6.7", style_anmerkung, nrow(Tab_6_7)+3, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-7_Konsum_nach_Gütergruppen.xlsx"), 
             overwrite=T)

