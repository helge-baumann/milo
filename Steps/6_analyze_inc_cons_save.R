# Analyse der Haushaltsnettoeinkommen, Konsumausgaben, Sparausgaben

# Tabelle 6.1: 
# Deskription von Einkommen im Mindestlohnbereich und darüber, Mittelwerte----

# Basistabelle  
Tab_6_1 <-  
  EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  pivot_longer(cols=c(minijob_hh, gesamt), names_to="Variable", values_to="Wert") %>%
  group_by(milo_hh, Variable, Wert, welle) %>%
  summarise(
    `(1) Bruttoeinkommen_mean` = 
      round(wtd.mean(brutto_winsor/3, gewicht), digits=2),
    `(1) Bruttoeinkommen_N` = 
      round(sum(!is.na(brutto_winsor/3)), digits=2),
    `(2) Nettoeinkommen_mean` =
      round(wtd.mean(netto_oecd_winsor*oecd_weight/3, gewicht), digits=2),
    `(2) Nettoeinkommen_N` =
      round(sum(!is.na(netto_oecd_winsor*oecd_weight/3)), digits=2),
    `(3) Bruttoäquivalenzeinkommen_mean` = 
      round(wtd.mean(brutto_winsor/oecd_weight/3, gewicht), digits=2),
    `(3) Bruttoäquivalenzeinkommen_N` = 
      round(sum(!is.na(brutto_winsor/oecd_weight)), digits=2),
    `(4) Nettoäquivalenzeinkommen_mean` =
      round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=2),
    `(4) Nettoäquivalenzeinkommen_N` =
      round(sum(!is.na(netto_oecd_winsor/3)), digits=2),
    `(5) Lohnsumme gesamt_mean` = 
      round(wtd.mean(lohn_summe_winsor/3, gewicht), digits=2),
    `(5) Lohnsumme gesamt_N` = 
      round(sum(!is.na(lohn_summe_winsor/3)), digits=2),
    `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen_mean` = 
      round(wtd.mean(lohn_anteil_winsor, gewicht), digits=1),
    `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen_N` = 
      round(sum(!is.na(lohn_anteil_winsor)), digits=2),
  ) %>%
  pivot_longer(-c(welle, milo_hh, Variable, Wert)) %>%
  ungroup() %>%
  separate(name, into=c("name", "Kat"), sep="_") %>%
  select(-Variable) %>%
  pivot_wider(names_from=c(milo_hh, Wert, Kat)) %>% 
  arrange(name) 

# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_1)/2)) {
  
  Tab_6_1 <- Tab_6_1 %>% add_row(welle = "2018/2013", name = Tab_6_1$name[i*2])
  
  for(j in 3:ncol(Tab_6_1)) {
    
    Tab_6_1[nrow(Tab_6_1),j] <- 
      round(as.numeric((Tab_6_1[i*2,j]/Tab_6_1[(i*2)-1,j]-1)*100), digits=1)
    
  }
  
}

# neu sortieren
Tab_6_1 <- Tab_6_1 %>% arrange(name)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.1")

head <- str_split(names(Tab_6_1), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.1", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.1", Tab_6_1, startRow=4, colNames=F)


writeData(wb, "Tab 6.1", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_1)+6, colNames=F)
addStyle(wb, "Tab 6.1", style_anmerkung, nrow(Tab_6_1)+6, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.1", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_1$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_1), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.1", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_1$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_1), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.1", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_1$welle == "2018/2013" | str_detect(Tab_6_1$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_1), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.1", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_1$welle != "2018/2013" & 
                        !str_detect(Tab_6_1$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_1), "_mean$")),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-1_Deskription_Einkommen.xlsx"), 
             overwrite=T)

# Tabellen 6.2 bis 6.5: Auswertung nach Kriterien----
Tab_6_2bis5 <- 
  EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  mutate(
    Haushaltstypen = as_factor(hhtype),#
    `Anzahl Erwerbstätige im Haushalt` = ET_hh) %>%
  pivot_longer(cols=c(Haushaltstypen, `Anzahl Erwerbstätige im Haushalt`), 
               names_to="Variable", values_to="Wert") %>%
  pivot_longer(cols=c(gesamt, minijob_hh), 
               names_to="Variable2", values_to="Wert2") %>%
  group_by(welle, milo_hh, Variable, Wert, Variable2, Wert2) %>%
  summarise(
    # Mittelwerte'
    `(1) Bruttoeinkommen_mean` = 
      round(wtd.mean(brutto_winsor/3, gewicht), digits=2),
    `(2) Nettoeinkommen_mean` =
      round(wtd.mean(netto_oecd_winsor*oecd_weight/3, gewicht), digits=2),
    `(3) Bruttoäquivalenzeinkommen_mean` = 
      round(wtd.mean(brutto_winsor/oecd_weight/3, gewicht), digits=2),
    `(4) Nettoäquivalenzeinkommen_mean` =
      round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=2),
    `(5) Lohnsumme gesamt_mean` = 
      round(wtd.mean(lohn_summe_winsor/3, gewicht), digits=2),
    `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen_mean` = 
      round(wtd.mean(lohn_anteil_winsor, gewicht), digits=1),
    # N
    `(1) Bruttoeinkommen_N` = 
      round(sum(!is.na(brutto_winsor/3)), digits=0),
    `(2) Nettoeinkommen_N` =
      round(sum(!is.na(netto_oecd_winsor*oecd_weight/3)), digits=0),
    `(3) Bruttoäquivalenzeinkommen_N` = 
      round(sum(!is.na(brutto_winsor/oecd_weight/3)), digits=0),
    `(4) Nettoäquivalenzeinkommen_N` =
      round(sum(!is.na(netto_oecd_winsor/3)), digits=0),
    `(5) Lohnsumme gesamt_N` = 
      round(sum(!is.na(lohn_summe_winsor/3)), digits=0),
    `(6) Lohnsumme Anteil am Bruttohaushaltseinkommen_N` = 
      round(sum(!is.na(lohn_anteil_winsor)), digits=0)
  ) %>%
  pivot_longer(-c(welle, milo_hh, Variable, Wert, Variable2, Wert2)) %>%
  arrange(milo_hh, Variable, Wert, Variable2, Wert2, name, welle) %>%
  separate(name, into=c("name", "Kat"), sep="_") %>%
  pivot_wider(names_from=c(Kat), values_from=value) %>% 
  ungroup() %>%
  select(-Variable2)

# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_2bis5)/2)) { # für jede zweite Zeile 
  
  Tab_6_2bis5 <- 
    Tab_6_2bis5 %>% 
    add_row(
      welle = "2018/2013", 
      milo_hh = Tab_6_2bis5$milo_hh[i*2],
      name = Tab_6_2bis5$name[i*2],
      Variable = Tab_6_2bis5$Variable[i*2],
      Wert = Tab_6_2bis5$Wert[i*2],
      Wert2 = Tab_6_2bis5$Wert2[i*2]
    )
  
  Tab_6_2bis5[nrow(Tab_6_2bis5), "mean"] <- 
    round(as.numeric((Tab_6_2bis5[i*2, "mean"]/Tab_6_2bis5[(i*2)-1,"mean"]-1)
                 *100), digits=1)
  Tab_6_2bis5[nrow(Tab_6_2bis5), "N"] <- 
    round(as.numeric((Tab_6_2bis5[i*2, "N"]/Tab_6_2bis5[(i*2)-1,"N"]-1)
                     *100), digits=1)
  
}

# Tabelle splitten
Tab_6_2 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe" &
      Variable == "Haushaltstypen") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = c(Wert, Wert2), values_from=c(mean, N),
              names_glue = "{Wert}_{Wert2}_{.value}") %>%
  arrange(name, welle) 

Tab_6_2 <- Tab_6_2 %>%
  select(welle, name, 
         paste0(
           rep(unique(str_remove(names(Tab_6_2)[-c(1,2)], "mean|N")), each=2), 
           c("mean", "N")) )

Tab_6_3 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe" &
      Variable == "Anzahl Erwerbstätige im Haushalt") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = c(Wert, Wert2), values_from=c(mean, N),
              names_glue = "{Wert}_{Wert2}_{.value}") %>%
  arrange(name, welle) 

Tab_6_3 <- Tab_6_3 %>%
  select(welle, name, 
         paste0(
           rep(unique(str_remove(names(Tab_6_3)[-c(1,2)], "mean|N")), each=2), 
           c("mean", "N")) )

Tab_6_4 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn" &
      Variable == "Haushaltstypen") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = c(Wert, Wert2), values_from=c(mean, N),
              names_glue = "{Wert}_{Wert2}_{.value}") %>%
  arrange(name, welle) 

Tab_6_4 <- Tab_6_4 %>%
  select(welle, name, 
         paste0(
           rep(unique(str_remove(names(Tab_6_4)[-c(1,2)], "mean|N")), each=2), 
           c("mean", "N")) )

Tab_6_5 <- 
  Tab_6_2bis5 %>% 
  filter(
    milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn" &
      Variable == "Anzahl Erwerbstätige im Haushalt") %>%
  select(-c(milo_hh, Variable)) %>%
  pivot_wider(names_from = c(Wert, Wert2), values_from=c(mean, N),
              names_glue = "{Wert}_{Wert2}_{.value}") %>%
  arrange(name, welle) 

Tab_6_5 <- Tab_6_5 %>%
  select(welle, name, 
         paste0(
           rep(unique(str_remove(names(Tab_6_5)[-c(1,2)], "mean|N")), each=2), 
           c("mean", "N")) )

rm(Tab_6_2bis5)

# schreiben
wb <- createWorkbook()
addWorksheet(wb, "Tab 6.2")

head <- str_split(names(Tab_6_2), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.2", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.2", Tab_6_2, startRow=4, colNames=F)

writeData(wb, "Tab 6.2", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_2)+6, colNames=F)
addStyle(wb, "Tab 6.2", style_anmerkung, nrow(Tab_6_2)+6, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.2", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_2$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_2), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.2", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_2$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_2), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.2", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_2$welle == "2018/2013" | str_detect(Tab_6_2$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_2), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.2", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_2$welle != "2018/2013" & 
                        !str_detect(Tab_6_2$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_2), "_mean$")),
         gridExpand = T)
saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-2_Haushaltstypen_Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.3")

head <- str_split(names(Tab_6_3), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.3", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.3", Tab_6_3, startRow=4, colNames=F)


writeData(wb, "Tab 6.3", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_1)+6, colNames=F)
writeData(wb, "Tab 6.3", 
          paste0("Achtung: Konstellation Erwerbstätige = 0   ",
                 "in betrachteter Gruppe  per definitionem nicht möglich"), 
          startCol=2, startRow=nrow(Tab_6_3)+7, colNames=F)
addStyle(wb, "Tab 6.3", style_anmerkung, nrow(Tab_6_3)+6, 2)
addStyle(wb, "Tab 6.3", style_anmerkung, nrow(Tab_6_3)+7, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.3", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_3$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_3), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.3", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_3$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_3), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.3", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_3$welle == "2018/2013" | str_detect(Tab_6_3$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_3), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.3", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_3$welle != "2018/2013" & 
                        !str_detect(Tab_6_3$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_3), "_mean$")),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-3_Erwerbstätige_Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.4")

head <- str_split(names(Tab_6_4), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.4", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.4", Tab_6_4, startRow=4, colNames=F)

writeData(wb, "Tab 6.4", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_4)+6, colNames=F)
addStyle(wb, "Tab 6.4", style_anmerkung, nrow(Tab_6_4)+6, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.4", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_4$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_4), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.4", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_4$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_4), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.4", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_4$welle == "2018/2013" | str_detect(Tab_6_4$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_4), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.4", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_4$welle != "2018/2013" & 
                        !str_detect(Tab_6_4$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_4), "_mean$")),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-4_Haushaltstypen_ueber-Mindestlohnbereich.xlsx"), 
             overwrite=T)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.5")

head <- str_split(names(Tab_6_5), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.5", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.5", Tab_6_5, startRow=4, colNames=F)


writeData(wb, "Tab 6.5", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_5)+6, colNames=F)
writeData(wb, "Tab 6.5", 
          paste0("Achtung: Konstellation Erwerbstätige = 0   ",
                 "in betrachteter Gruppe  per definitionem nicht möglich"), 
          startCol=2, startRow=nrow(Tab_6_5)+7, colNames=F)
addStyle(wb, "Tab 6.5", style_anmerkung, nrow(Tab_6_5)+6, 2)
addStyle(wb, "Tab 6.5", style_anmerkung, nrow(Tab_6_5)+7, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.5", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_5$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_5), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.5", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_5$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_5), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.5", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_5$welle == "2018/2013" | str_detect(Tab_6_5$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_5), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.5", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_5$welle != "2018/2013" & 
                        !str_detect(Tab_6_5$name, "Anteil"))+3,
         cols = which(str_detect(names(Tab_6_5), "_mean$")),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-5_Erwerbstätige_ueber-Mindestlohnbereich.xlsx"), 
             overwrite=T)

# Tabelle 6.6: 
# Deskription des Konsums (imputiert) und Sparens im Mindestlohn-Bereich 
# und darüber----

Tab_6_6 <- 
  EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  pivot_longer(cols=c(gesamt, minijob_hh), 
               names_to="Variable", values_to="Wert") %>%
  group_by(welle, milo_hh, Variable, Wert) %>%
  summarise(
    # Mittelwerte
    `(1) Konsum imputiert_mean` = 
      round(wtd.mean(c_imp_winsor/3, gewicht), digits=2),
    `(2) Konsumquote mit Konsum imputiert_mean` =
      round(wtd.mean((c_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100, gewicht), digits=1),
    `(3) Konsum imputiert äquivalenzgewichtet_mean` = 
      round(wtd.mean(c_imp_winsor/oecd_weight/3, gewicht), digits=2),
    `(4) Sparen imputiert_mean` = 
      round(wtd.mean(save_imp_winsor/3, gewicht), digits=2),
    `(5) Sparquote mit Ersparnis imputiert_mean` =
      round(wtd.mean((save_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100, gewicht), digits=1),
    `(6) Sparen imputiert äquivalenzgewichtet_mean` = 
      round(wtd.mean(save_imp_winsor/oecd_weight/3, gewicht), digits=2),
    # N 
    `(1) Konsum imputiert_N` = 
      round(sum(!is.na(c_imp_winsor/3)), digits=0),
    `(2) Konsumquote mit Konsum imputiert_N` =
      round(sum(!is.na((c_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100)), digits=0),
    `(3) Konsum imputiert äquivalenzgewichtet_N` = 
      round(sum(!is.na((c_imp_winsor/oecd_weight/3))), digits=0),
    `(4) Sparen imputiert_N` = 
      round(sum(!is.na((save_imp_winsor/3))), digits=0),
    `(5) Sparquote mit Ersparnis imputiert_N` =
      round(sum(!is.na(((save_imp_winsor/3)/(netto_oecd_winsor*oecd_weight/3)*100))), digits=0),
    `(6) Sparen imputiert äquivalenzgewichtet_N` = 
      round(sum(!is.na(save_imp_winsor/oecd_weight/3)), digits=2),
  ) %>%
  pivot_longer(-c(welle, milo_hh, Variable, Wert)) %>%
  ungroup() %>%
  separate(name, into=c("name", "Kat"), sep="_") %>%
  select(-Variable) %>%
  pivot_wider(names_from=c(milo_hh, Wert, Kat)) %>% 
  arrange(name) 

# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_6)/2)) {
  
  Tab_6_6 <- Tab_6_6 %>% add_row(welle = "2018/2013", name = Tab_6_6$name[i*2])
  
  for(j in 3:ncol(Tab_6_6)) {
    
    Tab_6_6[nrow(Tab_6_6),j] <- 
      as.numeric((Tab_6_6[i*2,j]/Tab_6_6[(i*2)-1,j]-1)*100)
    
  }
  
}

# neu sortieren
Tab_6_6 <- Tab_6_6 %>% arrange(name)

wb <- createWorkbook()
addWorksheet(wb, "Tab 6.6")

head <- str_split(names(Tab_6_6), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.6", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.6", Tab_6_6, startRow=4, colNames=F)

writeData(wb, "Tab 6.6", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_6)+6, colNames=F)
addStyle(wb, "Tab 6.6", style_anmerkung, nrow(Tab_6_6)+6, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.6", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_6$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_6), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.6", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_6$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_6), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.6", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_6$welle == "2018/2013" | str_detect(Tab_6_6$name, "quote"))+3,
         cols = which(str_detect(names(Tab_6_6), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.6", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_6$welle != "2018/2013" & 
                        !str_detect(Tab_6_6$name, "quote"))+3,
         cols = which(str_detect(names(Tab_6_6), "_mean$")),
         gridExpand = T)


saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-6_Deskription_Konsum_Sparen.xlsx"), 
             overwrite=T)

# Tabelle 6.7: Konsum nach Gütergruppen im Mindestlohn-Bereich und darüber

Tab_6_7 <- 
  EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  pivot_longer(cols=c(gesamt, minijob_hh), 
               names_to="Variable", values_to="Wert") %>%
  group_by(welle, milo_hh, Variable, Wert) %>%
  summarise(
    across(
      starts_with(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                    paste0("konsum", c(5,7,9), "new_winsor"))), 
      .fns = list( 
        ~round(wtd.mean(./3/oecd_weight, gewicht), digits=2),
        ~round(wtd.mean((./3/oecd_weight)/(c_imp_winsor/3/oecd_weight)*100, gewicht), digits=1),
        ~round(sum(!is.na(./3/oecd_weight)), digits=0),
        ~round(sum(!is.na(./3/oecd_weight)/(c_imp_winsor/3/oecd_weight)*100), digits=0)
        )#,
      #.names = "{.col}_absolut"
    )
  ) %>%
  rename_at(
    vars(ends_with("_1")), 
    ~str_replace(., ., 
                 sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                          paste0("konsum", c(5,7,9), "new_winsor")), 
                        function(x) paste0(attributes(EVS[[x]])$label, "_absolut__mean"))
    )) %>%
  rename_at(
    vars(ends_with("_2")), 
    ~str_replace(., ., 
                 sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                          paste0("konsum", c(5,7,9), "new_winsor")), 
                        function(x) paste0(attributes(EVS[[x]])$label, "_anteil__mean"))
    )) %>%
  rename_at(
    vars(ends_with("_3")), 
    ~str_replace(., ., 
                 sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                          paste0("konsum", c(5,7,9), "new_winsor")), 
                        function(x) paste0(attributes(EVS[[x]])$label, "_absolut__N"))
    )) %>%
  rename_at(
    vars(ends_with("_4")), 
    ~str_replace(., ., 
                 sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                          paste0("konsum", c(5,7,9), "new_winsor")), 
                        function(x) paste0(attributes(EVS[[x]])$label, "_anteil__N"))
    )) %>%
  pivot_longer(-c(welle, milo_hh, Variable, Wert)) %>%
  ungroup() %>%
  select(-Variable) %>%
  separate(name, into=c("name", "Kat"), sep="__") %>%
  pivot_wider(names_from=c(milo_hh, Wert, Kat), values_from=value) %>% 
  arrange(name) 


# Verhältnis 2018/2013
for(i in 1:(nrow(Tab_6_7)/2)) {
  
  Tab_6_7 <- Tab_6_7 %>% add_row(
    welle = "2018/2013", name = Tab_6_7$name[i*2])
  
  for(j in 3:ncol(Tab_6_7)) {
    
    Tab_6_7[nrow(Tab_6_7),j] <- 
      as.numeric((Tab_6_7[i*2,j]-Tab_6_7[(i*2)-1,j])/Tab_6_7[i*2,j]*100)
    
  }
  
}

# neu sortieren
Tab_6_7 <- Tab_6_7 %>% arrange(name) %>%
  separate(name, into=c("name", "Kategorie"), sep="_")

# schreiben
wb <- createWorkbook()
addWorksheet(wb, "Tab 6.7")

head <- str_split(names(Tab_6_7), "_")
for(i in 1:length(head)) 
  writeData(wb, "Tab 6.7", head[[i]], startCol=i, startRow=4-length(head[[i]]))

writeData(wb, "Tab 6.7", Tab_6_7, startRow=4, colNames=F)

writeData(wb, "Tab 6.7", 
          paste0("Achtung: 2018/2013 als prozentuale Veränderung  ",
                 "(Wert 2018/Wert 2013-1)*100"), 
          startCol=2, startRow=nrow(Tab_6_7)+6, colNames=F)
addStyle(wb, "Tab 6.7", style_anmerkung, nrow(Tab_6_7)+6, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 6.7", 
         style = createStyle(numFmt = "0"), 
         rows = which(Tab_6_7$welle != "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_7), "_N$")),
         gridExpand = T)

# eine Nachkommastelle
addStyle(wb, sheet = "Tab 6.7", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_7$welle == "2018/2013")+3,
         cols = which(str_detect(names(Tab_6_7), "_N$")),
         gridExpand = T)

addStyle(wb, sheet = "Tab 6.7", 
         style = createStyle(numFmt = "0.0"), 
         rows = which(
           Tab_6_7$welle == "2018/2013" | str_detect(Tab_6_7$Kategorie, "anteil"))+3,
         cols = which(str_detect(names(Tab_6_7), "_mean$")),
         gridExpand = T)

# zwei Nachkommastellen
addStyle(wb, sheet = "Tab 6.7", 
         style = createStyle(numFmt = "0.00"), 
         rows = which(Tab_6_7$welle != "2018/2013" & 
                        !str_detect(Tab_6_7$Kategorie, "anteil"))+3,
         cols = which(str_detect(names(Tab_6_7), "_mean$")),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_6-7_Konsum_nach_Gütergruppen.xlsx"), 
             overwrite=T)