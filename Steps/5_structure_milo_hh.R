# Strukturen Kapitel 5

EVS_defl <- EVS_defl %>%
  mutate(
    minijob_hh = case_when(
      rowSums(.[paste0("minijob_", 1:6)], na.rm=T) > 0 ~ "Minijob-HH", 
      rowSums(.[paste0("minijob_", 1:6)], na.rm=T) == 0 ~ "kein Minijob-HH"
    ))

# Tabelle 5.1----
Tab_5_1 <- EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  group_by(welle) %>%
  summarise(
    # Anzahl der Haushalte mit Beschäftigten (mit Lohnberechnung)
    `Haushalte mit Beschäftigten insgesamt__N` = 
      round(sum(lohn_empfänger > 0), digits=0), 
    `Haushalte mit Beschäftigten insgesamt__%` = 
      round(wtd.mean(lohn_empfänger > 0, gewicht)*100, digits=1), 
    `Haushalte im Mindestlohn-Bereich__N` = 
      round(sum(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe"), digits=0), 
    `Haushalte im Mindestlohn-Bereich__%` = 
      round(wtd.mean(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe", 
               gewicht)*100, digits=1), 
    `Haushalte über dem Mindestlohn-Bereich__N` = 
      round(sum(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn"), digits=0), 
    `Haushalte über dem Mindestlohn-Bereich__%` = 
      round(wtd.mean(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn", 
               gewicht)*100, digits=1)
  ) %>% 
  pivot_longer(
    -welle,
    names_to = c('.value', 'Wert'), 
    names_sep="__") 

wb <- createWorkbook()
addWorksheet(wb, "Tab 5.1")

writeData(wb, "Tab 5.1", names(Tab_5_1)[3:ncol(Tab_5_1)], startCol=1, startRow=3)
writeData(wb, "Tab 5.1", t(Tab_5_1[,1:2]), startCol=2, startRow=1, colNames=F)
writeData(wb, "Tab 5.1", t(Tab_5_1[,3:ncol(Tab_5_1)]), startCol=2, startRow=3, 
          colNames=F)
writeData(wb, "Tab 5.1", 
          paste0("Achtung: Milo-Berechtigung nur, ", 
                 "wenn gültiger Stundenlohn vorliegt! "),
          startCol=2, startRow=ncol(Tab_5_1)+2, colNames=F)
addStyle(wb, "Tab 5.1", style_anmerkung, ncol(Tab_5_1)+2, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 5.1", 
         style = createStyle(numFmt = "0.0"), 
         rows = 3:(nrow(Tab_5_1)+2), 
         cols = which(Tab_5_1$Wert == "%")+1, 
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_5-1_Fallzahlen.xlsx"), 
             overwrite=T)



# Tabelle 5.2 (Struktur Mindestlohn-Bereich) und 5.3 (HH über Mindestlohn)----

# Personen-Variablen generieren
for(i in 1:6) {

  # leere Vektoren einrichten
  EVS_defl[[paste0("lohn_w_", i, "_winsor")]] <- 0
  EVS_defl[[paste0("lohn_m_", i, "_winsor")]] <- 0
  EVS_defl[[paste0("lohn_vollzeit_", i, "_winsor")]] <- 0
  EVS_defl[[paste0("lohn_teilzeit_", i, "_winsor")]] <- 0
  
  # Frauen
  EVS_defl[[paste0("lohn_w_", i, "_winsor")]][
    EVS_defl[[paste0("sex_", i)]] == 2 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
    ] <- EVS_defl[[paste0("lohn_", i, "_winsor")]][
      EVS_defl[[paste0("sex_", i)]] == 2 &
        !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
    ]
  
  # Männer
  EVS_defl[[paste0("lohn_m_", i, "_winsor")]][
    EVS_defl[[paste0("sex_", i)]] == 1 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ] <- EVS_defl[[paste0("lohn_", i, "_winsor")]][
    EVS_defl[[paste0("sex_", i)]] == 1 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ]
  
  # Vollzeit
  EVS_defl[[paste0("lohn_vollzeit_", i, "_winsor")]][
    EVS_defl[[paste0("vollzeit_", i)]] == 1 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ] <- EVS_defl[[paste0("lohn_", i, "_winsor")]][
    EVS_defl[[paste0("vollzeit_", i)]] == 1 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ]
  
  # Teilzeit
  EVS_defl[[paste0("lohn_teilzeit_", i, "_winsor")]][
    EVS_defl[[paste0("vollzeit_", i)]] == 0 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ] <- EVS_defl[[paste0("lohn_", i, "_winsor")]][
    EVS_defl[[paste0("vollzeit_", i)]] == 0 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
  ]

}

Tab_5_23 <- 
  EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
    mutate(
      frauen_n = rowSums(.[paste0("sex_", 1:6)] == 2, na.rm=T),
      männer_n = rowSums(.[paste0("sex_", 1:6)] == 1, na.rm=T),
      teilzeit_n = rowSums(.[paste0("vollzeit_", 1:6)] == 0, na.rm=T),
      vollzeit_n = rowSums(.[paste0("vollzeit_", 1:6)] == 1, na.rm=T),
      minijob_n = rowSums(.[paste0("minijob_", 1:6)] == 1, na.rm=T),
      # Zeilensummen für bedingte Löhne (Frauen, Voll-/Teilzeit)
      lohn_frauen_sum = rowSums(.[paste0("lohn_w_", 1:6, "_winsor")], na.rm=T),
      lohn_männer_sum = rowSums(.[paste0("lohn_m_", 1:6, "_winsor")], na.rm=T),
      lohn_summe_ohne_deflation = rowSums(.[paste0("lohn_", 1:6, "_winsor")], na.rm=T),
      #lohn_vollzeit_sum = rowSums(.[paste0("lohn_vollzeit_", 1:6, "_winsor")], na.rm=T),
      #lohn_teilzeit_sum = rowSums(.[paste0("lohn_teilzeit_", 1:6, "_winsor")], na.rm=T)
    ) %>%
  pivot_longer(cols=c(minijob_hh, gesamt), names_to="Variable", values_to="Wert") %>%
  group_by(milo_hh, Variable, Wert, welle) %>%
  summarise(
    # Ebene: Haushaltsebene
    `Haushaltsebene_Gesamt__N` = 
      round(n(), digits=0),
    `Haushaltsebene_Gesamt__%` = 
      round(wtd.mean(!is.na(milo_hh), gewicht)*100, digits=1), 
    `Haushaltsebene_Paarhaushalte mit Kindern__N` = 
      round(sum(hhtype == 4), digits=0),
    `Haushaltsebene_Paarhaushalte mit Kindern__%` = 
      round(wtd.mean(hhtype == 4, gewicht)*100, digits=1),
    `Haushaltsebene_Paarhaushalte ohne Kinder__N` = 
      round(sum(hhtype == 3), digits=0), 
    `Haushaltsebene_Paarhaushalte ohne Kinder__%` = 
      round(wtd.mean(hhtype == 3, gewicht)*100, digits=1),
    `Haushaltsebene_Single-Haushalte__N` = 
      round(sum(hhtype == 1), digits=0),
    `Haushaltsebene_Single-Haushalte__%` = 
      round(wtd.mean(hhtype == 1, gewicht)*100, digits = 1), 
    `Haushaltsebene_Alleinerziehenden-Haushalte__N` = 
      round(sum(hhtype == 2), digits=0), 
    `Haushaltsebene_Alleinerziehenden-Haushalte__%` = 
      round(wtd.mean(hhtype == 2, gewicht)*100, digits=1), 
    `Haushaltsebene_Sonstige Haushalte__N` = 
      round(sum(hhtype == 5), digits=0), 
    `Haushaltsebene_Sonstige Haushalte__%` = 
      round(wtd.mean(hhtype == 5, gewicht)*100, 1), 
    # Ebene: Personen (Frauen und geringfügig Beschäftigte)
    `Personenebene_Frauen__N` = 
      round(sum(frauen_n, na.rm=T), digits=0), 
    `Personenebene_Frauen__%` = 
      round(sum(frauen_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100, digits=1),
    `Personenebene_Frauen-Anteil an den abh. Erwerbseinkommen__%` = 
      round(sum(lohn_frauen_sum*gewicht)/sum(lohn_summe_ohne_deflation*gewicht)*100, digits=1), 
    # Vollzeit
    `Personenebene_Geringfügige_Beschäftigung__N` = 
      round(sum(minijob_n, na.rm=T), digits=0), 
    `Personenebene_Geringfügige_Beschäftigung__%` = 
      round(sum(minijob_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100, digits=1),
    # Teilzeit
    `Personenebene_Teilzeit__N` = 
      round(sum(teilzeit_n, na.rm=T), digits=0), 
    `Personenebene_Teilzeit__%` = 
      round(sum(teilzeit_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100, digits=1), 
    # Vollzeit
    `Personenebene_Vollzeit__N` = 
      round(sum(vollzeit_n, na.rm=T), digits=0), 
    `Personenebene_Vollzeit__%` = 
      round(sum(vollzeit_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100, digits=1), 
    # Regionalebene (Haushaltsebene)
    `Haushaltsebene_Neue Länder einschließlich Berlin__N` = 
      round(sum(EF1 == 44),  digits=0), 
    `Haushaltsebene_Neue Länder einschließlich Berlin__%` = 
      round(wtd.mean(EF1 == 44, gewicht)*100, digits=1), 
    `Haushaltsebene_Früheres Bundesgebiet ohne Berlin__N` = 
      round(sum(EF1 == 33), digits=0), 
    `Haushaltsebene_Früheres Bundesgebiet ohne Berlin__%` = 
      round(wtd.mean(EF1 == 33, gewicht)*100, digits=1)
  )

# Tabelle splitten
Tab_5_2 <- Tab_5_23 %>%
  filter(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe") %>% 
  ungroup() %>% 
  select(-c(milo_hh, Variable)) %>%
  pivot_longer(-c(welle, Wert), names_to = c('.value', 'Kategorie'), 
               names_sep="__", names_repair="unique")

Tab_5_3 <- Tab_5_23 %>%
  filter(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn") %>% 
  ungroup() %>% 
  select(-c(milo_hh, Variable)) %>%
  pivot_longer(-c(welle, Wert), names_to = c('.value', 'Kategorie'), 
               names_sep="__", names_repair="unique")

# Tabellen schreiben

# 5.2
wb <- createWorkbook()
addWorksheet(wb, "Tab 5.2")

writeData(wb, "Tab 5.2", 
          sapply(str_split(names(Tab_5_2), fixed("_")), 
                 function(x) x[1])[3:ncol(Tab_5_2)], 
          startCol=1, startRow=3)
writeData(wb, "Tab 5.2", 
          unlist(sapply(str_split(names(Tab_5_2), fixed("_")), 
                        function(x) paste(x[-1], collapse=" "))[3:ncol(Tab_5_2)]),
          startCol=2, startRow=3)
writeData(wb, "Tab 5.2", t(Tab_5_2[,1:3]), startCol=3, startRow=1, colNames=F)
writeData(wb, "Tab 5.2", t(Tab_5_2[,4:ncol(Tab_5_2)]), startCol=3, startRow=4, 
          colNames=F)
writeData(wb, "Tab 5.2", 
          paste0("Achtung: Nur Haushalte mit gültiger Stundenlohnberechnung"), 
          startCol=3, startRow=ncol(Tab_5_2)+3, colNames=F)
writeData(wb, "Tab 5.2", 
          paste0("Achtung: ",
                 "Personenwerte als Anteile über alle Personen hinweg! "), 
          startCol=3, startRow=ncol(Tab_5_2)+4, colNames=F)
addStyle(wb, "Tab 5.2", style_anmerkung, ncol(Tab_5_2)+3, 3)
addStyle(wb, "Tab 5.2", style_anmerkung, ncol(Tab_5_2)+4, 3)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 5.2", 
         style = createStyle(numFmt = "0.0"), 
         rows = 4:(ncol(Tab_5_2)+3), 
         cols = which(Tab_5_2$Kategorie == "%")+2, 
         gridExpand = T)

saveWorkbook(wb, paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                        "Tab_5-2_Struktur ",
                        "(Haushalte im Mindestlohnbereich).xlsx"), 
             overwrite=T)

# 5.3
wb <- createWorkbook()
addWorksheet(wb, "Tab 5.3")

writeData(wb, "Tab 5.3", 
          sapply(str_split(names(Tab_5_3), fixed("_")), 
                 function(x) x[1])[3:ncol(Tab_5_3)], 
          startCol=1, startRow=3)
writeData(wb, "Tab 5.3", 
          unlist(sapply(str_split(names(Tab_5_3), fixed("_")), 
                        function(x) paste(x[-1], collapse=" "))[3:ncol(Tab_5_3)]),
          startCol=2, startRow=3)
writeData(wb, "Tab 5.3", t(Tab_5_3[,1:3]), startCol=3, startRow=1, colNames=F)
writeData(wb, "Tab 5.3", t(Tab_5_3[,4:ncol(Tab_5_3)]), startCol=3, startRow=4, 
          colNames=F)
writeData(wb, "Tab 5.3", 
          paste0("Achtung: Nur Haushalte mit gültiger Stundenlohnberechnung"), 
          startCol=3, startRow=ncol(Tab_5_3)+3, colNames=F)
writeData(wb, "Tab 5.3", 
          paste0("Achtung: ",
                 "Personenwerte als Anteile über alle Personen hinweg! "), 
          startCol=3, startRow=ncol(Tab_5_3)+4, colNames=F)
addStyle(wb, "Tab 5.3", style_anmerkung, ncol(Tab_5_3)+3, 3)
addStyle(wb, "Tab 5.3", style_anmerkung, ncol(Tab_5_3)+4, 3)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 5.3", 
         style = createStyle(numFmt = "0.0"), 
         rows = 4:(ncol(Tab_5_3)+3), 
         cols = which(Tab_5_3$Kategorie == "%")+2, 
         gridExpand = T)

saveWorkbook(wb, paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                        "Tab_5-3_Struktur ",
                        "(Haushalte über Mindestlohnbereich).xlsx"), 
             overwrite=T)


# Tabelle 5.4 (Mindestlohn-HH in Dezilen der Einkommensverteilung)----

# Frage: Wie viele Haushalte sind in welchen Dezilen der Einkommensverteilung?

Tab_5_4 <- 
  EVS_defl %>%
  group_by(welle) %>%
  # Dezile der Haushaltsnettoäquivalenzeinkommen: Zuweisung
  mutate(
    d_einkommen = cut(
      netto_oecd_winsor, 
      breaks= wtd.quantile(netto_oecd_winsor, gewicht, probs=seq(0, 1, 0.1)),
      labels= paste0(1:10, ". Dezil")
    ),
    # gewichtete Summe der Mindestlohn-HH pro Welle für spätere Anteile
    milo_n = sum(
      (milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe")*gewicht, na.rm=T
      )
    ) %>% 
  group_by(welle, d_einkommen) %>%
  summarise(
    # Anteil Milo-HH pro Dezil als Anteil an allen Milo-HH (in der Welle)
    p = 
      round(sum(
        (milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe")*gewicht,
        na.rm=T)/milo_n*100, digits=1)
    )

Tab_5_4 <- 
  unique(Tab_5_4) %>%
  pivot_wider(names_from = welle, values_from = p) %>% 
  rename(Dezil = 1)

# Tabelle abspeichern
wb <- createWorkbook()

addWorksheet(wb, "Tab 5.4")
writeData(wb, "Tab 5.4", Tab_5_4)

writeData(wb, "Tab 5.4", 
          paste0("Achtung: Anteile berechnet als (Anteil der  ",
                 "Mindestlohn-HH im Dezil als Anteil an allen ",
                 "Milo-HH in Welle) * 100"), 
          startCol=2, startRow=nrow(Tab_5_4)+3, colNames=F)
addStyle(wb, "Tab 5.4", style_anmerkung, nrow(Tab_5_4)+3, 2)

# Formatierung der Nachkommastellen
addStyle(wb, sheet = "Tab 5.4", 
         style = createStyle(numFmt = "0.0"), 
         rows = 2:(nrow(Tab_5_4)+1), 
         cols = which(sapply(Tab_5_4, class) == "numeric"),
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_5-4_Dezile_Milo-HH_Einkommensverteilung.xlsx"), 
             overwrite=T)

  
  
