# Strukturen Kapitel 5

# Tabelle 5.1----
Tab_5_1 <- EVS_defl %>%
  filter(!is.na(milo_hh)) %>%
  group_by(welle) %>%
  summarise(
    # Anzahl der Haushalte mit Beschäftigten (mit Lohnberechnung)
    `Haushalte mit Beschäftigten insgesamt__N` = 
      sum(lohn_empfänger > 0),
    `Haushalte mit Beschäftigten insgesamt__%` = 
      wtd.mean(lohn_empfänger > 0, gewicht)*100,
    `Haushalte im Mindestlohn-Bereich__N` = 
      sum(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe"),
    `Haushalte im Mindestlohn-Bereich__%` = 
      wtd.mean(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe", 
               gewicht)*100,
    `Haushalte über dem Mindestlohn-Bereich__N` = 
      sum(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn"),
    `Haushalte über dem Mindestlohn-Bereich__%` = 
      wtd.mean(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn", 
               gewicht)*100
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
          colNames=F, borders="surrounding")
writeData(wb, "Tab 5.1", 
          paste0("Achtung: Milo-Berechtigung nur, wenn gültiger Stundenlohn vorliegt! "),
          startCol=2, startRow=ncol(Tab_5_1)+2, colNames=F)
addStyle(wb, "Tab 5.1", style_anmerkung, ncol(Tab_5_1)+2, 2)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_5-1_Fallzahlen.xlsx"), 
             overwrite=T)



# Tabelle 5.2 (Struktur Mindestlohn-Bereich) und 5.3 (HH über Mindestlohn)----

# Personen-Variablen generieren
for(i in 1:6) {

  # leere Vektoren einrichten
  EVS_defl[[paste0("lohn_w_", i, "_winsor")]] <- NA
  EVS_defl[[paste0("lohn_vollzeit_", i, "_winsor")]] <- NA
  EVS_defl[[paste0("lohn_teilzeit_", i, "_winsor")]] <- NA
  
  # Frauen
  EVS_defl[[paste0("lohn_w_", i, "_winsor")]][
    EVS_defl[[paste0("sex_", i)]] == 2 &
      !is.na(EVS_defl[[paste0("lohn_", i, "_winsor")]])
    ] <- EVS_defl[[paste0("lohn_", i, "_winsor")]][
      EVS_defl[[paste0("sex_", i)]] == 2 &
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
      teilzeit_n = rowSums(.[paste0("vollzeit_", 1:6)] == 0, na.rm=T),
      vollzeit_n = rowSums(.[paste0("vollzeit_", 1:6)] == 1, na.rm=T),
      # Zeilensummen für bedingte Löhne (Frauen, Voll-/Teilzeit)
      lohn_frauen_sum = rowSums(.[paste0("lohn_w_", 1:6, "_winsor")], na.rm=T),
      lohn_vollzeit_sum = rowSums(.[paste0("lohn_vollzeit_", 1:6, "_winsor")], na.rm=T),
      lohn_teilzeit_sum = rowSums(.[paste0("lohn_teilzeit_", 1:6, "_winsor")], na.rm=T)
    ) %>%
  group_by(milo_hh, welle) %>%
  summarise(
    # Ebene: Haushaltsebene
    `Haushaltsebene_Gesamt__N` = n(),
    `Haushaltsebene_Gesamt__%` = wtd.mean(!is.na(milo_hh), gewicht)*100,
    `Haushaltsebene_Paarhaushalte mit Kindern__N` = 
      sum(hhtype == 4), 
    `Haushaltsebene_Paarhaushalte mit Kindern__%` = 
      wtd.mean(hhtype == 4, gewicht)*100, 
    `Haushaltsebene_Paarhaushalte ohne Kinder__N` = 
      sum(hhtype == 3), 
    `Haushaltsebene_Paarhaushalte ohne Kinder__%` = 
      wtd.mean(hhtype == 3, gewicht)*100, 
    `Haushaltsebene_Single-Haushalte__N` = 
      sum(hhtype == 1),
    `Haushaltsebene_Single-Haushalte__%` = 
      wtd.mean(hhtype == 1, gewicht)*100,
    `Haushaltsebene_Alleinerziehenden-Haushalte__N` = 
      sum(hhtype == 2), 
    `Haushaltsebene_Alleinerziehenden-Haushalte__%` = 
    wtd.mean(hhtype == 2, gewicht)*100,
    `Haushaltsebene_Sonstige Haushalte__N` = 
    sum(hhtype == 5),
    `Haushaltsebene_Sonstige Haushalte__%` = 
      wtd.mean(hhtype == 5, gewicht)*100, 
    # Ebene: Personen (Frauen und geringfügig Beschäftigte)
    `Personenebene_Frauen__N` = 
      sum(frauen_n, na.rm=T),
    `Personenebene_Frauen__%` = 
      sum(frauen_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100,
    `Personenebene_Frauen-Anteil an den abh. Erwerbseinkommen__%` = 
      sum(lohn_frauen_sum*gewicht)/sum(lohn_summe_winsor*gewicht)*100,
    # Teilzeit
    `Personenebene_Teilzeit__N` = 
      sum(teilzeit_n, na.rm=T),
    `Personenebene_Teilzeit__%` = 
      sum(teilzeit_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100,
    `Personenebene_Teilzeit-Anteil an den abh. Erwerbseinkommen__%` = 
      sum(lohn_teilzeit_sum*gewicht)/sum(lohn_summe_winsor*gewicht)*100,
    # Vollzeit
    `Personenebene_Vollzeit__N` = 
      sum(vollzeit_n, na.rm=T),
    `Personenebene_Vollzeit__%` = 
      sum(vollzeit_n*gewicht, na.rm=T)/sum(lohn_empfänger*gewicht)*100,
    `Personenebene_Vollzeit-Anteil an den abh. Erwerbseinkommen__%` = 
      sum(lohn_vollzeit_sum*gewicht)/sum(lohn_summe_winsor*gewicht)*100,
    # Regionalebene (Haushaltsebene)
    `Haushaltsebene_Neue Länder einschließlich Berlin__N` = 
      sum(EF1 == 44), 
    `Haushaltsebene_Neue Länder einschließlich Berlin__%` = 
      wtd.mean(EF1 == 44, gewicht)*100, 
    `Haushaltsebene_Früheres Bundesgebiet ohne Berlin__N` = 
      sum(EF1 == 33), 
    `Haushaltsebene_Früheres Bundesgebiet ohne Berlin__%` = 
      wtd.mean(EF1 == 33, gewicht)*100
  )

# Tabelle splitten
Tab_5_2 <- Tab_5_23 %>%
  filter(milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe") %>% 
  ungroup() %>% 
  select(-milo_hh) %>%
  pivot_longer(-welle, names_to = c('.value', 'Wert'), names_sep="__") 

Tab_5_3 <- Tab_5_23 %>%
  filter(milo_hh == "(2) Unterster Lohn im HH oberhalb Mindestlohn") %>% 
  ungroup() %>% 
  select(-milo_hh) %>%
  pivot_longer(-welle, names_to = c('.value', 'Wert'), names_sep="__")

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
writeData(wb, "Tab 5.2", t(Tab_5_2[,1:2]), startCol=3, startRow=1, colNames=F)
writeData(wb, "Tab 5.2", t(Tab_5_2[,3:ncol(Tab_5_2)]), startCol=3, startRow=3, 
          colNames=F, borders="surrounding")
writeData(wb, "Tab 5.2", 
          paste0("Achtung: Nur Haushalte mit gültiger Stundenlohnberechnung"), 
          startCol=3, startRow=ncol(Tab_5_2)+2, colNames=F)
writeData(wb, "Tab 5.2", 
          paste0("Achtung: ",
                 "Personenwerte als Anteile über alle Personen hinweg! "), 
          startCol=3, startRow=ncol(Tab_5_2)+3, colNames=F)
addStyle(wb, "Tab 5.2", style_anmerkung, ncol(Tab_5_2)+2, 3)
addStyle(wb, "Tab 5.2", style_anmerkung, ncol(Tab_5_2)+3, 3)

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
writeData(wb, "Tab 5.3", t(Tab_5_3[,1:2]), startCol=3, startRow=1, colNames=F)
writeData(wb, "Tab 5.3", t(Tab_5_3[,3:ncol(Tab_5_3)]), startCol=3, startRow=3, 
          colNames=F, borders="surrounding")
writeData(wb, "Tab 5.3", 
          paste0("Achtung: Nur Haushalte mit gültiger Stundenlohnberechnung"), 
          startCol=3, startRow=ncol(Tab_5_3)+2, colNames=F)
writeData(wb, "Tab 5.3", 
          paste0("Achtung: ",
                 "Personenwerte als Anteile über alle Personen hinweg! "), 
          startCol=3, startRow=ncol(Tab_5_3)+3, colNames=F)
addStyle(wb, "Tab 5.3", style_anmerkung, ncol(Tab_5_3)+2, 3)
addStyle(wb, "Tab 5.3", style_anmerkung, ncol(Tab_5_3)+3, 3)

saveWorkbook(wb, paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                        "Tab_5-3_Struktur ",
                        "(Haushalte über Mindestlohnbereich).xlsx"), 
             overwrite=T)

# Tabelle 5.4 (Mindestlohn-HH in Dezilen der Einkommensverteilung)----

# Frage: Wie viele Haushalte sind in welchen Dezilen der Einkommensverteilung?

Tab_5_4 <- EVS_defl %>%
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
      sum(
        (milo_hh == "(1) Unterster Lohn im HH in Mindestlohngruppe")*gewicht,
        na.rm=T)/milo_n*100
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

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
                    "Tab_5-4_Dezile_Milo-HH_Einkommensverteilung.xlsx"), 
             overwrite=T)

  
  
