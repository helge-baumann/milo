# Haushaltsnettoeinkommen

#HELGE: WIR KÖNNEN DIE ERSPARNIS DEFINIEREN WIE IN DAS DIW ALS VERFÜGBARES EINKOMMEN - KONSUM
#HELGE: BEI DESKRIPTIVEN AUSWERTUNGEN DES ABSOLUTEN HAUSHALTSEINKOMMENS UND 
  # VON KONSUMGRÖSSEN KÖNNTEN WIR ZUSÄTZLICH AUCH TABELLEN MIT ÄQUIVALENZGEWICHTETE WERTE AUSGEBEN
#Hierfür nötige Variable OECD Gewicht habe ich im Datensatz mit aufgenommen (Variable oecd_weight)

# Namen für Tabellen

# Für eine saubere Analyse und Auswertung packen wir jetzt mal 
# die EVS 2013 und 2018 zusammen

evs13_red <- evs13 %>%
  mutate(gewicht = EF107, westost = EF1, bland = EF2U1, ID = EF2U2,
         welle = "2013") %>%
  select(ID, welle, gewicht, westost, bland, 
         starts_with(c("konsum", "cons", "c_imp", "save", "lohn", "monat", 
                     "stunde", "unterster", "milo_hh", "netto", "brutto", "hhtype",
                     "oecd")))
evs18_red <- evs18 %>%
  mutate(gewicht = EF107, westost = EF1, bland = EF2U1, ID = EF2U2, welle = "2018") %>%
  select(ID, welle, gewicht, westost, bland, 
         starts_with(c("konsum", "cons", "c_imp", "save", "lohn", "monat", 
                       "stunde", "unterster", "milo_hh", "netto", "brutto", "hhtype",
                       "oecd")))

EVS <- rbind(evs13_red, evs18_red)

# Auswertung

# Deflationierung
#HH_auswertung <- 
EVS_defl <- EVS %>% 
  mutate(
    gesamt = "gesamt", 
    lohn_haushalt = if_else(lohn_empfänger == 0, "HH ohne Lohnberechnung", 
    "HH mit Lohnberechnung"),
    hh_size=as.character(hhtype),
    # Deflationierung
    brutto_winsor = if_else(welle == "2013", brutto_winsor * 1/0.985, brutto_winsor*1/1.038),
    netto_oecd_winsor = if_else(welle == "2013", netto_oecd_winsor * 1/0.985, netto_oecd_winsor*1/1.038),
    cons_winsor = if_else(welle == "2013", cons_winsor * 1/0.985, cons_winsor*1/1.038),
    c_imp_winsor = if_else(welle == "2013", c_imp_winsor * 1/0.985, c_imp_winsor*1/1.038),
    save_winsor = if_else(welle == "2013", save_winsor * 1/0.985, save_winsor*1/1.038),
    save_imp_winsor = if_else(welle == "2013", save_imp_winsor * 1/0.985, save_imp_winsor*1/1.038),
    konsum1_winsor = if_else(welle == "2013", konsum1_winsor * 1/0.983, konsum1_winsor*1/1.06),
    konsum2_winsor = if_else(welle == "2013", konsum2_winsor * 1/0.946, konsum2_winsor*1/1.08),
    konsum3_winsor = if_else(welle == "2013", konsum3_winsor * 1/0.991, konsum3_winsor*1/1.017),
    konsum4_winsor = if_else(welle == "2013", konsum4_winsor * 1/0.995, konsum4_winsor*1/1.03),
    konsum6_winsor = if_else(welle == "2013", konsum6_winsor * 1/0.965, konsum6_winsor*1/1.034),
    konsum8_winsor = if_else(welle == "2013", konsum8_winsor * 1/1.027, konsum8_winsor*1/0.966),
    konsum10_winsor = if_else(welle == "2013", konsum10_winsor * 1/1.023, konsum10_winsor*1/1.036),
    konsum11_winsor = if_else(welle == "2013", konsum11_winsor * 1/0.953, konsum11_winsor*1/1.067),
    konsum12_winsor = if_else(welle == "2013", konsum12_winsor * 1/0.972, konsum12_winsor*1/1.036),
    konsum5new_winsor = if_else(welle == "2013", konsum5new_winsor * 1/0.989, konsum5new_winsor*1/1.018),
    konsum7new_winsor = if_else(welle == "2013", konsum7new_winsor * 1/1.02, konsum7new_winsor*1/1.052),
    konsum9new_winsor = if_else(welle == "2013", konsum9new_winsor * 1/0.948, konsum9new_winsor*1/1.038)
    ) 

# Auswertung nach Kriterien
 EVS_defl %>% pivot_longer(cols=c(gesamt, lohn_haushalt, milo_hh_stat, milo_hh_dyn, hh_size), 
               names_to="Variable", values_to="Wert") %>%
  group_by(welle, Variable, Wert) %>%
  summarise(
    `Äquivalenzeinkommen brutto`= round(wtd.mean(brutto_winsor/3/oecd_weight, gewicht), digits=1),
    `Äquivalenzeinkommen netto`= round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=1),
    `Konsum gesamt` = round(wtd.mean(cons_winsor/3/oecd_weight, gewicht), digits=1),
    `Konsum (imputiert) gesamt` = round(wtd.mean(c_imp_winsor/3/oecd_weight, gewicht), digits=1),
    `Sparen gesamt` = round(wtd.mean(save_winsor/3/oecd_weight, gewicht), digits=1),
    `Sparen (imputiert) gesamt` = round(wtd.mean(save_imp_winsor/3/oecd_weight, gewicht), digits=1),
    across(
      starts_with(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                    paste0("konsum", c(5,7,9), "new_winsor"))), 
      .fns = list( ~round(wtd.mean(./3/oecd_weight, gewicht), digits=1))#,
     # .names = LETTERS[1:12] 
      )) %>%
    rename_at(vars(ends_with("_1")), 
              ~str_replace(., ., 
                           sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                                    paste0("konsum", c(5,7,9), "new_winsor")), 
                                  function(x) attributes(EVS[[x]])$label))
                            ) %>%
  pivot_wider(names_from=welle, values_from=-c(welle, Variable, Wert)) %>%
    write.csv2("Output/Werte.csv")

# Auswertung nach Milo und Haushaltsgrößen 
 EVS_defl %>% 
   group_by(welle, milo_hh_dyn, hh_size) %>%
   summarise(
     `Äquivalenzeinkommen brutto`= round(wtd.mean(brutto_winsor/3/oecd_weight, gewicht), digits=1),
     `Äquivalenzeinkommen netto`= round(wtd.mean(netto_oecd_winsor/3, gewicht), digits=1),
     `Konsum gesamt` = round(wtd.mean(cons_winsor/3/oecd_weight, gewicht), digits=1),
     `Konsum (imputiert) gesamt` = round(wtd.mean(c_imp_winsor/3/oecd_weight, gewicht), digits=1),
     `Sparen gesamt` = round(wtd.mean(save_winsor/3/oecd_weight, gewicht), digits=1),
     `Sparen (imputiert) gesamt` = round(wtd.mean(save_imp_winsor/3/oecd_weight, gewicht), digits=1),
     across(
       starts_with(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                     paste0("konsum", c(5,7,9), "new_winsor"))), 
       .fns = list( ~round(wtd.mean(./3/oecd_weight, gewicht), digits=1))#,
       # .names = LETTERS[1:12] 
     )) %>%
   rename_at(vars(ends_with("_1")), 
             ~str_replace(., ., 
                          sapply(c(paste0("konsum", c(1:4, 6, 8, 10:12), "_winsor"),
                                   paste0("konsum", c(5,7,9), "new_winsor")), 
                                 function(x) attributes(EVS[[x]])$label))
   ) %>%
   pivot_wider(names_from=welle, values_from=-c(welle, milo_hh_dyn, hh_size)) %>%
   write.csv2("Output/Werte_milo.csv")






