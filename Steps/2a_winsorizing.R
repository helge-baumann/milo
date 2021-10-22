# Winsorizing (plus Plausibilitätscheck)----

evs13 <- 
  evs13 %>%
  mutate(netto_oecd = EF62 / oecd_weight, brutto = EF60, netto = EF62) %>%
  mutate(
    across(
    starts_with(c(
      "lohn", "monat", "stunde", "konsum", "c_imp", "cons", "az",
      "save_imp", "save", "netto", "brutto"
    )),
    .fns = list(winsor = ~ winsor(.))
  ),
  lohn_summe = rowSums(.[paste0("lohn_", 1:6)], na.rm = T),
  lohn_summe_winsor = winsor(lohn_summe),
  lohn_anteil = if_else(lohn_summe > 0, lohn_summe / brutto * 100, NA_real_),
  lohn_anteil_winsor = winsor(lohn_anteil)
  )

evs18 <- 
  evs18 %>%
  mutate(netto_oecd = EF62 / oecd_weight, brutto = EF60, 
         netto = EF62) %>%
  mutate(across(
    starts_with(c(
      "lohn", "monat", "stunde", "konsum", "c_imp", "cons", "az",
      "save_imp", "save", "netto", "brutto"
    )),
    .fns = list(winsor = ~ winsor(.))
  ),
  lohn_summe = rowSums(.[paste0("lohn_", 1:6)], na.rm = T),
  lohn_summe_winsor = winsor(lohn_summe),
  lohn_anteil = if_else(lohn_summe > 0, lohn_summe / brutto * 100, NA_real_),
  lohn_anteil_winsor = winsor(lohn_anteil)
  )

# Plausibilitätscheck
ws <- names(evs13)[str_detect(names(evs13), "winsor")]
ws_o <- str_remove_all(ws, "_winsor")

winsor_13 <- 
  evs13 %>%
  select(ws, ws_o) %>%
  summarise_all(.funs = list(
    mmin = ~ min(x = ., na.rm = T),
    mmedian = ~ median(x = ., na.rm = T),
    mmean = ~ mean(x = ., na.rm = T),
    mmax = ~ max(x = ., na.rm = T)
  )) %>%
  pivot_longer(
    everything(),
    names_to = c(".value", "Wert"),
    names_sep = "_m"
  )

names(winsor_13)[!str_detect(names(winsor_13), "winsor")] <-
  paste0(
    names(winsor_13)[!str_detect(names(winsor_13), "winsor")],
    "_winsor_ohne"
  )

winsor_13 <- 
  winsor_13 %>%
  pivot_longer(
    -Wert_winsor_ohne,
    names_to = c(".value", "Wert2"),
    names_sep = "_winso"
  ) %>%
  # ohne Bedarfsgewichtung bei Nettoeinkommen (20.10.2021). 
  select(ws_o, -netto_oecd) %>%
  t()

winsor_18 <- evs18 %>%
  select(ws, ws_o) %>%
  summarise_all(.funs = list(
    mmin = ~ min(x = ., na.rm = T),
    mmedian = ~ median(x = ., na.rm = T),
    mmean = ~ mean(x = ., na.rm = T),
    mmax = ~ max(x = ., na.rm = T)
  )) %>%
  pivot_longer(
    everything(),
    names_to = c(".value", "Wert"),
    names_sep = "_m"
  )

names(winsor_18)[!str_detect(names(winsor_18), "winsor")] <-
  paste0(names(winsor_18)[!str_detect(names(winsor_18), "winsor")], 
         "_winsor_ohne")

winsor_18 <- winsor_18 %>%
  pivot_longer(
    -Wert_winsor_ohne,
    names_to = c(".value", "Wert2"),
    names_sep = "_winso"
  ) %>%
  # ohne Bedarfsgewichtung bei Nettoeinkommen (20.10.2021). 
  select(ws_o, -netto_oecd) %>%
  t()

wb <- createWorkbook()
addWorksheet(wb, "Winsorizing")
writeData(wb, "Winsorizing",
          paste0(
            "Vergleich winsorizeder Löhne mit nicht-winsorizeden Löhnen, ",
            "2013 und 2018"),
          startCol = 1, startRow = 1
)
writeData(wb, "Winsorizing", round(winsor_13, digits=2), 
          startCol = 1, startRow = 6, rowNames = T, colNames = F)
writeData(wb, "Winsorizing", round(winsor_18, digits=2),
          startCol = 2 + ncol(winsor_13), startRow = 6, colNames = F)
writeData(wb, "Winsorizing",
          t(c(rep("2013", ncol(winsor_13)), rep("2018", ncol(winsor_18)))),
          startRow = 3, startCol = 2, colNames = F
)
writeData(wb, "Winsorizing",
          t(rep(rep(c("Min", "Median", "Mean", "Max"), each = 2), 2)),
          startRow = 4, startCol = 2, colNames = F
)
writeData(wb, "Winsorizing",
          t(rep(c("mit Winsor", "ohne Winsor"), 8)),
          startRow = 5, startCol = 2, colNames = F
)

# Formatierung
addStyle(wb, sheet = "Winsorizing", 
        style = createStyle(numFmt = "0.00"), 
         rows = 6:(nrow(winsor_13)+6), 
         cols = 2:33, 
         gridExpand = T)

saveWorkbook(wb, 
             paste0("./Output/Tabellen für Berichte/", Sys.Date(), "/", 
             "Tab_2-1_Winsor_Vergleich.xlsx"), 
             overwrite = T)
