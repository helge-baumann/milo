# Deflationierung

defl <- defl %>%
  mutate(d2013 = V3 * 1/V4) # 2018 als Referenz (100) setzen

EVS_defl <- EVS %>% 
  mutate(
    gesamt = "alle", 
    lohn_haushalt = if_else(lohn_empfänger == 0, "HH ohne Lohnberechnung", 
                            "HH mit Lohnberechnung"),
    ET_hh = as_factor(rowSums(!is.na(.[paste0("ET_", 1:6)]))),
    # Deflationierung
    lohn_summe_winsor = if_else(
      welle == "2013", lohn_summe_winsor * 1/defl$d2013[13], lohn_summe_winsor),
    brutto_winsor = if_else(
      welle == "2013", brutto_winsor * 1/defl$d2013[13], brutto_winsor),
    netto_oecd_winsor = if_else(
      welle == "2013", netto_oecd_winsor * 1/defl$d2013[13], netto_oecd_winsor),
    cons_winsor = if_else(
      welle == "2013", cons_winsor * 1/defl$d2013[13], cons_winsor),
    c_imp_winsor = if_else(
      welle == "2013", c_imp_winsor * 1/defl$d2013[13], c_imp_winsor),
    save_winsor = if_else(
      welle == "2013", save_winsor * 1/defl$d2013[13], save_winsor),
    save_imp_winsor = if_else(
      welle == "2013", save_imp_winsor * 1/defl$d2013[13], save_imp_winsor),
    konsum1_winsor = if_else(
      welle == "2013", konsum1_winsor * 1/defl$d2013[1], konsum1_winsor),
    konsum2_winsor = if_else(
      welle == "2013", konsum2_winsor * 1/defl$d2013[2], konsum2_winsor),
    konsum3_winsor = if_else(
      welle == "2013", konsum3_winsor * 1/defl$d2013[3], konsum3_winsor),
    konsum4_winsor = if_else(
      welle == "2013", konsum4_winsor * 1/defl$d2013[4], konsum4_winsor),
    konsum6_winsor = if_else(
      welle == "2013", konsum6_winsor * 1/defl$d2013[5], konsum6_winsor),
    konsum8_winsor = if_else(
      welle == "2013", konsum8_winsor * 1/defl$d2013[6], konsum8_winsor),
    konsum10_winsor = if_else(
      welle == "2013", konsum10_winsor * 1/defl$d2013[7], konsum10_winsor),
    konsum11_winsor = if_else(
      welle == "2013", konsum11_winsor * 1/defl$d2013[8], konsum11_winsor),
    konsum12_winsor = if_else(
      welle == "2013", konsum12_winsor * 1/defl$d2013[9], konsum12_winsor),
    konsum5new_winsor = if_else(
      welle == "2013", konsum5new_winsor * 1/defl$d2013[10], konsum5new_winsor),
    konsum7new_winsor = if_else(
      welle == "2013", konsum7new_winsor * 1/defl$d2013[11], konsum7new_winsor),
    konsum9new_winsor = if_else(
      welle == "2013", konsum9new_winsor * 1/defl$d2013[12], konsum9new_winsor)
  ) 
