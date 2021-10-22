# Vorab: Zusammenfassung evs13 und evs18 in gepoolter "EVS"

EVS <- rbind(
  evs13 %>%
    mutate(gewicht = EF107*(nrow(evs13)/sum(evs13$EF107)), 
           westost = EF1, bland = EF2U1, ID = EF2U2,
           welle = "2013") %>%
    select(ID, welle, gewicht, westost, bland, EF38, EF35, EF107, EF1,
           paste0("EF", 8:13, "U2"),
           starts_with(
             c("konsum", "cons", "c_imp", "save", "lohn", "monat", "basis", 
               "az","stunde", "unterster", "milo_hh", "netto", "brutto", 
               "hhtype", "oecd", "AN", "ET", "sex", "vollzeit", "minijob")))
    ,
  evs18 %>%
    mutate(gewicht = EF107*(nrow(evs18)/sum(evs18$EF107)), 
           westost = EF1, bland = EF2U1, ID = EF2U2, 
           welle = "2018") %>%
    select(ID, welle, gewicht, westost, bland, EF38, EF35, EF107, EF1,
           paste0("EF", 8:13, "U2"),
           starts_with(
             c("konsum", "cons", "c_imp", "save", "lohn", "monat", "basis", 
               "az", "stunde", "unterster", "milo_hh", "netto", "brutto", 
               "hhtype", "oecd", "AN", "ET", "sex", "vollzeit", "minijob")))
) 

