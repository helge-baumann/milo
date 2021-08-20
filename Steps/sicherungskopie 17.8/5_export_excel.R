# Export to Excel

dir.create(paste0("./Output/Excel/", Sys.Date()), showWarnings=F)
   
bsl13 <- bsl13 %>% filter(!is.na(bsl_g))
bsl18 <- bsl18 %>% filter(!is.na(bsl_g))

for(b in c("bite", "bsl13", "bsl18", "Dezile", "HH", "HH_income", "HH_konsum",
           "HH_land", "HH_save", "konsumgüter", "konsumgüter_hh", "HH_konsum_EF73", "HH_konsum_EF76", "HH_konsum_EF82", "HH_konsum_EF85", "P", "bsl13_g", "bsl18_g", "bsl13_alle", "bsl18_alle"))    {
  
  
write.csv2(get(b), paste0("./Output/Excel/", Sys.Date(), "/", b, ".csv"))
           
}

