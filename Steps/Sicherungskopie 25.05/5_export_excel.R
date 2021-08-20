# Export to Excel

dir.create(paste0("./Output/Excel/", Sys.Date()), showWarnings=F)
   
bsl13 <- bsl13 %>% filter(!is.na(bsl_g))
bsl18 <- bsl18 %>% filter(!is.na(bsl_g))

for(b in c("bite", "bsl13", "bsl18", "Dezile", "HH", "HH_income", "HH_konsum",
           "HH_land", "HH_save", "konsumgüter", "konsumgüter_hh", "P", "bsl13_g", "bsl18_g"))    {
  
  
write.csv2(get(b), paste0("./Output/Excel/", Sys.Date(), "/", b, ".csv"))
           
}
