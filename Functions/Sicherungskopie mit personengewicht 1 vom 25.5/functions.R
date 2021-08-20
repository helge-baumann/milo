# Anteil Personen in Kategorie als Anteil an anspruchsberechtigten Personen
sum_pers <- function(data, varname, lower, upper, w) {
  
  data <- data[, str_detect(names(data), varname)]
  #x wurde auf > lower anstelle >= lower geändert, sonst werden Stundenlöhne 0 mitgezählt
  #Haushaltsgewichte wurden als Näherung für Personengewichte eingefügt
  summe <- sum(sapply(data, function(x) sum(x > lower & x <= upper, na.rm=T))*w)
  gesamt <- sum(sapply(data, function(x) sum(!is.na(x) & x > 0))*w)
  return(round(summe / gesamt * 100, digits = 1))
  
}

# Anteil Haushalte mit mind. 1 Person in Kategorie als Anteil an 
# anspruchsberechtigten Haushalten

sum_hh <- function(data, varname, lower, upper, w) {
  
  data <- data[, str_detect(names(data), varname)]
  
  summe <- sum((rowSums(data > lower & data <= upper, na.rm = T) > 0)*w)
  gesamt <- sum((rowSums(!is.na(data[, str_detect(names(data), varname)])) > 0)*w)
  return(round(summe / gesamt * 100, digits = 1))
  
}