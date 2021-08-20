# Anteil Personen in Kategorie als Anteil an anspruchsberechtigten Personen
sum_pers <- function(data, varname, lower, upper, w) {
  
  data <- data[, str_detect(names(data), varname)]

    #x wurde auf > lower anstelle >= lower geändert, sonst werden Stundenlöhne 0 mitgezählt
    #Haushaltsgewichte wurden als Näherung für Personengewichte eingefügt
    #Code geändert, wegen Gewichten und Nichtbetrachtung von STundenlohn 0

  summe <- sum(w * rowSums(data > lower & data <= upper, na.rm = T))
  gesamt <- sum(w * (rowSums(data[, str_detect(names(data), varname)] > 0 & !is.na(data[, str_detect(names(data), varname)]))))
  #sum(sapply(data, function(x) sum(x > 0 & x <= Inf, na.rm=T))*w)
  return(round(summe / gesamt * 100, digits = 1))
  
}


# Anteil Haushalte mit mind. 1 Person in Kategorie als Anteil an 
# anspruchsberechtigten Haushalten

sum_hh <- function(data, varname, lower, upper, w) {
  
  data <- data[, str_detect(names(data), varname)]
  
  #Stundenlöhne 0 wurden bei den Anteilen herausgenommen
  
  summe <- sum(w * (rowSums(data > lower & data <= upper, na.rm = T) > 0))
  gesamt <- sum(w * (rowSums(!is.na(data[, str_detect(names(data), varname)])) > 0 & rowSums(data > 0, na.rm = T)))
  return(round(summe / gesamt * 100, digits = 1))
  
}

# Winsorizing

winsor <- function(x, lower = 1, upper = 99, weight=NULL) {
  
  if(is.null(weight)) {
    q <- quantile(x, probs=c(lower/100, upper/100), na.rm=T)
  } else {
    q <- wtd.quantile(x, w = weight, probs=c(lower/100, upper/100), na.rm=T)
  }
  x[x <= q[1]] <- q[1]
  x[x >= q[2]] <- q[2]
  
  return(x)
  
}
