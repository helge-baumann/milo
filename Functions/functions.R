
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

# Excel-Stil für Anmerkungen
style_anmerkung <- createStyle(
  textDecoration = "italic", fontColour="red"
)
