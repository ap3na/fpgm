#
# 13. fun_date.complete 
#
date.complete <- function(fapd, fpt) {
  x = seq.Date(from = ymd(fapd) %m+% months(1), 
               by = "month", 
               length.out = fpt)
  return(x)
}
