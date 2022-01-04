# 
# field model     
# 
field.model <- function() {
  # 1. Select wells field data
  wfd <- wells.field.data(wpd, Field)
  # 1.1 run field.analysis.date 
  wfd <- field.analysis.date(wfd, fad)
  # 16.1 Calculate Q by field
  Qbyfield <- Q.by.field.1(wfd)
  # 16.2 Calculate Q by field
  Qbyfield <- Q.by.field.2(Qbyfield)
  # 17. run nlr.lgm
  nls_field <- nlr.lgm(Qbyfield)
  # 17.2 run Qf.qf
  ftwv <- Qf.qf(Qbyfield, nls_field, fpt)
  # 18. run fill.var
  ftwv <- fill.var(ftwv, Qbyfield)
  # 19. run qe tail
  qe = qe.tail(ftwv$qf, 2)
  
  return(list(ftwv = ftwv,
              wqQf = wqQf,
              fapd = fapd,
              fdme = fdme,
              well_status_sum = well_status_sum,
              qe = qe))
}
