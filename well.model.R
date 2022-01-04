#
# well model function  
#
well.model <- function(wpd, fad, fpt) {
  # 1. Select wells field data
  wfd <- wells.field.data(wpd, Field)
  # 1.1 run field.analysis.date
  wfd <- field.analysis.date(wfd, fad)  
  # 2. calculate Q by wells
  Qbywells <- Q.by.wells(wfd)
  # 3. create list of wells
  wl <- create.well.list(Qbywells)
  # 4. calculate number of wells before model
  nwells_b4_nsl = nrow(wl)
  # 5. run map non-linear regression to logistic growth model
  wl <- map.nlr.lgm(wl)
  # 6. remove wells model with failed models NA
  wl <- wl[!is.na(wl$nls_fit), ]
  # 7. calculate number of wells after model
  nwells_after_nsl = nrow(wl)
  # 8. create nls plots object
  nlsp <- nls.plot(wl)
  # 9. calculate well predict time
  wpt <- well.predict.time(nlsp, fpt)
  # 10. calculate well production Qf values
  wQf <- well.wQf.value(wpt, wl)
  # 11. calculate well production qf values
  wqQf <- well.wqQf.value(wQf, nlsp)
  # 11.1 run field.actual.production.date 
  fapd <- field.actual.production.date(wqQf)
  # 12. run well.status
  well_status <- well.status(wqQf, fapd)
  # 13. run date.extend function
  wqQf <- date.extend(wqQf, date) 
  # 14. run field.date.model.extend
  fdme <- field.date.model.extend(wqQf)
  # 15. calculate nls model parameters for each well 
  nlspw <- nls.parm.well(wl)
  # 16. run well.status.sum   
  well_status_sum <- well.status.sum(well_status)
  # 17. output   
  return(list(wfd = wfd,
              wqQf = wqQf,
              fapd = fapd,
              fdme = fdme,
              well_status_sum = well_status_sum,
              qe = qe))
}
