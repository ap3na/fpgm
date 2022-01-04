## 17.2 fun_Qf.qf 
Qf.qf <- function(cwpd, nls_field, fpt) {
  ftwv <- tibble(
    # create t vector
    t = seq(from = min(cwpd$t, na.rm = T), 
            to = max(cwpd$t, na.rm = T) + fpt)) %>%  
    dplyr::mutate(
      date = seq.Date(from = as.Date(min(cwpd$date, na.rm = T)),
                      by = "months",
                      # create date for extended model
                      length.out = nrow(cwpd) + fpt),   
      days = lubridate::days_in_month(date),  
      # calculate Qf
      Qf = predict(nls_field, newdata = list(t = .$t)),  
      # calculate qf in bpd
      qf = ymmd * (Qf - lag(Qf, n = 1L))) %>% 
    # remove NA  check !!!!!
#    drop_na(qf) %>%
    dplyr::select(date, days, t, qf, Qf)
  
}
