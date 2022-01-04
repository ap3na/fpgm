## 18. fun_fill.var 
fill.var <- function(ftwv, cwpd) {
  date <- q <- qf <- t <- days <- Q <- Qf <- NULL
  ftwv %>% dplyr::full_join(cwpd, by = c("t", "date")) %>%
    dplyr::arrange(date) %>%
    dplyr::select(date, q, qf, Q, Qf, t, days) %>%
    fill(q, .direction = "up") %>%
    fill(qf, .direction = "down") %>%
    fill(Q, .direction = "up") %>%
    fill(Qf, .direction = "down") %>%
    fill(qf, .direction = "down") %>%    
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::mutate(t = 1:n()) %>%  
    dplyr::select(date, days, t, q, qf, Q, Qf)
}
