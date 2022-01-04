# 
# 20. summarize model  
# 
summarize.model <- function(ftwv, wqQf) {
  
  # create wqQf summary
  wqQf_sum <- wqQf %>% 
    group_by(well_name) %>% 
    summarise(Qf = max(Qf, na.rm = T),
              Q = max(Q, na.rm = T),
              PR = Qf - Q) %>%
    ungroup() %>%
    summarise(Qf = sum(Qf, na.rm = T),
              Q = sum(Q, na.rm = T),
              PR = Qf - Q)
  
  
  # create ftwv summary
  ftwv_sum <- ftwv %>% 
    summarize(Q = max(Q, na.rm = T),
              Qf = max(Qf, na.rm = T),
              PR = Qf - Q)
  
  
  st <- tibble(Id = c(1:3),
               Parameters = 
                 c(paste("Production", "@", fapd),
                   paste("Forecast Production", "@", fdme),       
                   "Proved Developed Reserves"),
               byField = 
                 c(ftwv_sum$Q,
                   ftwv_sum$Qf, 
                   ftwv_sum$PR
                 ),  
               byWells = 
                 c(wqQf_sum$Q, 
                   wqQf_sum$Qf, 
                   wqQf_sum$PR))
  st
  
  return(st)
}
