## 12.7 fun_well.status.sum
well.status.sum <- function(well_status) {
  well_status_sum <- well_status %>% 
    group_by(Reserves.Status) %>%
    summarise(Wells = n(), 
              Production = sum(Q, na.rm = T),       
              Forecast.Production = sum(Qf, na.rm = T),               
              Reserves = sum(R, na.rm = T))  %>%
    mutate(across(where(is.numeric), round, 1))  
  return(well_status_sum)
}
