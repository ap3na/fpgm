## 13.1 fun_date.extend   
date.extend <- function(well_data, date) {
  b1 <- well_data %>%
    group_by(well_name) %>%
    filter(is.na(date)) %>%
    select(well_name, date, t, Qf, qf)
  #    select(well_name, date, t, Qf, qf, Reserves.Status)
  
  b2 <- well_data %>% 
    group_by(well_name) %>%
    filter(is.na(date)) %>%
    select(well_name, date, t) %>%
    mutate(date = date.complete(fapd, fpt))
  
  b3 <- b1 %>% 
    full_join(b2, by = c("well_name", 
                         "date", "t")) %>% 
    arrange(well_name, t) %>%
    fill(date, .direction = "up") %>%
    drop_na(Qf)
  
  b4 <- well_data %>% drop_na(date)
  
  well_data <- b3 %>% 
    full_join(b4, by = c("well_name", "date", "t", 
                         "Qf", "qf")) %>% 
    #                              "Qf", "qf", "Reserves.Status")) %>%          
    arrange(well_name, t)  %>%
    dplyr::select(well_name, date, t, q, qf, Q, Qf)    
  return(well_data)
}
