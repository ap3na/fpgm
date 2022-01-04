## 12.4 fun_well.status
well.status <- function(well_data, fdad) {
  
  #     - 1 producing reserves (producing wells: pw)  
  #         - actual production well date = actual production field date   
  #         - Qf > Q  
  #         - Q > 0  
  #     - 2 non-producing reserves (shut-in wells: siw) 
  #         - actual production well date < actual production field date
  #         - Qf > Q
  #         - Q > 0  
  #     - 3 non-producing reserves (behind pipe wells: bhw)  
  #         - multiples production cycles  (pending)
  #     - 4 Model  
  #         - with models  
  #         - without models         
  
  # spwd: start production well date      
  # apwd: actual production well date 
  # apfd: actual production field date
  # fdad: field date actual date
  # producing wells: pw
  # non-producing reserves (shut-in wells: siw) 
  # change R per Proved Developed Reserves: PDR  
  well_status <- well_data %>% 
    dplyr::group_by(well_name) %>%
    dplyr::summarise(spwd = min(date, na.rm = T),
                     apwd = max(date, na.rm = T),
                     Q = max(Q, na.rm = T),
                     Qf = max(Qf, na.rm = T),
                     Qf = if_else(Qf < 0, 0, Qf)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(R = if_else(Qf < 0, 0, Qf),  
                  R = if_else(Qf == 0, 0, Qf - Q),
                  R = if_else(R < 0, 0, R),
                  Reserves.Status = 
                    if_else(apwd == fdad & Qf > Q & Q > 0, "1.Producing", 
                            if_else(apwd < fdad & Qf > Q & Q > 0, 
                                    "2.Non-producing", 
                                    if_else(Qf != 0, "3.Model", 
                                            "4.Non-model")))) %>%
    mutate(across(where(is.numeric), round, 2))
  return(well_status)
}
