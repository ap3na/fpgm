wells.field.model <- function(wfd) {
  
  # print data table function  
  print.DT <- function(data, pagelength, caption, digits) {
    
    data <- data %>% mutate(across(where(is.numeric), round, digits))
    
    datatable(data, 
              filter = 'top', 
              caption = caption,
              options = list(
                pageLength = pagelength, 
                autoWidth = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$('body').css({'font-family': 'Arial'});",
                  "}"
                )))
  }
  
  ## 1. fun_wells.field.data
#  wells.field.data <- function(data, Field) 
#  {
#    data %>%
#      dplyr::filter(field %in% Field) %>%
#      dplyr::mutate(date = lubridate::ymd(date))  %>%
#      dplyr::select(field, well_name, date, year, 
#                    month, days, q, w) %>%
#      dplyr::arrange(well_name, date)
#  }
  
  ## 1.2 fun_field.analysis.date 
  field.analysis.date <- function(wfd, fad) {
    # fad: field analysis date  
    wfd %>% filter(date <= fad)
  }
  
  ## 2. fun_Qbywells  
  Q.by.wells <- function(data) {
    
    well_name <- date <- field <- q <-  days <- NULL
    qm <- n <- month <- year <- Q <- NULL
    data %>%
      dplyr::group_by(well_name) %>%
      dplyr::mutate(
        qm = q * days,
        # divide Q by 1000 to convert thousands to millions
        Q = cumsum(qm) / 1000) %>%
      # calculate time vector
      dplyr::mutate(t = 1:n()) %>%
      dplyr::select(
        field, date, t, days, month,
        year, well_name, q, Q)
  }
  
  ## 3. fun_create.well.list 
  # 3. create list of wells
  create.well.list <- function(data) {
    well_name <- date <- t <- Q <- q <- NULL
    data %>%
      dplyr::select(well_name, date, t, q, Q) %>%
      dplyr::group_by(well_name) %>%
      dplyr::group_nest()
  }
  
  ## 5. map.nlr.lgm  
  
  ## 5.1 fun_lgm
  lgm <- function(t, K, n, a) {
    (K * t ^ n) / (a + t ^ n)
  }
  
  ## 5.2 fun_nlr.lgm 
  nlr.lgm <- function(data) {
    minpack.lm::nlsLM(
      Q ~ lgm(t, K, n, a),
      start = c(K = max(data$Q, na.rm = T),
                a = max(data$Q, na.rm = T),
                n = 1),
      control = c(nls.control(maxiter = 1024)),
      trace = F,
      model = TRUE,
      data = data)
  }
  
  ## 5.3 fun_map.nlr.lgm
  map.nlr.lgm <- function(data) {
    data %>% 
      mutate(nls_fit = map(data, ~possibly(~nlr.lgm(.x),
                                           otherwise = NA,
                                           quiet = TRUE)(.x)))
  }
  
  ## 7. fun_nls.plot 
  nls.plot <- function(data) {
    data %>%
      dplyr::select(well_name, data) %>%
      unnest(data)
  }
  
  ## 8. fun_well.predict.time  
  well.predict.time <- function(nlsp, fpt) {
    nlsp %>% 
      group_by(well_name) %>%
      summarise(t = seq(1:(max(t) + fpt)))          
  }
  
  ## 10. fun_well.wQf.value 
  well.wQf.value <- function(wpt, wl) {
    well_name <- t <- NULL
    wpt %>%
      dplyr::group_by(well_name) %>%
      nest(preds = t) %>%
      dplyr::left_join(select(wl, well_name, nls_fit),
                       by = "well_name") %>%
      dplyr::mutate(
        preds = purrr::map2(preds, nls_fit,
                            ~broom::augment(.y, newdata = .x))) %>%
      dplyr::select(-nls_fit) %>%
      dplyr::ungroup() %>%
      unnest(cols = preds)
  }
  
  ## 11. fun_well.wqQf.value
  well.wqQf.value <- function(wQf, nlsp) {
    dplyr::full_join(wQf, nlsp, by = c("well_name", "t")) %>%
      dplyr::arrange(well_name, t) %>%
      dplyr::rename(Qf = .fitted) %>%
      dplyr::mutate(qf = lead(Qf, n = 1L, default = 0) - Qf,
                    qf = ymmd * qf,
                    qf = replace(qf, which(qf < 0), NA)) %>%
      dplyr::select(well_name, date, t, q, qf, Q, Qf)
  }
  
  # C. Wells Status 
  
  ## 12. fun_field.actual.production.date
  field.actual.production.date <- function(data) {
    # fapd: field actual production date 
    # function to calculate field actual production date     
    fapd = max(data$date, na.rm = T)
  }
  
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
  
  # D. Date Extend  
  
  ## 13. fun_date.complete 
  date.complete <- function(x) {
    x = seq.Date(from = ymd(fapd) %m+% months(1),                  
                 by = "month", 
                 length.out = fpt)
  }
  
  ## 13.1 fun_date.extend   
  date.extend <- function(well_data) {
    b1 <- well_data %>%
      group_by(well_name) %>%
      filter(is.na(date)) %>%
      select(well_name, date, t, Qf, qf)
    
    b2 <- well_data %>% 
      group_by(well_name) %>%
      filter(is.na(date)) %>%
      select(well_name, date, t) %>%
      mutate(date = date.complete(date))
    
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
  
  ## 13.4 fun_field.date.model.extend
  field.date.model.extend <- function(data) {
    # fdme: field.date.model.extend 
    # function to calculate field actual production date     
    fdme = max(data$date, na.rm = T)
  }
  
  ## 14. fun_nls.parm.well
  nls.parm.well <- function(wl) {
    
    nls_well_glance <- wl %>%
      dplyr::mutate(
        results = 
          purrr::map(nls_fit, ~possibly(~glance(.x),
                                        otherwise = NA,
                                        quiet = TRUE)(.x)))  %>%
      dplyr::select(-data, -nls_fit) %>%
      unnest(results)
    
    nls_well_tidy <- wl %>%
      dplyr::mutate(
        results = purrr::map(nls_fit, ~possibly(~tidy(.x),
                                                otherwise = NA,
                                                quiet = TRUE)(.x)))  %>%
      dplyr::select(-data, -nls_fit) %>%
      unnest(results)
    
    bar <- nls_well_tidy %>%
      tidyr::pivot_wider(names_from = term,
                         values_from  = estimate) %>%
      dplyr::select(well_name, K, a, n)
    
    K <- bar %>%
      dplyr::select(well_name, K) %>%
      drop_na()
    
    a <- bar %>%
      dplyr::select(well_name, a) %>%
      drop_na()
    
    n <- bar %>%
      dplyr::select(well_name, n) %>%
      drop_na()
    
    nls_wells_parm <- dplyr::full_join(K, a, 
                                       by = "well_name")
    nls_wells_parm <- dplyr::full_join(nls_wells_parm, n, 
                                       by = "well_name")
    nls_wells_parm <- dplyr::full_join(nls_wells_parm, nls_well_glance, 
                                       by = "well_name")
  }
  
  well.model <- function() {
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
    wqQf <- date.extend(wqQf) 
    # 14. run field.date.model.extend
    fdme <- field.date.model.extend(wqQf)
    # 15. calculate nls model parameters for each well 
    nlspw <- nls.parm.well(wl)
    # 16. run well.status.sum   
    well_status_sum <- well.status.sum(well_status)
    
  }
  #
  #        Run Model by wells
  #   
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
  # 
  wqQf <- wqQf %>%
    group_by(well_name) %>%
    filter(!any(well_name == "CUITLAHUAC-682")) %>%
    ungroup()
  
  well_status <- well.status(wqQf, fapd)
  # 13. run date.extend function
  wqQf <- date.extend(wqQf) 
  # 14. run field.date.model.extend
  fdme <- field.date.model.extend(wqQf)
  # 15. calculate nls model parameters for each well 
  nlspw <- nls.parm.well(wl)
  # 16. run well.status.sum   
  well_status_sum <- well.status.sum(well_status)
  
  #
  # 16 Field Model
  #  
  
  ## 16.1 fun_Q.by.field.1
  Q.by.field.1 <- function(well_data) {
    well_data %>%
      dplyr::group_by(year, month) %>%  # group by year & months
      dplyr::summarise(
        date = unique(date),         # extract date
        qm = q * days) %>%           # calculate production by months
      dplyr::ungroup()
  }
  
  ## 16.2 fun_Q.by.field.2
  Q.by.field.2 <- function(Qbyfield) {
    Qbyfield %>%
      dplyr::group_by(year, month) %>%  # group by year & months
      dplyr::summarise(
        date = unique(date),            # extract date
        q = sum(qm, na.rm = T)) %>%     # calculate q by year & months
      dplyr::ungroup() %>%
      dplyr::mutate(
        Q = cumsum(q) / 1000,           # convert to millions
        q = q /ymmd,                    # convert to bpd
        t = 1:n()) %>%                  # create t vector
      dplyr::select(date, year, month, t, q, Q)
  }
  
  ## 17. fun_nlr.lgm
  nlr.lgm <- function(data) {
    minpack.lm::nlsLM(
      Q ~ lgm(t, K, n, a),
      start = c(K = max(data$Q, na.rm = T),
                a = max(data$Q, na.rm = T),
                n = 1),
      control = c(nls.control(maxiter = 1024)),
      trace = F,
      model = TRUE,
      data = data)
  }
  
  ## 17.2 fun_Qf.qf 
  Qf.qf <- function(cwpd, nls_field, fpt) {
    ftwv <- tibble(
      # create t vector
      t = seq(from = min(cwpd$t), to = max(cwpd$t) + fpt)) %>%  
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
      drop_na(qf) %>%
      dplyr::select(date, days, t, qf, Qf)
  }
  
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
  
  ## 19. fun_qe.tail
  qe.tail <- function(qf, digit) {
    round(tail(qf, 1), digit)
  }
#
#        Run Model by field
#    
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
  qe = qe.tail(ftwv$qf, 1)
  
  return(list(ftwv = ftwv,
              wqQf = wqQf,
              fapd = fapd,
              fdme = fdme,
              well_status_sum = well_status_sum,
              qe = qe))
} 
