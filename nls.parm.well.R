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
