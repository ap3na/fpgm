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
