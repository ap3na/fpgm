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
