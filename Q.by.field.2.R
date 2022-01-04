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
