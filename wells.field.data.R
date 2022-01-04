## 1. fun_wells.field.data
wells.field.data <- function(data, Field) 
{
  data %>%
    dplyr::filter(field %in% Field) %>%
    dplyr::mutate(date = lubridate::ymd(date))  %>%
    dplyr::select(field, well_name, date, year, 
                  month, days, q, w) %>%
    dplyr::arrange(well_name, date)
}
