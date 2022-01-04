# 
# 22. plan.comp     
# 
plan.comp <- function(pep) {
  pep_sum <- pep %>% 
    summarise(id = unique(id),
              date = max(date, na.rm = T),
              year = max(year, na.rm = T),
              Np = sum(q_pep * 365/1000, na.rm = T),
              q = tail(q_pep, 1))
  
  # fpm results  
  
  fpm <- ftwv %>% 
    mutate(id = "fpm",
           date = date,
           year = year(date)) %>%
    group_by(year) %>%
    summarise(id = unique(id),
              date = max(date, na.rm = T),
              q_fpm = sum(qf/12, na.rm = T)) %>%
    ungroup() %>%
    filter(year >= 2019 & year < max(pep$year))
  
  fpm_sum <- fpm %>% 
    summarise(id = unique(id),
              date = max(date, na.rm = T),
              year = max(year, na.rm = T),
              Np = sum(q_fpm * 365/1000, na.rm = T),
              q = tail(q_fpm * 365/1000, 1))
  
  plan <- pep_sum %>% 
    full_join(fpm_sum, by = c("id", "date", "year", "Np", "q")) %>%
    mutate(across(where(is.numeric), round, 2))
  print.table(plan, "PEP-FPM Plan comparison", 2, "")
  
  p1 <- ftwv %>% filter(date <= max(pep$date)) %>%
    ggplot() + 
    geom_line(aes(date, qf, col = "qf")) +
    geom_line(aes(date, q, col = "q")) +
    geom_line(data = pep, 
              aes(date, q_pep, 
                  col = "q PEP"), 
              size = 1.2) +
    # update 
    theme(plot.subtitle =
            element_text(size = 12,
                         face = "bold",
                         color = "darkgreen"),
          plot.caption = 
            element_text(color = "blue",
                         size = 8,
                         face = "bold")) +
    labs(title = Title,
         subtitle = "Plan Comparison \nall units mb/d",
         x = "Time (years)",
         y = "Daily oil production (mb/d)",
         colour = "Legend:",
         caption = Caption) +
    scale_x_date(guide = guide_axis(check.overlap = TRUE)) +
    theme(legend.position = "top") 
  
  return(list(plan = plan,
              p1 = p1))
}
