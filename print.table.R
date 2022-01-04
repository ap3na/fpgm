# 
# 21. print.table fun    
# 
# 
#' print.table function
#' @keywords print_table
#' @keywords caption
#' @keywords digit
#' @keywords notes
#' @export
#'

print.table <- function(print_table, caption, digit, notes)
{
  print_table %>%
    kable(label = caption,
          align = "c",
          digits = digit,
          caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover"),
                  position = "center", font_size = 16) %>%
    footnote(notes)
}
