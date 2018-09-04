
#' tab2word
#' 
#' This function accepts a table and outputs a formatted word document
#'
#' @param table a table
#' @param file output file path
#' @param colwid column width
#' @param footer footer text
#'
#' @import dplyr
#' @import officer
#' @import flextable
#' 
#' @return
#' @export
#'
#' @examples
tab2word <- function(table, file, footer, colwid = 1){
    tab <- regulartable(table) %>%
    fontsize( size = 7, part = 'all') %>%
    theme_zebra(odd_header = "transparent", even_header = 'transparent') %>%
    hline(border = fp_border(width = .5, color = "#007FA6"), part = "body" ) %>%
    hline(border = fp_border(width = 1.5, color = "#007FA6"), part = "header" ) %>%
    width(j = 1, 1.5) %>%
    width(j = -1, colwid) %>%
    add_footer(param = footer)
  
  
  doc <- read_docx() %>%
    body_add_par(value = " ", style = "Normal") %>%
    body_end_section_landscape() %>%
    body_add_flextable( value = tab) %>%
    body_add_par(value = "", style = "Normal") %>%
    body_end_section_landscape() %>%
    print(target = file)
  
}
