
#' tab2word
#' 
#' This function accepts a table and outputs a formatted word document
#'
#' @param table a table
#' @param file output file path
#'
#' @import dplyr
#' @import officer
#' @import flextable
#' 
#' @return
#' @export
#'
#' @examples
tab2word <- function(table, file){
    tab <- regulartable(table) %>%
    fontsize( size = 7, part = 'all') %>%
    theme_zebra(odd_header = "transparent", even_header = 'transparent') %>%
    hline(border = fp_border(width = .5, color = "#007FA6"), part = "body" ) %>%
    hline(border = fp_border(width = 1.5, color = "#007FA6"), part = "header" ) %>%
    width(j = 1, 1.5) %>%
    width(j = -1, 1) %>%
    add_footer(param = 'footer test')
  
  
  doc <- read_docx() %>%
    body_add_par(value = " ", style = "Normal") %>%
    body_end_section_landscape() %>%
    body_add_flextable( value = tab) %>%
    body_add_par(value = "", style = "Normal") %>%
    body_end_section_landscape() %>%
    print(target = file)
  
}
