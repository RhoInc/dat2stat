
#' tab2word
#' 
#' This function accepts a table made in tib2tab and outputs a formatted word document
#'
#' @param table a table
#' @param file output file path
#' @param colwid column width
#' @param footer footer text
#' @param headnames list of strings for header column titles
#'
#' @import dplyr
#' @import officer
#' @import flextable
#' 
#' @return
#'
#' @examples
#' \dontrun{
#' tib <- dat2tib(data = cdisc_data, 
#'                model = lm(aval ~ arm + age + sex), 
#'                outcome = aval, 
#'                trt = arm, 
#'                nest = param, 
#'                tran='none')
#'                
#' tab <- tib2tab(mtib = tib, 
#'         stat_fmt = "{mean} ({sd})", 
#'         comp_fmt = "{estimate} ({lower_CL}, {upper_CL})")
#'         
#' tab2word(tab, 
#'          file = 'tabletest.docx', 
#'          footer = 'Table created with dat2stat R package', 
#'          colwid = 1)
#' }
#' 
#' @export
#' 
#' 
tab2word <- function(table, file, footer, colwid = 1, headnames = NA){
  
  n <- attributes(table)$ngroup
  
  if (is.na(headnames[1])){
  head <- c("sep1", "sep2", "sep3", names(table)) %>% tidy %>%
    mutate(names = c("", "", "", names(table)),
           colA = c('', '', '', names(table)[1], rep(attributes(table)$stat, n), rep(attributes(table)$comp, n), rep('p-value', n))
    )
  } else {
    
    head <- c("sep1", "sep2", "sep3", names(table)) %>% tidy %>%
      mutate(names = c("", "", "", names(table)),
             colA = c('', '', '', names(table)[1], rep(headnames[1], n), rep(headnames[2], n), rep(headnames[3], n))
      )
  }
  
   tab <- table %>%
    regulartable(col_keys = c(names(table)[1], 
                              'sep1', 
                              names(table)[2:(n+1)], 
                              'sep2', 
                              names(table)[(n+2):(2*n+1)], 
                              'sep3', 
                              names(table)[(2*n+2):(3*n+1)])) %>%
    set_header_df(mapping = head, key = "x") %>%
    fontsize(size = 7, part = 'all') %>%
    theme_zebra(odd_header = "transparent", even_header = 'transparent') %>%
    hline(border = fp_border(width = .5, color = "#007FA6"), part = "body" ) %>%
    hline(border = fp_border(width = 1.5, color = "#007FA6"), part = "header" ) %>%
    empty_blanks %>%
    merge_h(part = 'header') %>%
    merge_v(part = 'header') %>%
    add_footer(param = footer) %>%
    width(j = 1, 1.5) %>%
    width(j = c(2, (n+3), (2*n+4)), .1) %>%
    width(j = c(3:(n+2),  (n+4):(2*n+3), (2*n+5):(3*n+4)), colwid) %>%
    align(align = 'center', part = "header")
  
  doc <- read_docx() %>%
    body_add_par(value = " ", style = "Normal") %>%
    body_end_section_landscape() %>%
    body_add_flextable( value = tab) %>%
    body_add_par(value = "", style = "Normal") %>%
    body_end_section_landscape() %>%
    print(target = file)
  
}
