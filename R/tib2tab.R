
#' tib2tab
#' 
#' This function accepts a master tibble made in dat2tib and creates a summary table with one row per outcome variable 
#' and summary statistics by treatment arms.
#'
#' @param mtib A master tibble
#' @param stat_fmt Format for summary statistics by arm in glue syntax
#' @param comp_fmt Format for pairwise comparison statistics in glue syntax
#' @param adjust a logical value indicating whether summary statistics should be adjusted or unadjusted by model covariates
#'
#' @return
#' 
#' 
#' @import dplyr
#' @importFrom glue str_glue
#' @importFrom purrr map
#' @importFrom pixiedust pval_string
#' 
#'
#' @examples
#' @export
#' 
#' 

#this version is user specified format

# tib2tab <- function(mtib, stat_fmt, comp_fmt, adjust = F){
#   
#   stat_vars <- stat_fmt %>%
#     str_match_all("[{](.*?)[:]") %>% .[[1]] %>% .[,2]
#   stat_vars_sym <- stat_vars %>% syms
#   
#   comp_vars <- comp_fmt %>%
#     str_match_all("[{](.*?)[:]") %>% .[[1]] %>% .[,2]
#   
#   if(adjust == F){
#     mtab <- mtib %>%
#       mutate(summ = map(summ, ~.x %>%
#                           as_data_frame %>%
#                           filter(stat %in% stat_vars_sym) %>%
#                           select(arm, stat, value) %>%
#                           spread(stat, value) %>%
#                           mutate(means = glue_fmt(stat_fmt)) %>%
#                           select(-c(!!! stat_vars_sym)) %>%
#                           spread(arm, means))) %>%
#        mutate(p = map(comp, ~ .x %>%
#                        unite(diff_name, level1, level2, sep = "v. ")  %>%
#                        mutate(diff_name = str_glue("{diff_name} p-value"),
#                               pval = str_glue("{format_pval(p.value, 3, equal = FALSE)}"))  %>%
#                        select(diff_name, pval)  %>%
#                        spread(diff_name, pval)),
#               comp = map(comp, ~ .x %>%
#                           unite(diff_name, level1, level2, sep=" vs. ") %>%
#                           select(diff_name, comp_vars) %>%
#                           mutate(diff = glue_fmt(comp_fmt)) %>%
#                           select(diff_name, diff) %>%
#                           spread(diff_name, diff) ))%>%
#       select(-c(data, mod, ref, emm)) %>%
#       unnest
#   } else if (adjust == T){
#     mtab <- mtib %>%
#       mutate(emm = map(emm, ~.x %>%
#                          select(arm, stat_vars) %>%
#                          mutate(means = glue_fmt(stat_fmt)) %>%
#                          select(-c(!!! stat_vars_sym)) %>%
#                          spread(arm, means)),
#              p = map(comp, ~ .x %>%
#                        unite(diff_name, level1, level2, sep = "v. ")  %>%
#                        mutate(diff_name = str_glue("{diff_name} p-value"),
#                               pval = str_glue("{format_pval(p.value, 3, equal = FALSE)}"))  %>%
#                        select(diff_name, pval)  %>%
#                        spread(diff_name, pval)),
#              comp = map(comp, ~ .x %>%
#                           unite(diff_name, level1, level2, sep=" vs. ") %>%
#                           select(diff_name, comp_vars) %>%
#                           mutate(diff = glue_fmt(comp_fmt)) %>%
#                           select(diff_name, diff) %>%
#                           spread(diff_name, diff) )) %>%
#       select(-c(data, mod, ref, summ)) %>%
#       unnest
#   }
#   
#   return(mtab)
#   
# }


#this version is using our autoformat function


tib2tab <- function(mtib, stat_fmt, comp_fmt, adjust = F){

  stat_vars <- stat_fmt %>%
    str_match_all("[{](.*?)[}]") %>% .[[1]] %>% .[,2]
  stat_vars_sym <- stat_vars %>% syms

  comp_vars <- comp_fmt %>%
    str_match_all("[{](.*?)[}]") %>% .[[1]] %>% .[,2]

  if(adjust == F){
    mtab <- mtib %>%
      mutate(summ = map(summ, ~.x %>%
                          mutate_at(vars(-1), funs(format_value_auto(.))) %>%
                          mutate(means = str_glue(stat_fmt)) %>%
                          select(arm, means) %>%
                          spread(arm, means))) %>%
       mutate(p = map(comp, ~ .x %>%
                      mutate_at(vars(-c(1, p_value)), funs(format_value_auto(.))) %>%
                       mutate(contrast = str_glue("{contrast} p-value"),
                              pval = str_glue("{pixiedust::pval_string(p_value)}"))  %>%
                       select(contrast, pval)  %>%
                       spread(contrast, pval)),
              comp = map(comp, ~ .x %>%
                          mutate_at(vars(-c(1, p_value)), funs(format_value_auto(.))) %>%
                          select(contrast, comp_vars) %>%
                          mutate(diff = str_glue(comp_fmt)) %>%
                          select(contrast, diff) %>%
                          spread(contrast, diff) ))%>%
      select(-c(data, mod, ref, emm, emm_summ)) %>%
      unnest
  } else if (adjust == T){
    mtab <- mtib %>%
      mutate(emm_summ = map(emm_summ, ~.x %>%
                         mutate_at(vars(-1), funs(format_value_auto(.))) %>%
                         select(arm, stat_vars) %>%
                         mutate(means = str_glue(stat_fmt)) %>%
                         select(arm, means) %>%
                         spread(arm, means)),
             p = map(comp, ~ .x %>%
                       mutate_at(vars(-c(1, p_value)), funs(format_value_auto(.))) %>%
                       mutate(contrast = str_glue("{contrast} p-value"),
                              pval = str_glue("{pixiedust::pval_string(p_value)}"))  %>%
                       select(contrast, pval)  %>%
                       spread(contrast, pval)),
             comp = map(comp, ~ .x %>%
                          mutate_at(vars(-c(1, p_value)), funs(format_value_auto(.))) %>%
                          select(contrast, comp_vars) %>%
                          mutate(diff = str_glue(comp_fmt)) %>%
                          select(contrast, diff) %>%
                          spread(contrast, diff) )) %>%
      select(-c(data, mod, ref, summ, emm)) %>%
      unnest
  }

  return(mtab)

}