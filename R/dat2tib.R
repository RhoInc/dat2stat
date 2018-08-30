#' dat2tib
#' 
#' This function accepts data, grouping, and model specifications from the user and produces a Master Tibble.  The Master
#' Tibble contains summary statistics, estimated marginal means, and contrasts for all possible combinations of nest_var and trt.
#'
#' @param data A data frame
#' @param model Full modeling function
#' @param outcome Outcome variable used in the model
#' @param trt Treatment arm or grouping variable used in the model
#' @param tran Specify how outcome was previously transformed prior to modeling 
#' @param nest_var Grouping variable to be used for the data nesting
#'
#' @return
#' 
#' @examples 
#' 
#' @importFrom skimr skim
#' @import emmeans
#' @import dplyr
#' @import tidyr
#' @importFrom purrr map
#' @import rlang
#' 
#' @export
dat2tib <- function(data, model, outcome, trt,
                         tran = NULL,  
                         nest_var){
  # ci_level_mod = 0.95,
  # ci_level_contrast = 0.95,
  # adjust = "none"){
  
  
  model <- enexpr(model)
  outcome <- enquo(outcome)
  trt <- enquo(trt)
  trt_string <- quo_text(trt)
  
  nest_var <- enquo(nest_var)  ## capture the nesting var as a quosure
  d <- data %>% group_by(!!nest_var) %>% nest() ## nest by the supplied nesting var
  
  d_mt <- d %>%
    mutate(summ = map(data, ~ .x %>%
                        group_by(!!trt) %>%
                        skimr::skim(!!outcome) %>%
                        select(!!trt, stat, value) %>%
                        spread(stat, value) %>%
                        select(!!trt, complete, missing, n, mean, sd, p0, p25, p50, p75, p100) %>%
                        remove_rownames(.) %>%
                        as.data.frame(.)),
           mod = map(data, ~ with(., !! model)),
           ref = case_when(
             is.null(tran) ~ map(mod, ~ ref_grid(.,
                                                 type = "response")),
             TRUE ~ map(mod, ~ ref_grid(.,
                                        type = "response") %>%
                          update(tran = tran))),
           emm = map(ref,  ~ emmeans(., ~ !!trt)),
           emm_summ = map(emm, ~ summary(.,
                                         level = 0.95,
                                         adjust = "none",
                                         type = "response",
                                         infer = TRUE)  %>%
                            as.data.frame(.) %>%
                            setNames(., c(trt_string, "estimate", "SE","df","lower_CL","upper_CL","t_ratio","p_value"))),
           comp = map(emm,  ~ contrast(.,
                                       method='pairwise') %>%
                        summary(., level = 0.95,
                                adjust = "none",
                                type = "response",
                                infer = TRUE)  %>%
                        as.data.frame(.) %>%
                        setNames(., c("contrast", "estimate", "SE","df","lower_CL","upper_CL","t_ratio","p_value")))
    )
  
  class(d_mt) <- append(class(d_mt), "masterTibble")
  
  return(d_mt)
  
}

