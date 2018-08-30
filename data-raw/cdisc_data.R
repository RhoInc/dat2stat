library(dplyr)

adsl <- read.csv("data-raw/ADSL.csv") %>% 
  setNames(., tolower(names(.))) %>% 
  select(usubjid, arm, age, sex, race)

adlbc <- read.csv("data-raw/ADLBC.csv") %>%
  setNames(., tolower(names(.))) %>%
  select(usubjid, param, visit, visitnum, aval, base) %>%
  filter(param %in% c( "Sodium (mmol/L)",
                       "Potassium (mmol/L)",
                       "Chloride (mmol/L)",
                       "Bilirubin (umol/L)",
                       "Alkaline Phosphatase (U/L)",
                       "Gamma Glutamyl Transferase (U/L)",
                       "Alanine Aminotransferase (U/L)",
                       "Aspartate Aminotransferase (U/L)",
                       "Blood Urea Nitrogen (mmol/L)",
                       "Creatinine (umol/L)",
                       "Urate (umol/L)",
                       "Phosphate (mmol/L)",
                       "Calcium (mmol/L)",
                       "Glucose (mmol/L)",
                       "Protein (g/L)",
                       "Albumin (g/L)",
                       "Cholesterol (mmol/L)",
                       "Creatine Kinase (U/L)") &
           visitnum %in% c(1:13))

### bring all together & merge w adsl
cdisc_data <- left_join(adsl, adlbc) %>%
  mutate(aval_log10 = log10(aval+0.1))

### output
devtools::use_data(cdisc_data, overwrite = TRUE)
