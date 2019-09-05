

#'--- 
#' title: "Tableau competition - EDA"
#' author: "Nayef Ahmad"
#' date: "2019-09-04"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---
#'

#+ lib, include = FALSE 
library(tidyverse)
library(janitor)
library(kableExtra)
library(DT)
library(here)

#+ rest 

# read data -----

df1.raw_data <- read_csv(here::here("data", 
                                    "WCTHUG Healthy Viz Competition Guidelines.csv"), 
                         na = c("", "(null)"))

str(df1.raw_data)
summary(df1.raw_data)


# make modifications
df2.modified <- 
  df1.raw_data %>% 
  
  # all chars to factors 
  mutate_if(is.character, 
            as.factor) %>%
  
  # specific numerics to factors: 
  mutate_at(vars(contains("dt_tm")), 
            lubridate::mdy_hms) %>% 
  mutate_at(vars(first_triage_acuity_cd, 
                 is_left_without_being_seen, 
                 is_admitted, 
                 start_date_fiscal_period_long, 
                 start_date_fiscal_period, 
                 start_date_fiscal_year_long), 
            as.factor)


str(df2.modified)
summary(df2.modified)



