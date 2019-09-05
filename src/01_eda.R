

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
library(lubridate)
library(feasts)
library(ggbeeswarm)
library(anomalize)

#+ rest 
#' ## Data prep 
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
                 # is_left_without_being_seen, 
                 # is_admitted, 
                 start_date_fiscal_period_long, 
                 start_date_fiscal_period, 
                 start_date_fiscal_year_long), 
            as.factor) %>% 
  
  # separate dates and times
  mutate(start_date = lubridate::date(start_dt_tm), 
         # start_time = strftime(as.character(start_dt_tm), format="%H:%M:%S"), 
         start_hour = hour(start_dt_tm), 
         weekday = weekdays(start_date) %>% 
           fct_relevel(levels = c("Monday", 
                                  "Tuesday", 
                                  "Wednesday", 
                                  "Thursday", 
                                  "Friday",
                                  "Saturday", 
                                  "Sunday"))) %>% 
  
  arrange(start_dt_tm)


str(df2.modified)
summary(df2.modified)




# group by day -----------
df3.group_day <- 
  df2.modified %>% 
  
  group_by(start_date, 
           weekday, 
           start_date_fiscal_year_long, 
           start_date_fiscal_period) %>% 
  summarize(daily_visits = n(), 
            daily_admits = sum(is_admitted)) %>% 
  ungroup
  

df3.group_day

v1.min_date <- min(df3.group_day$start_date)
v2.max_date <- max(df3.group_day$start_date)

# v2.max_date - v1.min_date  # 1095 days difference


# group by day and hour -------------
df4.group_day_hr <- 
  df2.modified %>% 
  group_by(start_date,
           weekday, 
           start_date_fiscal_year_long, 
           start_date_fiscal_period, 
           start_hour) %>% 
  summarise(hourly_visits = n(), 
            hourly_admits = sum(is_admitted)) %>% 
  ungroup

df4.group_day_hr

#+ plots
#' ## Exploratory plots 
# time series: -----
df3.group_day %>% 
  ggplot(aes(x = start_date, 
             y = daily_visits)) + 
  geom_line() + 
  geom_smooth()

df3.group_day %>% 
  ggplot(aes(x = start_date, 
             y = daily_admits)) + 
  geom_line() + 
  geom_smooth()


df3.group_day %>% 
  as_tsibble() %>% 
  STL(daily_visits) %>% 
  autoplot

#' No significant seasonality across week. Probably no sig seasonality across year

df3.group_day %>% 
  as_tsibble() %>% 
  STL(daily_visits) %>% 
  filter(start_date >= "2022-04-01", 
         start_date <= "2023-03-31" ) %>% 
  ggplot(aes(x = start_date, 
             y = season_year)) + 
  geom_line() + 
  geom_hline(yintercept = 0, col = "blue")+ 
  labs(title = "Seasonality in ED visits across fy2023")


# anomaly detection: 
# df3.group_day %>% 
#   select(start_date, 
#          daily_visits) %>% 
#   mutate(daily_visits = as.numeric(daily_visits)) %>% 
#   as_tibble() %>% 
#   time_decompose(daily_visits)
# 
# df <- data.frame(date = ymd(c("2019-01-01", "2019-01-02")), 
#                  value = c(240, 243))
# df %>% as_tibble() %>% time_decompose(value)
# 
# 
#   anomalize(daily_visits, 
#             method = "iqr") %>% 
#   autoplot()


# densities: ----
df3.group_day %>% 
  ggplot(aes(x = daily_visits)) + 
  geom_density() + 
  facet_wrap(~start_date_fiscal_year_long)


df3.group_day %>% 
  ggplot(aes(x = daily_admits)) + 
  geom_density() + 
  facet_wrap(~start_date_fiscal_year_long)


# boxplots ------
df3.group_day %>% 
  ggplot(aes(x = weekday, 
             y = daily_visits)) + 
  geom_boxplot() + 
  geom_beeswarm(alpha = .2) + 
  facet_wrap(~start_date_fiscal_year_long) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
      

df3.group_day %>% 
  ggplot(aes(x = weekday, 
             y = daily_admits)) + 
  geom_boxplot() + 
  geom_beeswarm(alpha = .2) + 
  facet_wrap(~start_date_fiscal_year_long) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))



df4.group_day_hr %>% 
  ggplot(aes(x = as.factor(start_hour), 
             y = hourly_visits)) + 
  geom_boxplot() + 
  facet_wrap(~start_date_fiscal_year_long) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

df4.group_day_hr %>% 
  ggplot(aes(x = as.factor(start_hour), 
             y = hourly_admits)) + 
  geom_boxplot() + 
  facet_wrap(~start_date_fiscal_year_long) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))







#+ out
# outputs: 
# write_csv(df2.modified,
#           here::here("results", 
#                      "dst", 
#                      "01_cleaned_data.csv"))
# 
# write_csv(df3.group_day,
#           here::here("results", 
#                      "dst", 
#                      "02_grouped_day.csv"))
# 
# write_csv(df4.group_day_hr,
#           here::here("results", 
#                      "dst", 
#                      "03_grouped_day_hour.csv"))
#              