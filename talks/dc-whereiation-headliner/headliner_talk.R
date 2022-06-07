library(tidyverse)
library(whereiation)
library(headliner)
library(lubridate)
library(glue)


rnorm(
  n = 10,
  mean = 0,
  sd = 1
)

#> -0.587 -0.134  0.408  0.232 -1.282  0.523  0.191 -0.389 -0.597  0.441

rnorm(n = 10, mean = 0, sd = 1) %>% mean()

mean(rnorm(n = 10, mean = 0, sd = 1))
#> -0.119



employee_attrition %>%
  relocate(attrition, job_level, job_role, gender) %>%
  select(attrition:performance_rating) %>%
  sample_n(10)

employee_attrition %>%
  group_by(job_level) %>%
  summarise(
    n = n(),
    attrition_rate = mean(attrition)
  ) %>%
  ungroup() %>%
  arrange(desc(attrition_rate)) %>%
  mutate(attrition_rate = scales::percent(attrition_rate, accuracy = 1)) %>%
  knitr::kable() %>%
  kableExtra::kable_styling()

employee_attrition %>%
  select(attrition, job_level) %>%
  plot_deltas("attrition") +
  scale_size(range = c(4,6)) +
  scale_x_continuous(
    limits = c(0, 0.3),
    labels = scales::percent_format(accuracy = 1)
  )


plot_spread(
  df =
    employee_attrition %>%
    select(-matches("rate|satisf|year|number|hike")),
  dep_var = "attrition"
)




plot_deltas(
  df = employee_attrition %>% select(-starts_with("years")),
  dep_var = "attrition"
) +
  scale_x_continuous(
    limits = c(0, 0.5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_size(range = c(2,6)) +
  labs(title = NULL, subtitle = NULL)

employee_attrition %>% select(-starts_with("years"), -daily_rate) %>%
  filter(job_level != "Intern") %>%
  plot_deltas(dep_var = "attrition") +
  scale_x_continuous(
    limits = c(0, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_size(range = c(2,6)) +
  labs(title = NULL, subtitle = NULL)

employee_attrition %>% select(-matches("^(years|month)"), -daily_rate, -employee_number) %>%
  #filter(job_level != "Intern") %>%
  plot_deltas(dep_var = "gender == 'Male'") +
  scale_x_continuous(
    #limits = c(0, NA),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_size(range = c(2,6)) +
  labs(title = NULL, subtitle = NULL)

plot_group_split(
  df = employee_attrition,
  dep_var = "attrition",
  split_on = "gender",
  type = "percent"
)

plot_group_split(
  df = employee_attrition,
  dep_var = "attrition",
  split_on = "gender",
  type = "percent_factor"
)


plot_spread_single_obs <-
  function (df,
            dep_var,
            ...,
            avg_type = c("mean", "median"),
            labels = FALSE,
            id = 1)
  {
    avg_name <- "mean"
    avg <- eval(parse(text = avg_name))
    compare_values <-
      generate_estimate_details(df = df,
                                dep_var = dep_var,
                                avg_type = avg_name,
                                ...)
    get_id <- id

    one_obs_profile <-
      compare_values %>%
      filter(.data$unique_id == get_id) %>%
      select(
        .data$unique_id,
        .data$field,
        .data$value,
        .data$field_wt,
        .data$factor_avg,
        .data$factor_avg_wt,
        .data$estimate,
        .data$n
      ) %>%
      mutate(
        label = "",
        diff = obs_estimate - .data$factor_avg
      )

    grand_avg <- mean(df[[dep_var]])

    x_int <- one_obs_profile$estimate[1]


    plot_orig <-
      plot_spread(df = df, dep_var = dep_var) +
      #geom_vline(xintercept = x_int, size = 1) +
      # geom_segment(
      #   data = one_obs_profile,
      #   xend = x_int,
      #   aes(yend = .data$field, size = .data$field_wt * 10,alpha = .data$field_wt * 10),
      #   size = 2,
      #   color = "black",
      #   show.legend = FALSE
      # ) +
      geom_path(
        data = one_obs_profile %>% arrange(field_wt),
        aes(group = unique_id),
        color = "black", size = 3, alpha = 0.3
      ) +
      geom_point(
        data = one_obs_profile,
        aes(size = n),
        #fill = "white",
        color = "black",
        shape = 21,
        stroke = 2
      ) +
      labs(title = paste0(
        dep_var,
        " across all factors of all fields",
        " - single observation"
      ))

    if (labels == TRUE) {
      plot_orig <-
        plot_orig +
        geom_label(
          data = one_obs_profile %>% filter(diff >= 0),
          aes(x = factor_avg, label = value),
          hjust = 1.2, size = 3, alpha = 0.8,
          fill = "white", color = "black", label.padding = unit(0.2, "lines")#, label.size = NA
        ) +
        geom_label(
          data = one_obs_profile %>% filter(diff < 0),
          aes(x = factor_avg, label = value),
          hjust = -0.15, size = 3, alpha = 0.8,
          fill = "white", color = "black", label.padding = unit(0.2, "lines")#, label.size = NA
        )
    }
  plot_orig
}


plot_spread(
  df =
    employee_attrition %>%
    select(-matches("rate|satisf|year|number|hike")),
  dep_var = "attrition"
) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(x = "mean attrition by factor")


plot_spread_single_obs(
  df =
    employee_attrition %>%
    select(-matches("rate|satisf|year|number|hike")),
  dep_var = "attrition",
  id = 358,
  labels = TRUE
)

set.seed(23456)
df <-
  tibble(
    date = as.Date("2019-01-01") %m+% months(1:28)
  ) %>%
  mutate(
    rand = runif(n(), -2, 2),
    timeframe = case_when(
      date < "2020-03-01" ~ "before COVID",
      date < "2020-07-01" ~ "COVID",
      TRUE ~ "current state"
    ),
    color = ifelse(timeframe == "COVID", "grey60", "blue"),
    impact = case_when(
      date < "2020-03-01" ~ 31,
      date < "2020-07-01" ~
        difftime(as.Date("2020-03-01"), date, units = "days") %>%
        as.numeric() %>%  scales::rescale(to = c(20, 32)),
      TRUE ~ 24
    ),
    y = (rand  + impact),
    callback_time = y
  ) %>%
  group_by(timeframe) %>%
  mutate(mean_y = mean(y)) %>%
  ungroup() %>%
  print()


df %>%
  filter(year(date) == 2020) %>%
  select(date, timeframe, callback_time, mean_y) %>%
  arrange(desc(date))


df %>% #print(n = 30)
  ggplot(aes(x = date, y = y)) +
  geom_line(alpha = 0.5) +
  geom_step(
    aes(y = mean_y, group = timeframe, color = color), direction = "mid"#, linetype = "dashed"
  ) +
  geom_point(aes(color = color), size = 1.5) +
  scale_size(range = c(0, 1)) +
  # facet_grid(~timeframe, scales = "free_x") +
  ylim(0, NA) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = "Time to Return Call (mins)")

p <- .Last.value

p +
  geom_point(
    aes(color = color),
    show.legend = FALSE
  )

headline(24, 32)
# decrease of 1 (4 vs. 5)

headline(32, 24)
# increase of 1 (5 vs. 4)

headline(24, 32, return_data = TRUE) %>% view_list()

          8  delta
         25  delta_p
       an 8  article_delta
       a 25  article_delta_p
         24  comp_value
         32  ref_value
         -8  raw_delta
        -25  raw_delta_p
       a -8  article_raw_delta
      a -25  article_raw_delta_p
         -1  sign
   decrease  trend
  24 vs. 32  orig_values




callback_times <-
  df %>%
  compare_conditions(
    compare = (timeframe == "current state"),
    reference = (timeframe == "before COVID"),
    cols = callback_time
  )



callback_times

#> [1] 10.5
#> [2] 16


callback_times %>%
  headline(
    headline =
      "The avg. callback time has {trend}d by {delta} minutes since COVID began ({orig_values})" #, n_decimal = 0
  )

#>  The avg. callback time has decreased by 8 minutes since COVID began (24 vs. 32)


slice(animal_sleep, c(1,3,5)) %>%
  select(common_name, hours_asleep, hours_awake) %>%
  mutate(
    more = ifelse(hours_asleep > 12, "asleep", "awake"),
    less = ifelse(hours_asleep > 12, "awake", "asleep")
#   ) %>%
#   add_headline_column(
#     compare = hours_asleep,
#     reference = hours_awake,
#     headline = "The {common_name} spends {delta} more {hours} {more} than {less}.",
#     plural_phrases = list(hours = plural_phrasing(single = "hour", multi = "hours"))
#   )
#
# #>  common_name      | hours_asleep| hours_awake | more   |less   |headline
# #>  African Elephant |          3.3|        20.7 | awake  |asleep |The African Elephant spends 17.4 more hours awake than asleep.
# #>  Arctic Fox       |         12.5|        11.5 | asleep |awake  |The Arctic Fox spends 1 more hour asleep than awake.
# #>  Asian Elephant   |          3.9|        20.1 | awake  |asleep |The Asian Elephant spends 16.2 more hours awake than asleep.
#


headline(1, 2) + "; " + headline(3, 4)
#> decrease of 1 (1 vs. 2); decrease of 1 (3 vs. 4)

headline(
  5, 4,
  now = format(today(), "%b %d"),
  headline = "as of {now}, attendence has {trend}d by {delta_p}%"
)
#> as of May 12, attendence has increased by 25%


employee_attrition %>%
  headliner::compare_conditions(
    compare = (gender == "Female"),
    reference = (gender == "Male"),
    cols = attrition
  ) %>%
  headline(
    headline = "The attrition rate of women is {delta_p}% {trend} than men {orig_values}",
    orig_values = "({c}% vs {r}%)",
    trend_phrases = trend_terms("higher", "lower"),
    multiplier = 100#, return_data = TRUE
  )

headline(
  compare = 101,
  reference = 107
)
#> decrease of 6 (101 vs. 107)
#
headliner::animal_sleep

headliner::flights_jfk %>%
  filter(dep_delay < 20) %>%
  whereiation::plot_deltas("dep_delay")

library(headliner)
yoy <-
  demo_data() %>%
  add_date_columns(date_col = date) %>%
  compare_conditions(
    compare = (month == 0),     # this month
    reference = (month == -12), # vs 12 months ago
    cols = x                    # compare column x
  )

compare_values(
  yoy$mean_x_comp,
  yoy$mean_x_ref
) %>%
  view_list()

#> $mean_x_comp
#> [1] 101
#>
#> $mean_x_ref
#> [1] 107

yoy %>%
  headline(
    headline = "We have seen {article_delta_p}% {trend} compared to the same time last year ({orig_values})."
  )

#> We have seen a 5.6% decrease compared to the same time last year (101 vs. 107).
#

headline(
  34, 37,
  headline =
    "We had {article_delta_p}% {trend} in wait times ({orig_values} minutes)"
)

# We had an 8.1% decrease in wait times (34 vs. 37 minutes)
