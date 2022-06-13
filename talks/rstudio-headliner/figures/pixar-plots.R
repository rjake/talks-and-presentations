library(headliner)
library(simplecolors)
library(tidyverse)
library(lubridate)
library(glue)
library(ggtext) # if bold doesn't work, need to install dev version of gridtext
library(scales)

setwd(dirname(.rs.api.getSourceEditorContext()$path))

# cran_tidy <- cranlogs::cran_downloads(tidyverse_packages(), from = "2020-01-01", to = "2021-12-31")
# saveRDS(cran_tidy, "cran_tidyverse.Rds")
cran_tidy <- readRDS("cran_tidyverse.Rds")

plot_color <-
  list(
    increase = sc("mutedteal3"),
    decrease = sc("mutedpink3")
  )


text_color <- function(x, color) {
  glue("<span style='color:{color};'>{x}</span>")
}

font_size <- function(x, size) {
  glue("<span style='font-size:{size}pt;'>{x}</span>")
}

# pixar_plots ----

pixar_long <-
  pixar_films |>
  pivot_longer(-c(order:rating)) |>
  print()

headline_table <- {
  tribble(
    ~name,            ~title,                     ~id,  ~script,
    "bo_domestic",     "Box Office Domestic",       1,  "{trend}d by ${delta} USD",
    "bo_intl",         "Box Office International",  4,  "{trend}d by ${delta} USD",
    "metacritic",      "Metacritic",                2,  "{trend}d by {delta} points",
    "rotten_tomatoes", "Rotten Tomatoes",           5,  "{trend}d by {delta} points",
    "run_time",        "Run time",                  3,  "{trend}d by {delta} min."
  )
}

headlines <-
  pixar_long |>
  group_by(name) |>
  compare_conditions(
    x = order > 10,
    y = order <= 10,
    .cols = value
  ) |>
  left_join(headline_table) |>
  mutate(
    headline = headline(
      x = mean_value_x,
      y = mean_value_y,
      headline = script
    )
  ) |>
  select(-script) |>
  print()



headlines |>
  rename(
    later = mean_value_x,
    first_10 = mean_value_y
  ) |>
  mutate(
    color = ifelse(later > first_10, plot_color$increase, plot_color$decrease),
    headline = map_chr(
      headline,
      ~str_replace(.x, "increased", text_color("increased", plot_color$increase)) |>
        str_replace("decreased", text_color("decreased", plot_color$decrease))
    ) |>
      font_size("10"),
    facet = fct_reorder(
      .f = paste(title, headline, sep = "<br>"),
      .x = id
    )
  ) |>
  ggplot() +
  facet_wrap(~facet, scales = "free_x", nrow = 2) +
  geom_col(aes(x = first_10, y = "First 10", fill = sc("grey2"))) +
  geom_col(aes(x = later, y = "Recent", fill = color)) +
  scale_fill_identity() +
  theme(
    plot.title.position = "plot",
    panel.background = element_rect("white", "grey80"),
    strip.background = element_rect("white"),
    strip.text = ggtext::element_textbox(size = 12, hjust = 0, vjust = 1),
    plot.title = ggtext::element_textbox_simple()
  ) +
  labs(
    title = "How do recent Pixar films compare with the first 10 movies?",
    x = NULL, y = NULL
  )

# CRAN tidyverse packages ----
pkg_month <- 
  cran_tidy |> 
  filter(year(date) %in% 2020:2021) |> 
  count(
    package, 
    year = year(date),
    month = floor_date(date, "month"),
    wt = count
  ) |> 
  print()

pkg_growth <- 
  pkg_month |> 
  group_by(package) |> 
  compare_conditions(
    x = year == 2021,
    y = year == 2020,
    .cols = n
  ) |> 
  add_headline_column(
    x = mean_n_x, 
    y = mean_n_y,
    headline = "{package} {trend}d {delta_p}% ({ln(x)} vs {ln(y)})",
    return_cols = c(delta, raw_delta),
    # extra arguments passed on to glue
    ln = #format_large_number(accuracy_1k = 0)
      label_number(
        accuracy = 0.1,
        scale_cut = cut_short_scale()
      )
  ) |> 
  arrange(raw_delta) |> 
  print()

pkg_labels <- 
  pkg_growth |> 
  mutate(
    keep = case_when(
      delta == min(delta) ~ text_color("lowest change", sc("grey4")),
      raw_delta == min(raw_delta) ~ text_color("biggest decrease", sc("mutedpink4")),
      raw_delta == max(raw_delta) ~ text_color("biggest increase", sc("mutedteal4"))
    ),
    .keep = "unused"
  ) |> 
  drop_na(keep) |> 
  select(-starts_with("mean_")) |> 
  mutate(
    facet = paste0(
       keep, "<br>",
      map_chr(
        headline,
        ~str_replace(.x, "ggplot2", text_color("**ggplot2**", sc("mutedteal4"))) |> 
          str_replace("magrittr", text_color("**magrittr**", sc("mutedpink4"))) |> 
          str_replace("xml2", text_color("**xml2**", sc("grey4")))
      ) |>
        font_size("10")
    )
  ) |> 
  left_join(pkg_month)


pkg_labels |> 
  #filter(str_detect(keep, "decr")) |> 
  ggplot(aes(month, n, color = package)) +
  facet_wrap(~facet, nrow = 1) +
  geom_line( ) + geom_point() +
  #geom_line(color = sc("mutedblue4")) + geom_point(color = sc("mutedblue4")) +
  scale_y_continuous(
    labels = number_format(scale_cut = cut_short_scale()),
    expand = expansion(c(0, 0.02)),
    limits = c(0, NA)
  ) +
  scale_x_date(date_labels = "%m/%y", expand = expansion(c(0.1, 0.025))) +
  scale_color_manual(values = sc("mutedteal3", "mutedpink3", "grey3")) +
  labs(
    title = "Changes in overall monthly downloads **2021** vs **2020**",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect("white", "grey80"),
    legend.position = "none",
    strip.background = element_rect("white"),
    strip.text = ggtext::element_textbox(size = 12, hjust = 0, vjust = 1),
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple()
  )
