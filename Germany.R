library(ggplot2)

# Run djhurio's script once to get all EU data & regenerate faceted plots
# install.packages(c("data.table", "ISOcodes", "eurostat"))
source('deaths-by-week.R'); rm(list = ls())

# My goal here: Split mortality data for a single country by pre- & post-CoVID
# TODO: Loop over all other countries & generate plots with English labels.
country <- 'Germany'
covid_start <- 2020
label_pre  <- 'vor SARS-CoV2'
label_post <- 'Pandemiejahre'
weeks <- seq(from = 0, to = 52, by = 4)
file_name <- paste0('output/', country, '.jpg')

data <- readr::read_csv('data.csv') |>
  dplyr::filter(cntry == country) |>
  dplyr::transmute(year, # = gsub('W-\\d+', '', time),
                   # country = cntry, # for all countries, instead of filter
                   week, death.rate) |>
  dplyr::mutate(SARS = ifelse(year < covid_start, label_pre, label_post)) #|>
# dplyr::group_by(Woche)
# death_median = median(death.rate),
# death_iqr = IQR(death.rate)

pre <- dplyr::filter(data, SARS == label_pre) |> dplyr::rename(year_pre = year)
post <- dplyr::filter(data, SARS == label_post) |> dplyr::rename(year_post = year)
post_years <- unique(post$year_post)
N_years <- length(post_years)
max_deaths <- ceiling(max(post$death.rate, pre$death.rate))

pre_start <- min(pre$year_pre)
pre_end <- max(pre$year_pre)
# Grippe <- dplyr::filter(pre, Woche > 7 & Woche < 11) %>%
#   dplyr::slice_max(death.rate, prop = 0.05) %>%
#   dplyr::select(Jahr) %>%
#   unique()

plot <- function(year) {
  dplyr::filter(post, year_post == year) |>
    ggplot(mapping = aes(x = week, y = death.rate)) +
    geom_line(linewidth = 1, color = "red") +
    geom_line(data = pre, aes(alpha = year_pre, group = year_pre), show.legend = FALSE) +
    # geom_line(
    #   data = dplyr::filter(post, year == 2020),
    #   mapping = aes(y = death.rate),
    #   linewidth = 1
    # ) +
    # facet_wrap(vars(country)) + # for all countries, instead of filter
    scale_x_continuous(breaks = seq(0, 52, 13), minor_breaks = NULL) +
    # scale_color_gradient(low = 'orange', high = "darkred", name = label_post) +
    # ylab('Death rate [per million]') +
    labs(
      title = year,
      # paste0(country, ': Death rate before & after SARS-CoV2'),
      # subtitle = 'Sources: eurostat demo_r_mwk_ts & GitHub.com/djhurio/COVID-19',
      # subtitle = paste0(
      #   'Quelle: eurostat demo_r_mwk_ts & GitHub.com/djhurio/COVID-19',
      #   '\nFrühere Krisen: Grippe im Spätwinter und Hitze im Sommer',
      #   '\nAlterungstrend: ',
      #   pre_start,
      #   '/hellgrau → ',
      #   pre_end,
      #   '/schwarz'
      # ),
      # x = 'calendar week'
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = c(.5, .8),
      legend.background = element_rect(fill = 'white', linewidth = 0)
    ) +
    guides(col = guide_legend(label.position = 'top'))
}

(
  aggregate_plot <- gridExtra::arrangeGrob(
      plot(2020),
      plot(2021),
      plot(2022),
      plot(2023),
      plot(2024),
      plot(2025),
    ncol = 3,
    top = paste0("Deutschland: Sterblichkeit seit SARS-CoV2\n",
      'Quelle: eurostat demo_r_mwk_ts & GitHub.com/djhurio/COVID-19',
      '\nFrühere Krisen: Grippe im Spätwinter und Hitze im Sommer',
      '\nAlterungstrend: ',
      pre_start,
      '/hellgrau → ',
      pre_end,
      '/schwarz'
    ),
    left = "Todesfälle pro 1 Mio. Einwohner:inne:n",
    bottom = "Kalenderwochen"
  )
)

ggsave(file_name, aggregate_plot)

