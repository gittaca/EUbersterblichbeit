library(ggplot2)

# Run djhurio's script once to get all EU data & regenerate faceted plots
# source('deaths-by-week.R'); rm(list = ls())

# My goal here: Split mortality data for a single country by pre- & post-CoVID
country <- 'Germany'
covid_start <- 2020
label_pre  <- 'vor SARS-CoV2'
label_post <- 'nach SARS-CoV2'
weeks <- seq(from = 0, to = 52, by = 4)
file_name <- paste0('output/', country, '.jpg')

data <- readr::read_csv('data.csv') |>
  dplyr::filter(cntry == country) |>
  dplyr::transmute(year = gsub('W\\d+', '', time),
                   week, death.rate) |>
  dplyr::mutate(SARS = ifelse(year < covid_start, label_pre, label_post))

pre <- dplyr::filter(data, SARS == label_pre)
post <- dplyr::filter(data, SARS == label_post)
start <- min(pre$year)
end <- max(pre$year)

ggplot(mapping = aes(x = week, y = death.rate)) +
  geom_line(data = pre, aes(alpha = year), show.legend = FALSE) +
  geom_line(
    data = post,
    mapping = aes(y = death.rate, color = year),
    linewidth = 1
  ) +
  scale_color_brewer(palette = 'YlOrRd', name = label_post) +
  scale_x_continuous(breaks = weeks,
                     minor_breaks = NULL) +
  ylab('Todesfälle pro 1 Mio. Einwohner:inne:n') +
  labs(
    title = paste0('Sterblichkeit seit SARS-CoV2 (Quelle: eurostat demo_r_mwk_ts)'),
    subtitle = paste0(
      'Frühere Krisen: Grippe im Spätwinter und Hitze im Sommer',
      '\nAlterungstrend: ',
      start,
      '/hellgrau → ',
      end,
      '/schwarz'
    ),
    x = 'Kalenderwoche'
  ) +
  theme_minimal() +
  theme(legend.position = c(.5, .8),
        legend.background = element_rect(fill = 'white', linewidth = 0))

ggsave(file_name)
