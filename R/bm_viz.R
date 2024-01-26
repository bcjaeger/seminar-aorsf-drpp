#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bm_viz <- function() {

  model_recoder <- c(
    'obliqueRSF-net' = 'obliqueRSF',
    'aorsf-fast' = 'aorsf',
    'rsf-standard' = 'randomForestSRC',
    'ranger-extratrees' = 'ranger',
    'cif-standard' = 'party'
  )

  time_fig_data <- read_rds("data/time_fig_data.rds") %>%
    mutate(model = recode(model, !!!model_recoder))

  medians <- read_rds("data/time_fig_medians.rds") %>%
    mutate(
      model = recode(model, !!!model_recoder),
      color = recode(
        model,
        'obliqueRSF' = 'black',
        'aorsf' = 'white',
        'randomForestSRC' = 'white',
        'ranger' = 'white',
        'party' = 'black'
      ),
      vjust = -0.25
    )

  eval_fig_data <- read_rds('data/eval_fig_data.rds') %>%
    mutate(
      model = recode(model, !!!model_recoder),
      metric = recode(
        metric,
        "Scaled integrated Brier score" = 'Index of Prediction Accuracy',
        "Time-dependent C-statistic" = 'C-statistic'
      )
    ) %>%
    filter(model != 'aorsf-cph',
           model != 'aorsf-net',
           model != 'aorsf-random') %>%
    split(f = .$metric)

  fig_subsets <- list(
    slide_one = c('obliqueRSF',
                  'randomForestSRC',
                  'ranger',
                  'party'),
    slide_two = c('obliqueRSF',
                  'aorsf',
                  'randomForestSRC',
                  'ranger',
                  'party')
  )

  fig_colors <- list(
    slide_one = c('grey',
                  'grey',
                  'grey',
                  'purple'),
    slide_two = c('orange',
                  'grey',
                  'grey',
                  'grey',
                  'purple')
  )


  figs_time <- fig_subsets %>%
    map2(
      .y = fig_colors,
      ~ fig_time_worker(
        fig_time_data = filter(time_fig_data, model %in% .x),
        medians = filter(medians, model %in% .x),
        colors = .y
      )
    )

  figs_eval <- eval_fig_data %>%
    map(fig_eval_worker)

  list(time = figs_time,
       eval = figs_eval)

}

fig_time_worker <- function(fig_time_data, medians, colors){

  fig <- ggplot(fig_time_data) +
    aes(x = time, y = reorder(model, time, FUN=median),
        fill = model) +
    stat_density_ridges(
      quantile_lines = TRUE,
      quantiles = 0.5,
      bandwidth = 0.4,
      scale = 1
    ) +
    scale_x_log10(
      breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000"),
      expand = c(0,0)
    ) +
    geom_text(
      data = medians,
      hjust = medians$hjust,
      vjust = medians$vjust,
      color = "black",
      size = 6,
      aes(label = table_glue("{time}s"))
    ) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = '',
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 15)) +
    labs(x = 'Time to fit a model and compute predictions, seconds',
         y = '')


}

fig_eval_worker <- function(fig_eval_data,  equiv_bound = 0.01){

  fig_eval_data$x <- seq(nrow(fig_eval_data), 1)

  xmax <- max(fig_eval_data$x)

  y_col_0 = -equiv_bound * 10
  y_col_1 = equiv_bound * 4
  y_col_2 = equiv_bound * 7
  y_col_3 = equiv_bound * 10

  y_breaks <- seq(-5, 1) * 1/100

  gg_header <- tibble(
    x = c(xmax + 1,
          xmax + 1,
          xmax + 2,
          xmax + 1,
          xmax + 1,
          xmax + 1),
    median = c(y_col_0,
               (min(y_breaks) + max(y_breaks)) / 2,
               (y_col_1 + y_col_3) / 2,
               y_col_1,
               y_col_2,
               y_col_3),
    label = c("Learner",
              as.character(fig_eval_data$metric[1]),
              "Posterior probability",
              "Equivalence",
              "Difference < 0",
              "Difference < -1"),
    hjust = c(0, 1/2, 1/2, 1/2, 1/2, 1/2)
  )

  gg_rect <- tibble(
    xmin = seq(xmax+1) - 1/2,
    xmax = seq(xmax+1) + 1/2,
    ymin = -Inf,
    ymax = Inf
  ) |>
    filter(seq(n()) %% 2 == 0)

  ggplot(fig_eval_data) +
    aes(x = x, y = median, label = model) +
    geom_segment(x = 0, y = -.01, xend = xmax+1/2, yend = -.01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = .01, xend = xmax+1/2, yend = .01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = 0, xend = xmax+1/2, yend = 0,
                 color = 'darkorange', linetype = 2) +
    geom_vline(xintercept = c(xmax + 1/2, xmax + 3/2)) +
    geom_segment(
      mapping = aes(x = x,
                    y = ci_lwr,
                    xend = x,
                    yend = ci_upr),
      linewidth = 0.75,
      color = 'grey80'
    ) +
    geom_segment(
      mapping = aes(x = x,
                    y = q25,
                    xend = x,
                    yend = q75),
      linewidth = 2,
      color = 'grey60'
    ) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey',
              alpha = 1/6) +
    geom_segment(x = 0, xend = 0,
                 y = min(y_breaks), yend = max(y_breaks)) +
    geom_point(size = 3, color = 'darkorange') +
    geom_text(aes(y = y_col_0), hjust = 0) +
    geom_text(aes(x = x, y = y_col_1, label = prob_equiv)) +
    geom_text(aes(x = x, y = y_col_2, label = prob_super)) +
    geom_text(aes(x = x, y = y_col_3, label = prob_super_duper)) +
    geom_text(data = gg_header, aes(label = label, hjust = hjust)) +
    scale_y_continuous(limits = c(y_col_0, y_col_0*(-1)*1.25),
                       breaks = y_breaks,
                       labels = 100 * y_breaks,
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, xmax + 2.5),
                       expand = c(0, 0)) +
    coord_flip() +
    theme_bw() +
    labs(x = '', y = 'Difference versus aorsf, times 100') +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(hjust = .31),
          legend.position = '')

}
