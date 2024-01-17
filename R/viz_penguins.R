#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

viz_penguins <- function() {

  penguins <- drop_na(penguins)

  fig_peng <- ggplot(data = penguins) +
    aes(x = flipper_length_mm, y = bill_length_mm, label = species) +
    geom_point(aes(color = species, shape = species),
               size = 3,
               alpha = 0.8) +
    theme_bw() +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) +
    labs(x = "\nFlipper length, mm",
         y = "Bill length, mm\n",
         color = "Penguin species",
         shape = "Penguin species") +
    coord_cartesian(ylim = c(30, 70),
                    xlim = c(170, 235)) +
    theme(panel.grid = element_blank(),
          legend.position = '',
          text = element_text(size = 18))

  fig_demo <-  fig_peng +
    geom_mark_ellipse(aes(color = species, fill = species),
                      alpha = 0.075)

  library(rpart)
  library(parttree)

  axis_1 <- rpart(formula = species ~ flipper_length_mm + bill_length_mm,
                  data = penguins,
                  control = rpart.control(maxdepth = 1))

  fig_axis_1 <- fig_peng +
    geom_parttree(data = axis_1, aes(fill=species),
                  alpha = 0.1) +
    scale_fill_manual(values = c("darkorange","cyan4"))

  axis_2 <- rpart(formula = species ~ flipper_length_mm + bill_length_mm,
                  data = penguins,
                  control = rpart.control(maxdepth = 2))

  fig_axis_2 <- fig_peng +
    geom_parttree(data = axis_2, aes(fill=species),
                  alpha = 0.1)

  list(demo = fig_demo,
       axis_1 = fig_axis_1,
       axis_2 = fig_axis_2)

}
