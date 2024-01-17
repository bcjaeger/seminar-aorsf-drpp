## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  data_sm = load_sm(),

  penguin_figs = viz_penguins(),

  tar_quarto(slides, path = 'index.qmd')

)
