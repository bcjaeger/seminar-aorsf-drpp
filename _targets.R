## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(future)
library(future.callr)
plan(callr)


## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  data_sm = load_sm(),

  penguin_figs = viz_penguins(),

  mccv_sm = mccv_sm_run(data_sm),

  bm_figs = bm_viz(),

  tar_quarto(slides, path = 'index.qmd')

) |>
  tar_hook_before(
    hook = source("conflicts.R"),
    names = everything()
  )

#
# mccv_sm %>%
#   group_by(model) %>%
#   summarize(auc = mean(auc),
#             ipa = mean(ipa),
#             ipa = round(ipa, 3))
