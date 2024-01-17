#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_sm <- function() {

  outcomes <-
    read_csv("T:/sprint/bjaeger/sprint-dementia-jm/data/pd_outcomes.csv")

  covariates <-
    read_csv("T:/sprint/npajewski/NDI/Data/longterm_death.csv") %>%
    transmute(
    pid,
    age_cat = factor(sub_senior,
                     levels = c(0, 1),
                     labels = c("lt75_years", "gteq75_years")),
    sex = factor(female,
                 levels = c(0,1),
                 labels = c("Male","Female")),
    race = factor(is.na(race_black),
                  levels = c(TRUE, FALSE),
                  labels = c("Non_Black", "Black")),
    ckd = factor(eGFR_CKDEPI < 60,
                 levels = c(FALSE, TRUE),
                 labels = c("No", "Yes")),
    # New creatinine based race-agnostic eGFR equation
    egfr_2021 = ckd_epi_2021_compute(age = age,
                                     sex = sex,
                                     screat = screat),
    ckd_2021 = factor(egfr_2021 < 60,
                      levels = c(FALSE, TRUE),
                      labels = c("No", "Yes")),
    moca = factor(moca_status,
                  levels = c(0, 1),
                  labels = c("gt10th_percentile",
                             "lteq10th_percentile")),
    frail = factor(frail_status,
                   levels = c(0,1,2),
                   labels = c("Fit",
                              "Pre_frail",
                              "Frail")),
    atrialFib,
    BUN,
    CHR,
    CL,
    CO2,
    CRDUR,
    GLUR,
    HDL,
    LDLR,
    TRR,
    UMALCR,
    UMALI,
    POTASSIUM,
    moca_score_new
  )

  left_join(outcomes, covariates) %>%
    filter(time > 0) %>%
    drop_na()

}
