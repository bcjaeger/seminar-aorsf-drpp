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
    age,
    sex = factor(female,
                 levels = c(0,1),
                 labels = c("Male","Female")),
    race = factor(is.na(race_black),
                  levels = c(TRUE, FALSE),
                  labels = c("Non_Black", "Black")),
    treatment = factor(intensive,
                       levels = c(0,1),
                       labels = c("standard", "intensive")),
    # New creatinine based race-agnostic eGFR equation
    egfr_2021 = ckd_epi_2021_compute(age = age,
                                     sex = sex,
                                     screat = screat),
    frail = factor(frail_status,
                   levels = c(0,1,2),
                   labels = c("Fit",
                              "Pre_frail",
                              "Frail")),
    edu_4cat = factor(Edu_4cat),
    smoke_pkyrs,
    stand_sbp,
    stand_dbp,
    height,
    weight,
    PHQ9_score,
    VR12_MCS,
    moca_score_new,
    moca_executive,
    moca_naming,
    moca_attention1,
    moca_attention2,
    moca_attention3,
    moca_language1,
    moca_language2,
    moca_abstraction,
    moca_recall,
    moca_orientation,
    atrialFib,
    BUN,
    HDL,
    UMALCR,
    UMALI,
  )

  left_join(outcomes, covariates) %>%
    filter(time > 0) %>%
    drop_na()

}
