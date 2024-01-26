#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_sm
mccv_sm_run <- function(data_sm) {

  res <- list()

  for(i in 1:50){

    print(i)

    trn <- sample(nrow(data_sm), round(nrow(data_sm)/2))

    data_trn <- data_sm[trn, ]
    data_tst <- data_sm[-trn,]

    fit_aorsf <- orsf(Surv(time, status) ~ . - pid,
                      data = data_trn)

    data_trn_censor <- data_trn %>%
      mutate(status = (status - 1) * (-1),
             risk = as.numeric(fit_aorsf$pred_oobag))

    fit_censor <- orsf_update(fit_aorsf, data = data_trn_censor)

    wts_ipc <- data_trn$status / as.numeric(1 - fit_censor$pred_oobag)

    wts_ipc[wts_ipc == 0] <- min(wts_ipc[wts_ipc!=0])

    fit_aorsf_ipc <- orsf_update(fit_aorsf, weights = wts_ipc)

    fit_rsf <- rfsrc(Surv(time, status) ~ . - pid,
                     data = as.data.frame(data_trn))

    prd_aorsf <- predict(fit_aorsf,
                         new_data = data_tst,
                         pred_horizon = 5)

    prd_ipc <- predict(fit_aorsf_ipc,
                       new_data = data_tst,
                       pred_horizon = 5)

    prd_rsf <- predictRisk(fit_rsf,
                           newdata = as.data.frame(data_tst),
                           times = 5)

    sc <- Score(list(aorsf = prd_aorsf,
                     ipc = prd_ipc,
                     rsf = prd_rsf),
                formula = Surv(time, status) ~ 1,
                data = data_tst,
                times = 5,
                summary = "IPA")

    res[[i]] <- tibble(model = sc$AUC$score$model,
                       auc = sc$AUC$score$AUC,
                       ipa = sc$Brier$score$IPA[-1])


  }

  bind_rows(res)


}
