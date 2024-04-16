get_icer_obj <- function(parameters, perisk, model, qol_LE, c_matrix, n_samples = 200){

    base1         <- perisk$df
    npositives    <- perisk$npositives
    n             <- n_samples
    cohort_size   <- parameters$cohort_size
    time_horizon  <- parameters$time_horizon
    will_to_pay   <- parameters$will_to_pay
    tpt_cov       <- parameters$tpt_cov
    tpt_compl     <- parameters$tpt_compl
    tpt_cost_lfup <- parameters$tpt_cost_lfup
    dr            <- parameters$dr
    n_sec_cases   <- parameters$new_cases

    #CFR Crofts et al2008
    cfr       <- c(0.012, 0.012, 0.048, 0.176)
    frac_eptb <- 0.2
    av_tbdur  <- 1
    frac_post <- 0.25 # https://karger.com/res/article/100/8/751/820992/Post-Tuberculosis-Lung-Disease-Clinical-Review-of

    lookup_tpt_eff <- c(
        "6INH" = 0.6,
        "3HP"  = 0.53,
        "3RH"  = 0.6
    )

    lookup_tpt_ae <- c(
        "6INH" = 0.04,
        "3HP"  = 0.08,
        "3RH"  = 0.039
    )

    tpt_eff  <- lookup_tpt_eff[parameters$tpt]
    tpt_ae   <- lookup_tpt_ae[parameters$tpt]

    tst_attr <- if (parameters$test == "Tuberculin Skin Test") parameters$tst_attr else 0

    # Campaign costs
    if (parameters$campcost_dist == "Gamma") {

        cost_camp      <- parameters$cost_camp_gamma
        cost_campsd    <- parameters$cost_campsd_gamma#(xmax-xmin)/3.92
        cost_campmin   <- NA
        cost_campmax   <- NA
        cost_campshape <- NA
        xmean          <- parameters$cost_camp_gamma
        sd             <- parameters$cost_campsd_gamma#(xmax-xmin)/3.92
        shape          <- (xmean^2) / (sd^2)
        sim_camp_cost  <- rgamma(n, shape = shape, rate = shape / xmean)

    } else if (parameters$campcost_dist =="PERT"){

        cost_camp      <- parameters$cost_camp_pert
        cost_campsd    <- NA
        cost_campmin   <- parameters$cost_campmin_pert
        cost_campmax   <- parameters$cost_campmax_pert
        cost_campshape <- parameters$cost_camplam
        xmean          <- parameters$cost_camp_pert
        xmin           <- parameters$cost_campmin_pert
        xmax           <- parameters$cost_campmax_pert
        lam            <- parameters$cost_camplam
        sim_camp_cost  <- freedom::rpert(n, xmin, xmax, xmean, lam)

    } else {

        stop("something is wrong: contact app developers")

    }


    ## test
    if (parameters$testcost_dist =="Gamma"){

        cost_test         <- parameters$cost_test_gamma
        cost_testsd       <- parameters$cost_testsd_gamma
        cost_testmin      <- NA
        cost_testmax      <- NA
        cost_testshape    <- NA
        xmean             <- parameters$cost_test_gamma
        sd                <- parameters$cost_testsd_gamma# (xmax-xmin)/3.92
        shape             <- (xmean^2) / (sd^2)
        sim_test_cost     <- if (xmean == 0) replicate(n, xmean) else rgamma(n, shape = shape, rate = shape / xmean)

    } else if (parameters$testcost_dist =="PERT"){

        cost_test         <- parameters$cost_test_pert
        cost_testsd       <- NA
        cost_testmin      <- parameters$cost_testmin_pert
        cost_testmax      <- parameters$cost_testmax_pert
        cost_testshape    <- parameters$cost_testlam
        xmean             <- parameters$cost_test_pert
        xmin              <- parameters$cost_testmin_pert
        xmax              <- parameters$cost_testmax_pert
        lam               <- parameters$cost_testlam
        sim_test_cost     <- if (xmean == 0) replicate(n, xmean) else freedom::rpert(n, xmin, xmax, xmean, lam)

    } else {

        stop("something is wrong: contact app developers")

    }


    ## TPT cost
    if (parameters$tptcost_dist == "Gamma") {

        cost_tpt      <- parameters$cost_tpt_gamma
        cost_tptsd    <- parameters$cost_tptsd_gamma
        cost_tptmin   <- NA
        cost_tptmax   <- NA
        cost_tptshape <- NA
        xmean         <- parameters$cost_tpt_gamma
        sd            <- parameters$cost_tptsd_gamma#(xmax-xmin)/3.92
        shape         <- (xmean^2) / (sd^2)
        sim_tpt_cost  <- if (xmean==0) replicate(n, xmean) else rgamma(n, shape = shape, rate = shape / xmean)

    } else if (parameters$tptcost_dist == "PERT"){

        cost_tpt      <- parameters$cost_tpt_pert
        cost_tptsd    <- NA
        cost_tptmin   <- parameters$cost_tptmin_pert
        cost_tptmax   <- parameters$cost_tptmax_pert
        cost_tptshape <- parameters$cost_tptlam
        xmean         <- parameters$cost_tpt_pert
        xmin          <- parameters$cost_tptmin_pert
        xmax          <- parameters$cost_tptmax_pert
        lam           <- parameters$cost_tptlam
        sim_tpt_cost  <- if (xmean == 0) replicate(n, xmean) else freedom::rpert(n, xmin, xmax, xmean, lam)
    }

    ## TB Tx cost
    if (parameters$tbtxcost_dist =="Gamma"){

        cost_tbtx      <- parameters$cost_tbtx_gamma
        cost_tbtxsd    <- parameters$cost_tbtxsd_gamma
        cost_tbtxmin   <- NA
        cost_tbtxmax   <- NA
        cost_tbtxshape <- NA
        xmean          <- parameters$cost_tbtx_gamma
        sd             <- parameters$cost_tbtxsd_gamma#(xmax-xmin)/3.92
        shape          <- (xmean^2) / (sd^2)
        sim_tbtx_cost  <- rgamma(n, shape = shape, rate = shape / xmean)

    } else if (parameters$tbtxcost_dist == "PERT"){

        cost_tbtx      <- parameters$cost_tbtx_pert
        cost_tbtxsd    <- NA
        cost_tbtxmin   <- parameters$cost_tbtxmin_pert
        cost_tbtxmax   <- parameters$cost_tbtxmax_pert
        cost_tbtxshape <- parameters$cost_tbtxlam
        xmean          <- parameters$cost_tbtx_pert
        xmin           <- parameters$cost_tbtxmin_pert
        xmax           <- parameters$cost_tbtxmax_pert
        lam            <- parameters$cost_tbtxlam
        sim_tbtx_cost  <- freedom::rpert(n,xmin,xmax,xmean,lam)
    }

    # TB QOL
    if (parameters$ptbqol_dist == "Beta") {

        qol_tb      <- parameters$qol_ptb_beta
        qol_tbsd    <- parameters$qol_ptbsd_beta
        qol_tbmin   <- NA
        qol_tbmax   <- NA
        qol_tbshape <- NA
        xmean       <- parameters$qol_ptb_beta
        sd          <- parameters$qol_ptbsd_beta# (xmax-xmin)/3.92
        pars_beta   <- estBetaParams(xmean, sd^2 )
        sim_tb_qol  <- rbeta(n, pars_beta$alpha, pars_beta$beta)

    } else if (parameters$ptbqol_dist =="PERT"){

        qol_tb      <- parameters$qol_ptb_pert
        qol_tbsd    <- NA
        qol_tbmin   <- parameters$qol_ptbmin_pert
        qol_tbmax   <- parameters$qol_ptbmax_pert
        qol_tbshape <- parameters$qol_ptblam_pert
        xmean       <- parameters$qol_ptb_pert
        xmin        <- parameters$qol_ptbmin_pert
        xmax        <- parameters$qol_ptbmax_pert
        lam         <- parameters$qol_ptblam_pert
        sim_tb_qol  <- freedom::rpert(n,xmin,xmax,xmean,lam)

    }


    #EPTB QoL
    if (parameters$eptbqol_dist == "Beta"){
        qol_eptb      <- parameters$qol_eptb_beta
        qol_eptbsd    <- parameters$qol_eptbsd_beta
        qol_eptbmin   <- NA
        qol_eptbmax   <- NA
        qol_eptbshape <- NA
        xmean         <- parameters$qol_eptb_beta
        sd            <- parameters$qol_eptbsd_beta#(xmax-xmin)/3.92
        pars_beta     <- estBetaParams(xmean,sd^2 )
        sim_eptb_qol  <- rbeta(n, pars_beta$alpha, pars_beta$beta)

    } else if (parameters$eptbqol_dist =="PERT"){
        qol_eptb      <- parameters$qol_eptb_pert
        qol_eptbsd    <- NA
        qol_eptbmin   <- parameters$qol_eptbmin_pert
        qol_eptbmax   <- parameters$qol_eptbmax_pert
        qol_eptbshape <- parameters$qol_eptblam_pert
        xmean         <- parameters$qol_eptb_pert
        xmin          <- parameters$qol_eptbmin_pert
        xmax          <- parameters$qol_eptbmax_pert
        lam           <- parameters$qol_eptblam_pert
        sim_eptb_qol  <- freedom::rpert(n, xmin, xmax, xmean, lam)

    }

    # POstTB QoL
    if (parameters$postqol_dist == "Beta"){
        qol_postb      <- parameters$qol_post_beta
        qol_postbsd    <- parameters$qol_postsd_beta
        qol_postbmin   <- NA
        qol_postbmax   <- NA
        qol_postbshape <- NA
        xmean          <- parameters$qol_post_beta
        sd             <- parameters$qol_postsd_beta#(xmax-xmin)/3.92
        pars_beta      <- estBetaParams(xmean,sd^2 )
        sim_post_qol   <- rbeta(n, pars_beta$alpha, pars_beta$beta)

    } else if (parameters$postqol_dist =="PERT"){
        qol_postb      <- parameters$qol_post_pert
        qol_postbsd    <- NA
        qol_postbmin   <- parameters$qol_postmin_pert
        qol_postbmax   <- parameters$qol_postmax_pert
        qol_postbshape <- parameters$qol_postlam_pert
        xmean          <- parameters$qol_post_pert
        xmin           <- parameters$qol_postmin_pert
        xmax           <- parameters$qol_postmax_pert
        lam            <- parameters$qol_postlam_pert
        sim_post_qol   <- freedom::rpert(n, xmin, xmax, xmean, lam)

    }

    #AE TPT QOL
    if (parameters$aeqol_dist =="Beta"){

        xmean      <- parameters$qol_ae_beta
        sd         <- parameters$qol_aesd_beta#(xmax-xmin)/3.92
        pars_beta  <- estBetaParams(xmean, sd^2 )
        sim_ae_qol <- rbeta(n, pars_beta$alpha, pars_beta$beta)

    } else if (parameters$aeqol_dist =="PERT"){

        xmean      <- parameters$qol_ae_pert
        xmin       <- parameters$qol_aemin_pert
        xmax       <- parameters$qol_aemax_pert
        lam        <-  parameters$qol_aelam_pert
        sim_ae_qol <- freedom::rpert(n,xmin,xmax,xmean,lam)

    }


    #TODO - JV, can we scrap this? TT
    # Table of parameters -----------------------------------------------------

    # Table_params <- data.frame(
    #   Parameter = c("Cohort size",
    #                 "Time horizon",
    #                 "Willingness to pay",
    #                 "TPT",
    #                 "TPT effectiveness",
    #                 "TPT coverage",
    #                 "TPT completion",
    #                 "TPT cost in LFUP",
    #                 "Cost of campaign",
    #                 "Cost of campaign SD (Gamma)",
    #                 "Cost of campaign min (PERT)",
    #                 "Cost of campaign max (PERT)",
    #                 "Cost of campaign shape (PERT)",
    #                 "Cost of TBI test",
    #                 "Cost of TBI test SD (Gamma)",
    #                 "Cost of TBI test min (PERT)",
    #                 "Cost of TBI test max (PERT)",
    #                 "Cost of TBI test shape (PERT)",
    #                 "Cost of TPT",
    #                 "Cost of TPT SD (Gamma)",
    #                 "Cost of TPT min (PERT)",
    #                 "Cost of TPT max (PERT)",
    #                 "Cost of TPT shape (PERT)",
    #                 "Discount rate",
    #                 "QoL of TB disease",
    #                 "QoL of TB disease SD (Gamma)",
    #                 "QoL of TB disease min (PERT)",
    #                 "QoL of TB disease max (PERT)",
    #                 "QoL of TB disease shape (PERT)",
    #                 "QoL of EPTB disease",
    #                 "QoL of EPTB disease SD (Gamma)",
    #                 "QoL of EPTB disease min (PERT)",
    #                 "QoL of EPTB disease max (PERT)",
    #                 "QoL of EPTB disease shape (PERT)",
    #                 "QoL of Post-TB disease",
    #                 "QoL of Post-TB disease SD (Gamma)",
    #                 "QoL of Post-TB disease min (PERT)",
    #                 "QoL of Post-TB disease max (PERT)",
    #                 "QoL of Post-TB disease shape (PERT)"
    #
    #   ),
    #   Current_scenario =c( parameters$n,
    #                        parameters$t_hor,
    #                        parameters$will_to_pay,
    #                        parameters$tpt,
    #                        tpt_eff,
    #                        tpt_cov,
    #                        tpt_compl,
    #                        tpt_cost_lfup,
    #                        cost_camp,
    #                        cost_campsd,
    #                        cost_campmin,
    #                        cost_campmax,
    #                        cost_campshape,
    #                        cost_test,
    #                        cost_testsd,
    #                        cost_testmin,
    #                        cost_testmax,
    #                        cost_testshape,
    #                        cost_tpt,
    #                        cost_tptsd,
    #                        cost_tptmin,
    #                        cost_tptmax,
    #                        cost_tptshape,
    #                        dr,
    #                        qol_tb,
    #                        qol_tbsd,
    #                        qol_tbmin,
    #                        qol_tbmax,
    #                        qol_tbshape,
    #                        qol_eptb,
    #                        qol_eptbsd,
    #                        qol_eptbmin,
    #                        qol_eptbmax,
    #                        qol_eptbshape,
    #                        qol_postb,
    #                        qol_postbsd,
    #                        qol_postbmin,
    #                        qol_postbmax,
    #                        qol_postbshape
    #
    #
    #   )
    # )


    # Yearly predictions
    ii                 <- time_horizon
    base1$studytime    <- (365 * ii) - 42
    preds              <- predict(model, base1, type="fail", se.fit=T)
    meanx              <- preds[,1]
    sd                 <- (preds[,3] - preds[,2]) / 3.92
    predictions        <- rnorm(cohort_size, meanx, sd)

    # Cumulative TB cases by age
    logical_matrix_itv <- predictions * tpt_eff# p < predictions*tpt_eff
    tmp                <- list2DF(list(age = base1$agespl1, pred = predictions))
    sim_cases_age      <- summarise(tmp, pred = sum(pred, na.rm = TRUE), .by = age)

    # Estimate and add secondary cases
    cases                  <- sim_cases_age$pred
    sec_cases              <- (cases %*% c_matrix) * n_sec_cases
    sim_cases_age          <- cases + sec_cases
    sim_cases_age_eptb     <- round(sim_cases_age * frac_eptb)
    sim_cases_age_ptb      <- sim_cases_age-sim_cases_age_eptb
    sim_deaths             <- sim_cases_age_ptb * cfr

    cases_itv              <- cases * tpt_eff
    sec_cases_itv          <- (cases_itv %*% c_matrix) * n_sec_cases
    sim_cases_age_itv      <- cases_itv + sec_cases_itv
    sim_cases_age_eptb_itv <- round(sim_cases_age_itv * frac_eptb)
    sim_cases_age_ptb_itv  <- sim_cases_age_itv-sim_cases_age_eptb_itv
    sim_deaths_itv         <- sim_cases_age_ptb_itv * cfr

    baseline_qaly          <- table(base1$agespl1) * qol_LE
    qaly_loss_deaths       <- sum(sim_deaths * qol_LE)
    qaly_loss_deaths_itv   <- sum(sim_deaths_itv * qol_LE)

    discount               <- 1/ ( (1 + dr) ^ time_horizon )

    qaly_loss_ptb          <- discount * sum(sim_cases_age_ptb) * sim_tb_qol * av_tbdur
    qaly_loss_eptb         <- discount * sum(sim_cases_age_eptb) * sim_eptb_qol * av_tbdur
    qaly_loss_ptb_itv      <- discount * sum(sim_cases_age_ptb_itv) * sim_tb_qol * av_tbdur
    qaly_loss_eptb_itv     <- discount * sum(sim_cases_age_eptb_itv) * sim_eptb_qol * av_tbdur
    qaly_loss_post         <- sum((sim_cases_age_ptb - sim_deaths) * frac_post * qol_LE) * sim_post_qol
    qaly_loss_post_itv     <- sum((sim_cases_age_ptb_itv - sim_deaths_itv) * frac_post * qol_LE)  * sim_post_qol
    qaly_loss_AE_itv       <- npositives * tpt_cov  * tpt_ae * sim_ae_qol
    deaths_loss            <- rep.int(qaly_loss_deaths, n_samples)
    deaths_loss_itv        <- rep.int(qaly_loss_deaths_itv, n_samples)
    qaly                   <- sum(baseline_qaly) - (qaly_loss_ptb + qaly_loss_eptb + qaly_loss_post + deaths_loss)
    qaly_itv               <- sum(baseline_qaly) - (qaly_loss_ptb_itv + qaly_loss_eptb_itv + qaly_loss_post_itv + deaths_loss_itv + qaly_loss_AE_itv)
    qaly_margin            <- qaly_itv - qaly


    # QALY loss composition
    qaly_loss_ptb          <- discount * sum(sim_cases_age_ptb) * mean(sim_tb_qol) * av_tbdur
    qaly_loss_eptb         <- discount * sum(sim_cases_age_eptb) * mean(sim_eptb_qol) * av_tbdur
    qaly_loss_ptb_itv      <- discount * sum(sim_cases_age_ptb_itv) * mean(sim_tb_qol) * av_tbdur
    qaly_loss_eptb_itv     <- discount * sum(sim_cases_age_eptb_itv) * mean(sim_eptb_qol) * av_tbdur
    qaly_loss_post         <- sum((sim_cases_age_ptb - sim_deaths) * frac_post * (qol_LE - qol_LE * (1 - mean(sim_post_qol)))) # TODO - did you mean this (it cancels)??
    qaly_loss_post_itv     <- sum((sim_cases_age_ptb_itv - sim_deaths_itv)  * frac_post * (qol_LE - qol_LE * (1 - mean(sim_post_qol)))) # TODO - did you mean this (it cancels)??
    qaly_loss_AE_itv       <- npositives * tpt_cov  * tpt_ae * mean(sim_ae_qol)
    qaly_loss_AE           <- 0

    qaly_loss_dist         <- rbind(Death=qaly_loss_deaths,
                                    PTB=qaly_loss_ptb,
                                    EPTB=qaly_loss_eptb,
                                    POstTB=qaly_loss_post,
                                    AE=qaly_loss_AE)

    qaly_loss_dist_itv     <- rbind(Death=qaly_loss_deaths_itv,
                                    PTB=qaly_loss_ptb_itv,
                                    EPTB=qaly_loss_eptb_itv,
                                    PostTB=qaly_loss_post_itv,
                                    AE=qaly_loss_AE_itv)

    # Cost composition
    tbtx_cost              <- discount * sum(sim_cases_age) *  sim_tbtx_cost
    tbtx_cost_itv          <- discount * sum(sim_cases_age_itv) *  sim_tbtx_cost
    itv_start_cost         <- sim_test_cost * cohort_size * (1 - tst_attr) +
                                npositives * tpt_cov * tpt_compl * sim_tpt_cost +
                                npositives * tpt_cov * (1 - tpt_compl) * sim_tpt_cost * tpt_cost_lfup + sim_camp_cost

    cost                   <- tbtx_cost
    cost_itv               <- tbtx_cost_itv + itv_start_cost
    cost_margin            <- cost_itv - cost

    tbtx_cost              <- discount * sum(sim_cases_age) * mean(sim_tbtx_cost)
    tbtx_cost_itv          <- discount * sum(sim_cases_age_itv) *   mean(sim_tbtx_cost)
    test_cost_itv          <- mean(sim_test_cost) * cohort_size  * (1 - tst_attr)
    tpt_cost_itv           <- npositives * tpt_cov * tpt_compl * mean(sim_tpt_cost) +
                                npositives * tpt_cov * (1-tpt_compl) * mean(sim_tpt_cost) * tpt_cost_lfup
    camp_cost_itv          <-  mean(sim_camp_cost)
    test_cost              <- 0
    tpt_cost               <- 0
    camp_cost              <- 0

    cost_dist              <- rbind(TBtx = tbtx_cost,
                                    LTBITests = test_cost,
                                    TPT = tpt_cost,
                                    Campaign = camp_cost)

    cost_dist_itv          <- rbind(TBtx = tbtx_cost_itv,
                                    LTBITests = test_cost_itv,
                                    TPT = tpt_cost_itv,
                                    Campaign = camp_cost_itv)

    # Incremental Net Benefit
    lambda   <- seq(0, 50000, 1000)
    inb      <- lambda %*% t(qaly_margin) - t(replicate(length(lambda), cost_margin))
    df_inb   <- as.data.frame(matrixStats::rowQuantiles( inb, probs = c(0.025, 0.5, 0.975)))
    df_inb$x <- lambda

    # Breakdown tables
    coh_tab <- data.frame(
        Baseline = c(
            cohort_size,
            0,
            npositives,
            0,
            0,
            sum(sim_cases_age),
            round(sum(sim_cases_age_ptb - sim_deaths) * frac_post),
            sum(sim_deaths)
        ),

        Intervention = c(
            cohort_size,
            cohort_size,
            npositives,
            npositives * tpt_cov,
            npositives * tpt_cov * tpt_compl,
            sum(sim_cases_age_itv),
            round(sum(sim_cases_age_ptb_itv - sim_deaths_itv) * frac_post),
            sum(sim_deaths_itv)
        )
    )

    rownames(coh_tab) <- c(
        "Total population",
        "LTBI tested",
        "LTBI+",
        "Received TPT",
        "Completed TPT",
        "Active TB cases",
        "PTBLD",
        "TB Deaths"
    )

    # ICER  objects
    treats             <- c("No intervention", "Intervention")
    eff                <- cbind(qaly, qaly_itv)
    cost               <- cbind(cost, cost_itv)
    QALYdist           <- cbind(qaly_loss_dist, qaly_loss_dist_itv)
    colnames(QALYdist) <- treats
    Costdist           <- cbind(cost_dist, cost_dist_itv)
    colnames(Costdist) <- treats

    out <- list(
        cohort_size   = parameters$n,
        time_horizon  = parameters$t_hor,
        will_to_pay   = parameters$will_to_pay,
        treats        = treats,
        eff           = eff,
        cost          = cost,
        bcea_tb       = bcea(eff, cost, ref = 2, interventions = treats),
        ICER          = cost_margin/qaly_margin,
        marginal_cost = cost_margin,
        marginal_qaly = qaly_margin,
        QALYdist      = QALYdist,
        Costdist      = Costdist,
        INB           = df_inb,
        cohort_table  = coh_tab
    )
    out
}
