get_periskope_dataset<-function(data, prevalence_tab, age.categorical){

    cohort_size  <- data$cohort_size
    test         <- data$test
    cohort_type  <- data$cohort_mode
    highburden   <- data$burden_100k
    contact_type <- data$contact_type
    time_horizon <- data$t_hor
    prev         <- prevalence_tab[,1]/100

    # Sample from categorical distribution of age UK
    # Note: This used to be age.categorical (made from age_uk) in original app
    age_cats     <- rownames(age.categorical)
    probs        <- c(age.categorical) / sum(age.categorical)

    # TODO - How do you want to handle the randomness? TT
    samps_age    <- sample(x = age_cats, size = cohort_size, prob = probs, replace = TRUE)

    # create the base1 table
    # Note: now using named characters
    samps_age    <- sort(samps_age) # to match previous version
    lookage      <- c(
        "16-35" = 21,
        "36-45" = 36L,
        "46-65" = 56L,
        "65+"   = 83L
    )

    age <- lookage[samps_age]
    result <- rep("Negative", times = cohort_size)
    na <- vector("list", length(age_cats))
    for (i in seq_along(age_cats)) {
        index <- samps_age == age_cats[[i]]
        apos <- round(sum(index) * prev[[i]])
        result[index][seq_len(apos)] <- "Positive"
        na[[i]] <- index
    }
    npositives <- sum(result == "Positive")

    # Results
    if(test == "QuantiFERON") {
        qfn_result <- result
        tspot_result <- rep(NA_character_, times = cohort_size)
    } else if (test == "T-SPOT.TB") {
        tspot_result <- result
        qfn_result <- rep(NA_character_, times = cohort_size)
    }

    # Impute result from qualitative result
    pct_testspl1 <- case_when(
        qfn_result == "Positive"              ~ 87L,
        qfn_result == "Negative"              ~ 1L,
        tspot_result == "Positive"            ~ 87L,
        tspot_result == "Negative"            ~ 1L
    )

    ### Add test result splines (5 knots at fixed positions)
    pct_test_spline5 <- as.data.frame(
        Hmisc::rcspline.eval(pct_testspl1, knots = c(5, 27.5, 50, 72.5, 95))
    )
    colnames(pct_test_spline5) <- c("pct_testspl2", "pct_testspl3", "pct_testspl4")

    ## Age splines (5 knots at fixed positions)
    age_spline5 <- as.data.frame(
        Hmisc::rcspline.eval(age, knots = c(8, 25, 33.07, 45, 64))
    )
    colnames(age_spline5) <- c("agespl2", "agespl3", "agespl4")

    # Status
    contact <- if (cohort_type=="contact") "Yes" else "No"
    indexcase_proximity <- if (cohort_type == "contact") contact_type else "Not applicable"
    migrant <- if (cohort_type == "new") "Yes" else "No"

    # Exposure category
    if (cohort_type == "contact" && contact_type == "household") {
        exposure_cat4b <- "Household, smear+"
    } else if (cohort_type=="contact"&& contact_type == "other"){
        exposure_cat4b <-  "Other contacts"
    } else {
        exposure_cat4b <- "No contact, non-migrant"
        for (i in seq_along(na)) {
            index <- na[[i]]
            apos<-round(sum(index) * highburden)
            exposure_cat4b[index][seq_len(apos)] <- "No contact, migrant"
        }
    }

    base1 <- data.frame(
        agespl1 = age, age_spline5,
        pct_testspl1, pct_test_spline5,
        exposure_cat4b,
        months_migrant = 12,
        hivpos = "No",
        transplant_assumed = "No",
        ltbi_treatment = "No",
        qfn_result, tspot_result
    )

    list(df = base1, npositives = npositives)
}
