get_periskope_dataset<-function(data, prevalence_tab, age.categorical){

    cohort_size<-data$cohort_size
    test<-data$test
    cohort_type<-data$cohort_mode
    highburden<-data$burden_100k
    contact_type<-data$contact_type
    time_horizon<-data$t_hor
    prev<-prevalence_tab[,1]/100

    # Sample from categorical distribution of age UK
    # Note: This used to be age.categorical (made from age_uk) in original app
    age_cats <- rownames(age.categorical)
    probs <- c(age.categorical) / sum(age.categorical)
    samps_age <- sample(x = age_cats, size = cohort_size, prob = probs, replace = TRUE)

    # create the base1 table
    # Note: now using named characters
    samps_age <- sort(samps_age) # to match previous version
    lookage <- setNames(c(21L, 36L, 56L, 83L), age_cats)
    base1 <- list2DF(list(
        Age_cat = samps_age,
        Age = lookage[samps_age]
    ))

    base1$result<-"Negative"
    na <- vector("list", length(age_cats))
    for (i in seq_along(age_cats)) {
        index <- which(base1$Age_cat == age_cats[i])
        apos <- round(length(index) * prev[i])
        base1$result[index[seq_len(apos)]] <- "Positive"
        na[[i]] <- index
    }

    npositives<-length(which(base1$result=="Positive"))

    # Results
    base1$pct_qfn <-NA
    base1$pct_tspot <- NA
    base1$pct_tst <- NA
    base1$qfn_result <-NA
    base1$tspot_result <- NA
    base1$tst_result <- NA

    if(test == "QuantiFERON") {
        base1$qfn_result <- base1$result
    } else if (test == "T-SPOT.TB") {
        base1$tspot_result <- base1$result
    } else {
        base1$tst_result <- base1$result
    }

    # Impute result from qualitative result
    base1 <- dplyr::mutate(
        base1,
        pct_testspl1 = dplyr::case_when(
            !is.na(pct_qfn)                       ~ as.integer(pct_qfn),
            !is.na(pct_tspot)                     ~ as.integer(pct_tspot),
            qfn_result == "Positive"              ~ 87L,
            qfn_result == "Negative"              ~ 1L,
            tspot_result == "Positive"            ~ 87L,
            tspot_result == "Borderline positive" ~ 79L,
            tspot_result == "Borderline negative" ~ 76L,
            tspot_result == "Negative"            ~ 1L,
            !is.na(pct_tst)                       ~ as.integer(pct_tst)
        )
    )

    ### Add test result splines (5 knots at fixed positions)
    pct_test_spline5 <- as.data.frame(
        Hmisc::rcspline.eval(
            base1$pct_testspl1,
            knots = c(5, 27.5, 50, 72.5, 95)
        )
    )
    colnames(pct_test_spline5) <- c("pct_testspl2", "pct_testspl3", "pct_testspl4")
    base1 <- cbind(base1,pct_test_spline5)

    ## Age splines (5 knots at fixed positions)
    base1 <- dplyr::rename(base1, agespl1 = Age)
    age_spline5 <- as.data.frame(
        Hmisc::rcspline.eval(
            base1$agespl1,
            knots = c(8, 25, 33.07, 45, 64)
        )
    )
    colnames(age_spline5) <- c("agespl2", "agespl3", "agespl4")
    base1 <- cbind(base1, age_spline5)

    # Status
    base1$contact <- if (cohort_type=="contact") "Yes" else "No"

    base1$indexcase_proximity <-
        if (cohort_type == "contact") contact_type else "Not applicable"

    base1$migrant <- if (cohort_type=="new") "Yes" else "No"

    # Exposure category
    if(cohort_type=="contact"&& contact_type=="household"){

        base1$exposure_cat4b <- "Household, smear+"

    } else if(cohort_type=="contact"&& contact_type=="other"){

        base1$exposure_cat4b <-  "Other contacts"

    }else{

        base1$exposure_cat4b <- "No contact, non-migrant"
        for (i in seq_along(na)) {
            index <- na[[i]]
            apos<-round(length(index) * highburden)
            base1$exposure_cat4b[index[seq_len(apos)]] <- "No contact, migrant"
        }

    }

    base1$months_migrant<-12
    base1$hivpos <- "No"
    base1$transplant<-"No"
    base1$ltbi_treatment<-"No"

    base1 <- dplyr::select(
        base1,
        agespl1, agespl2, agespl3, agespl4,
        pct_testspl1, pct_testspl2, pct_testspl3, pct_testspl4,
        exposure_cat4b, months_migrant,
        hivpos, transplant_assumed = transplant,
        ltbi_treatment, qfn_result, tspot_result
    )

    list(
        df = base1,
        npositives = npositives
    )
}
