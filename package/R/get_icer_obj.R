get_icer_obj<-function(parameters, perisk, model, qol_LE, nsamp=200){


    base1<-perisk$df
    npositives<- perisk$npositives
    n <- nsamp
    n_samples<- n
    cohort_size<-parameters$cohort_size
    time_horizon<-parameters$time_horizon
    will_to_pay=parameters$will_to_pay

    tpt_cov<-parameters$tpt_cov
    tpt_compl<-parameters$tpt_compl
    tpt_cost_lfup<-parameters$tpt_cost_lfup
    dr<-parameters$dr


    #CFR Crofts et al2008
    cfr <- c(0.012,0.012,0.048,0.176)
    frac_eptb<-0.2
    av_tbdur <- 1
    frac_post<-0.25 # https://karger.com/res/article/100/8/751/820992/Post-Tuberculosis-Lung-Disease-Clinical-Review-of


    tpt_eff<-0
    tpt_ae<-0
    if (parameters$tpt == "6INH"){
        tpt_eff <- 0.6
        tpt_ae <- 0.04
    } else if(parameters$tpt =="3HP"){
        tpt_eff <- 0.53
        tpt_ae <- 0.08
    } else if(parameters$tpt =="3RH"){
        tpt_eff <- 0.6
        tpt_ae <- 0.039
    }

    tst_attr<-0
    if (parameters$test == "Tuberculin Skin Test"){

        tst_attr<-parameters$tst_attr

    }



    # Campaign costs
    cost_camp<- 0
    cost_campsd<-  0
    cost_campmin<-  0
    cost_campmax<-  0
    cost_campshape<-  0

    if (parameters$campcost_dist =="Gamma"){
        cost_camp<- parameters$cost_camp_gamma
        cost_campsd<-  parameters$cost_campsd_gamma#(xmax-xmin)/3.92
        cost_campmin<-  NA
        cost_campmax<-  NA
        cost_campshape<-  NA

        xmean<-parameters$cost_camp_gamma
        sd<- parameters$cost_campsd_gamma#(xmax-xmin)/3.92
        shape<- (xmean^2)/(sd^2)
        sim_camp_cost  <- rgamma(n,shape=shape, rate=shape/xmean)

    } else if (parameters$campcost_dist =="PERT"){

        cost_campsd<-  NA
        cost_camp<-  parameters$cost_camp_pert
        cost_campmin<-  parameters$cost_campmin_pert
        cost_campmax<-  parameters$cost_campmax_pert
        cost_campshape<-  parameters$cost_camplam

        xmean<-parameters$cost_camp_pert
        xmin<-parameters$cost_campmin_pert
        xmax<-parameters$cost_campmax_pert
        lam<- parameters$cost_camplam
        sim_camp_cost<-freedom::rpert(n,xmin,xmax,xmean,lam)
    }


    ## test
    cost_test<- 0
    cost_testsd<-  0
    cost_testmin<-  0
    cost_testmax<-  0
    cost_testshape<-  0
    if (parameters$testcost_dist =="Gamma"){

        cost_test<- parameters$cost_test_gamma
        cost_testsd<-  parameters$cost_testsd_gamma
        cost_testmin<-  NA
        cost_testmax<-  NA
        cost_testshape<-  NA

        xmean<-parameters$cost_test_gamma
        sd<- parameters$cost_testsd_gamma# (xmax-xmin)/3.92
        shape<- (xmean^2)/(sd^2)

        if(xmean==0){
            sim_test_cost<-replicate(n,xmean)

        }else{
            sim_test_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
        }

    } else if (parameters$testcost_dist =="PERT"){

        cost_test<-parameters$cost_test_pert
        cost_testsd<-  NA
        cost_testmin<-  parameters$cost_testmin_pert
        cost_testmax<-  parameters$cost_testmax_pert
        cost_testshape<-  parameters$cost_testlam

        xmean<-parameters$cost_test_pert
        xmin<-parameters$cost_testmin_pert
        xmax<-parameters$cost_testmax_pert
        lam<- parameters$cost_testlam
        sim_test_cost<-freedom::rpert(n,xmin,xmax,xmean,lam)

        if(xmean==0){
            sim_test_cost<-replicate(n,xmean)

        }else{
            sim_test_cost<-freedom::rpert(n,xmin,xmax,xmean,lam)
        }



    }



    ## TPT cost
    cost_tpt<- 0
    cost_tptsd<-  0
    cost_tptmin<-  0
    cost_tptmax<-  0
    cost_tptshape<-  0
    if (parameters$tptcost_dist =="Gamma"){
        cost_tpt<- parameters$cost_tpt_gamma
        cost_tptsd<-  parameters$cost_tptsd_gamma
        cost_tptmin<-  NA
        cost_tptmax<-  NA
        cost_tptshape<-  NA
        xmean<-parameters$cost_tpt_gamma
        # xmin<-parameters$cost_tptmin_gamma
        # xmax<-parameters$cost_tptmax_gamma
        sd<- parameters$cost_tptsd_gamma#(xmax-xmin)/3.92
        shape<- (xmean^2)/(sd^2)

        if(xmean==0){
            sim_tpt_cost<-replicate(n,xmean)

        }else{
            sim_tpt_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
        }




    } else if (parameters$tptcost_dist =="PERT"){
        cost_tpt<- parameters$cost_tpt_pert
        cost_tptsd<-  NA
        cost_tptmin<- parameters$cost_tptmin_pert
        cost_tptmax<-  parameters$cost_tptmax_pert
        cost_tptshape<-  parameters$cost_tptlam
        xmean<-parameters$cost_tpt_pert
        xmin<-parameters$cost_tptmin_pert
        xmax<-parameters$cost_tptmax_pert
        lam<- parameters$cost_tptlam

        if(xmean==0){
            sim_tpt_cost<-replicate(n,xmean)

        }else{

            sim_tpt_cost<-freedom::rpert(n,xmin,xmax,xmean,lam)
        }
    }

    ## TB Tx cost
    cost_tbtx<- 0
    cost_tbtxsd<-  0
    cost_tbtxmin<-  0
    cost_tbtxmax<-  0
    cost_tbtxshape<-  0
    if (parameters$tbtxcost_dist =="Gamma"){
        cost_tbtx<- parameters$cost_tbtx_gamma
        cost_tbtxsd<-  parameters$cost_tbtxsd_gamma
        cost_tbtxmin<-  NA
        cost_tbtxmax<-  NA
        cost_tbtxshape<-  NA
        xmean<-parameters$cost_tbtx_gamma
        # xmin<-parameters$cost_tbtxmin_gamma
        # xmax<-parameters$cost_tbtxmax_gamma
        sd<- parameters$cost_tbtxsd_gamma#(xmax-xmin)/3.92
        shape<- (xmean^2)/(sd^2)
        sim_tbtx_cost  <- rgamma(n,shape=shape, rate=shape/xmean)

    } else if (parameters$tbtxcost_dist =="PERT"){
        cost_tbtx<- parameters$cost_tbtx_pert
        cost_tbtxsd<-  NA
        cost_tbtxmin<- parameters$cost_tbtxmin_pert
        cost_tbtxmax<-  parameters$cost_tbtxmax_pert
        cost_tbtxshape<-  parameters$cost_tbtxlam
        xmean<-parameters$cost_tbtx_pert
        xmin<-parameters$cost_tbtxmin_pert
        xmax<-parameters$cost_tbtxmax_pert
        lam<- parameters$cost_tbtxlam
        sim_tbtx_cost<-freedom::rpert(n,xmin,xmax,xmean,lam)
    }



    # TB QOL
    qol_tb<- 0
    qol_tbsd<-  0
    qol_tbmin<-  0
    qol_tbmax<-  0
    qol_tbshape<-  0
    if (parameters$ptbqol_dist =="Beta"){
        qol_tb<- parameters$qol_ptb_beta
        qol_tbsd<-  parameters$qol_ptbsd_beta
        qol_tbmin<-  NA
        qol_tbmax<-  NA
        qol_tbshape<-  NA
        xmean<-parameters$qol_ptb_beta
        # xmin<-parameters$qol_ptbmin_beta
        # xmax<-parameters$qol_ptbmax_beta
        sd<- parameters$qol_ptbsd_beta# (xmax-xmin)/3.92
        pars_beta<-estBetaParams(xmean,sd^2 )
        sim_tb_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)

    } else if (parameters$ptbqol_dist =="PERT"){
        qol_tb<- parameters$qol_ptb_pert
        qol_tbsd<-  NA
        qol_tbmin<-  parameters$qol_ptbmin_pert
        qol_tbmax<-  parameters$qol_ptbmax_pert
        qol_tbshape<- parameters$qol_ptblam_pert
        xmean<-parameters$qol_ptb_pert
        xmin<-parameters$qol_ptbmin_pert
        xmax<-parameters$qol_ptbmax_pert
        lam<- parameters$qol_ptblam_pert
        sim_tb_qol<-freedom::rpert(n,xmin,xmax,xmean,lam)

    }


    #EPTB QoL
    qol_eptb<- 0
    qol_eptbsd<-  0
    qol_eptbmin<-  0
    qol_eptbmax<-  0
    qol_eptbshape<-  0
    if (parameters$eptbqol_dist =="Beta"){
        qol_eptb<- parameters$qol_eptb_beta
        qol_eptbsd<- parameters$qol_eptbsd_beta
        qol_eptbmin<-  NA
        qol_eptbmax<-  NA
        qol_eptbshape<- NA
        xmean<-parameters$qol_eptb_beta
        # xmin<-parameters$qol_eptbmin_beta
        # xmax<-parameters$qol_eptbmax_beta
        sd<- parameters$qol_eptbsd_beta#(xmax-xmin)/3.92
        pars_beta<-estBetaParams(xmean,sd^2 )
        sim_eptb_qol  <- rbeta(n,pars_beta$alpha, pars_beta$beta)

    } else if (parameters$eptbqol_dist =="PERT"){
        qol_eptb<- parameters$qol_eptb_pert
        qol_eptbsd<-  NA
        qol_eptbmin<-  parameters$qol_eptbmin_pert
        qol_eptbmax<-  parameters$qol_eptbmax_pert
        qol_eptbshape<- parameters$qol_eptblam_pert
        xmean<-parameters$qol_eptb_pert
        xmin<-parameters$qol_eptbmin_pert
        xmax<-parameters$qol_eptbmax_pert
        lam<- parameters$qol_eptblam_pert
        sim_eptb_qol <-freedom::rpert(n,xmin,xmax,xmean,lam)

    }

    # POstTB QoL
    qol_postb<- 0
    qol_postbsd<-  0
    qol_postbmin<-  0
    qol_postbmax<-  0
    qol_postbshape<-  0
    if (parameters$postqol_dist =="Beta"){
        qol_postb<- parameters$qol_post_beta
        qol_postbsd<-  parameters$qol_postsd_beta
        qol_postbmin<-  NA
        qol_postbmax<-  NA
        qol_postbshape<-  NA
        xmean<-parameters$qol_post_beta
        # xmin<-parameters$qol_postmin_beta
        # xmax<-parameters$qol_postmax_beta
        sd<- parameters$qol_postsd_beta#(xmax-xmin)/3.92
        pars_beta<-estBetaParams(xmean,sd^2 )
        sim_post_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)

    } else if (parameters$postqol_dist =="PERT"){
        qol_postb<- parameters$qol_post_pert
        qol_postbsd<-  NA
        qol_postbmin<-  parameters$qol_postmin_pert
        qol_postbmax<-  parameters$qol_postmax_pert
        qol_postbshape<-  parameters$qol_postlam_pert
        xmean<-parameters$qol_post_pert
        xmin<-parameters$qol_postmin_pert
        xmax<-parameters$qol_postmax_pert
        lam<- parameters$qol_postlam_pert
        sim_post_qol<-freedom::rpert(n,xmin,xmax,xmean,lam)

    }

    #AE TPT QOL
    if (parameters$aeqol_dist =="Beta"){

        xmean<-parameters$qol_ae_beta
        # xmin<-parameters$qol_aemin_beta
        # xmax<-parameters$qol_aemax_beta
        sd<- parameters$qol_aesd_beta#(xmax-xmin)/3.92
        pars_beta<-estBetaParams(xmean,sd^2 )
        sim_ae_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)

    } else if (parameters$aeqol_dist =="PERT"){

        xmean<-parameters$qol_ae_pert
        xmin<-parameters$qol_aemin_pert
        xmax<-parameters$qol_aemax_pert
        lam<- parameters$qol_aelam_pert
        sim_ae_qol<-freedom::rpert(n,xmin,xmax,xmean,lam)

    }






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
    predictions<-matrix(0,nrow = cohort_size, ncol = time_horizon)


    #for (ii in 2:time_horizon){
    ii<-time_horizon
    base1$studytime <- (365*ii)-42
    preds <- as.data.frame((predict(model, base1, type="fail", se.fit=T)))
    meanx<-preds[,1]
    sd<- (preds[,3] - preds[,2])/3.92

    predictions[,ii]<-rnorm(cohort_size,meanx,sd)
    #  }

    # Draw TB deaths from cases
    exp_prob<-replicate(time_horizon,cfr)

    binom_draw<- function(n,p){
        round(n*p) #rbinom(n=1, size=n, prob=p)
    }


    p<-matrix(runif(cohort_size*time_horizon), nrow=cohort_size)
    logical_matrix <-predictions# p < predictions
    logical_matrix_itv <-predictions*tpt_eff# p < predictions*tpt_eff


    # Cumulative TB cases by age
    tmp<-data.frame(age=base1$agespl1,logical_matrix)
    sim_cases_age<-tmp %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., na.rm = TRUE)))

    tmp<-data.frame(age=base1$agespl1,logical_matrix_itv)
    sim_cases_age_itv<-tmp %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., na.rm = TRUE)))


    sim_cases_age<-sim_cases_age[,2:ncol(sim_cases_age)]
    sim_cases_age_eptb<- round(sim_cases_age * frac_eptb)
    sim_cases_age_ptb <- sim_cases_age-sim_cases_age_eptb

    #c<-mapply(binom_draw,sim_cases_age_ptb,exp_prob)
    sim_deaths <- sim_cases_age_ptb*cfr#matrix(c, nrow = nrow(sim_cases_age_ptb), ncol = ncol(logical_matrix))


    sim_cases_age_itv<-sim_cases_age_itv[,2:ncol(sim_cases_age_itv)]
    sim_cases_age_eptb_itv<- round(sim_cases_age_itv * frac_eptb)
    sim_cases_age_ptb_itv <- sim_cases_age_itv-sim_cases_age_eptb_itv
    #  c<-mapply(binom_draw,sim_cases_age_ptb_itv,exp_prob)
    sim_deaths_itv <- sim_cases_age_ptb_itv*cfr#matrix(c, nrow = nrow(sim_cases_age_ptb_itv), ncol = ncol(logical_matrix_itv))



    baseline_qaly<- table(base1$agespl1) * qol_LE
    qaly_loss_deaths<- colSums(sim_deaths * qol_LE)
    qaly_loss_deaths_itv<- colSums(sim_deaths_itv * qol_LE)



    qaly<-matrix(0,nrow = n_samples, ncol=time_horizon)
    qaly_itv<-matrix(0,nrow = n_samples, ncol=time_horizon)
    qaly_margin<-matrix(0,nrow = n_samples, ncol=time_horizon)

    cost<-matrix(0,nrow = n_samples, ncol=time_horizon)
    cost_itv<-matrix(0,nrow = n_samples, ncol=time_horizon)
    cost_margin<-matrix(0,nrow = n_samples, ncol=time_horizon)


    tmp<-matrix(1,nrow = 1, ncol=time_horizon)

    discount_matrix<-((tmp)/((1 + dr)^ seq_len(ncol(tmp)) ))
    qol_exp<-matrix(rep(qol_LE,n_samples), nrow = nrow(sim_cases_age_ptb_itv), ncol = time_horizon )


    qaly_loss_ptb <- t(t(discount_matrix * colSums(sim_cases_age_ptb))  %*%   (sim_tb_qol * av_tbdur))
    qaly_loss_eptb<- t(t(discount_matrix * colSums(sim_cases_age_eptb))  %*%   (sim_eptb_qol * av_tbdur))
    qaly_loss_ptb_itv <- t(t(discount_matrix * colSums(as.data.frame(sim_cases_age_ptb_itv)))  %*%   (sim_tb_qol * av_tbdur))
    qaly_loss_eptb_itv<- t(t(discount_matrix * colSums(as.data.frame(sim_cases_age_eptb_itv)))  %*%   (sim_eptb_qol * av_tbdur))
    qaly_loss_post     <-t(colSums((sim_cases_age_ptb - sim_deaths)* frac_post * qol_LE)  %*% t(sim_post_qol))
    qaly_loss_post_itv <-t(colSums(as.data.frame((sim_cases_age_ptb_itv - sim_deaths_itv)* frac_post * qol_LE))  %*% t(sim_post_qol))
    qaly_loss_AE_itv<- npositives * tpt_cov  * tpt_ae * sim_ae_qol

    deaths_loss<-t(replicate(n_samples,qaly_loss_deaths))
    deaths_loss_itv<-t(replicate(n_samples,qaly_loss_deaths_itv))

    qaly<-     sum(baseline_qaly)- (qaly_loss_ptb + qaly_loss_eptb + qaly_loss_post + deaths_loss)
    qaly_itv<- sum(baseline_qaly)- (qaly_loss_ptb_itv + qaly_loss_eptb_itv +
                                        qaly_loss_post_itv + deaths_loss_itv+qaly_loss_AE_itv)

    qaly_margin<-qaly_itv-qaly

    #Costs


    tbtx_cost<- t(replicate(n_samples, drop(discount_matrix) * drop(colSums(sim_cases_age)))*  t(replicate(time_horizon, sim_tbtx_cost)))
    tbtx_cost_itv<- t(replicate(n_samples, drop(discount_matrix) * drop(colSums(sim_cases_age_itv)))*  t(replicate(time_horizon, sim_tbtx_cost)))


    itv_start_cost<-replicate(time_horizon, sim_test_cost * cohort_size *(1-tst_attr) +
                                  npositives * tpt_cov * tpt_compl * sim_tpt_cost +
                                  npositives * tpt_cov * (1-tpt_compl) * sim_tpt_cost * tpt_cost_lfup + sim_camp_cost)



    cost<- tbtx_cost
    cost_itv<- tbtx_cost_itv + itv_start_cost
    cost_margin<- cost_itv- cost





    # QALY loss composition

    qaly_loss_ptb <- discount_matrix * colSums( sim_cases_age_ptb *
                                                    mean(sim_tb_qol) * av_tbdur)
    qaly_loss_eptb <- discount_matrix * colSums( sim_cases_age_eptb *
                                                     mean(sim_eptb_qol) * av_tbdur)


    qaly_loss_ptb_itv <- discount_matrix * colSums(as.data.frame(sim_cases_age_ptb_itv *
                                                                     mean(sim_tb_qol) * av_tbdur))


    qaly_loss_eptb_itv <- discount_matrix * colSums(as.data.frame(sim_cases_age_eptb_itv *
                                                                      mean(sim_eptb_qol) * av_tbdur))

    qaly_loss_post<- colSums((sim_cases_age_ptb - sim_deaths)* frac_post *(qol_LE - qol_LE*(1-mean(sim_post_qol))))

    qaly_loss_post_itv<- colSums(as.data.frame((sim_cases_age_ptb_itv -
                                                    sim_deaths_itv)  * frac_post *(qol_LE - qol_LE*(1-mean(sim_post_qol)))))


    qaly_loss_AE_itv <- npositives * tpt_cov  * tpt_ae * mean(sim_ae_qol)

    qaly_loss_AE <- 0

    qaly_loss_dist<-rbind(Death=qaly_loss_deaths,
                          PTB=qaly_loss_ptb,
                          EPTB=qaly_loss_eptb,
                          POstTB=qaly_loss_post,
                          AE=qaly_loss_AE)


    qaly_loss_dist_itv<-rbind(Death=qaly_loss_deaths_itv,
                              PTB=qaly_loss_ptb_itv,
                              EPTB=qaly_loss_eptb_itv,
                              PostTB=qaly_loss_post_itv,
                              AE=qaly_loss_AE_itv)



    # Cost composition
    tbtx_cost<-as.numeric(discount_matrix * colSums(sim_cases_age ) *  mean(sim_tbtx_cost))
    tbtx_cost_itv<- as.numeric(discount_matrix * colSums(sim_cases_age_itv) *   mean(sim_tbtx_cost))


    test_cost_itv<- mean(sim_test_cost) * cohort_size  *(1-tst_attr)
    tpt_cost_itv<-  npositives * tpt_cov * tpt_compl * mean(sim_tpt_cost) +
        npositives * tpt_cov * (1-tpt_compl) * mean(sim_tpt_cost) * tpt_cost_lfup
    camp_cost_itv<-  mean(sim_camp_cost)

    test_cost<- 0
    tpt_cost<- 0
    camp_cost<- 0

    cost_dist<- rbind(TBtx =tbtx_cost,
                      LTBITests=test_cost,
                      TPT = tpt_cost,
                      Campaign=camp_cost)

    cost_dist_itv<- rbind(TBtx=tbtx_cost_itv,
                          LTBITests=test_cost_itv,
                          TPT = tpt_cost_itv,
                          Campaign=camp_cost_itv)




    # Incremental Net Benefit
    lambda<-seq(0,50000,1000)
    inb<-lambda%*%t(qaly_margin[,time_horizon]) - t(replicate(length(lambda),cost_margin[,time_horizon]))

    df_inb <- as.data.frame(
        matrixStats::rowQuantiles(inb,
                     probs = c(0.025, 0.5, 0.975)
        )
    )
    df_inb$x<-lambda


    # Breakdown tables



    coh_tab<-data.frame(
        Baseline    =c(cohort_size,
                       0,
                       npositives,
                       0,
                       0,
                       colSums(sim_cases_age)[time_horizon],
                       round(colSums(sim_cases_age_ptb - sim_deaths)[time_horizon]* frac_post),
                       colSums(sim_deaths)[time_horizon]
        ),
        Intervention=c(cohort_size,
                       cohort_size,
                       npositives,
                       npositives * tpt_cov,
                       npositives * tpt_cov * tpt_compl,
                       colSums(sim_cases_age_itv)[time_horizon],
                       round(colSums(sim_cases_age_ptb_itv - sim_deaths_itv)[time_horizon]* frac_post),
                       colSums(sim_deaths_itv)[time_horizon]
        )

    )

    rownames(coh_tab)<-paste<-c("Total population",
                                "LTBI tested",
                                "LTBI+",
                                "Received TPT",
                                "Completed TPT",
                                "Active TB cases",
                                "PTBLD",
                                "TB Deaths")




    # ICER  objects
    treats=c("No intervention", "Intervention")
    eff=cbind(qaly[,time_horizon],qaly_itv[,time_horizon])
    cost=cbind(cost[,time_horizon],cost_itv[,time_horizon])

    QALYdist=cbind(qaly_loss_dist[,time_horizon],qaly_loss_dist_itv[,time_horizon])
    colnames(QALYdist)<-paste(treats)

    Costdist=cbind(cost_dist[,time_horizon],cost_dist_itv[,time_horizon])
    colnames(Costdist)<-paste(treats)



    out <-list(
        cohort_size=parameters$n,
        time_horizon=parameters$t_hor,
        will_to_pay=parameters$will_to_pay,
        treats=treats,
        eff=eff,
        cost=cost,
        bcea_tb=bcea(eff,cost, ref=2,interventions=treats),
        ICER =cost_margin/qaly_margin,
        marginal_cost=cost_margin,
        marginal_qaly=qaly_margin,
        QALYdist=QALYdist,
        Costdist=Costdist,
        INB=df_inb,
        cohort_table=coh_tab
    )

    return(out)
}
