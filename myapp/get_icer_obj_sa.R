get_icer_obj_sa<-function(parameters, perisk, model, qol_LE, var, min_sa, max_sa){
  
  
  base1<-perisk$df
  npositives<- perisk$npositives
  n_samples <- 100
  n<-n_samples 
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
    
    xmean<-parameters$cost_camp_gamma
    sim_camp_cost  <- replicate(n,xmean)
    
  } else if (parameters$campcost_dist =="PERT"){
    
    xmean<-parameters$cost_camp_pert
    sim_camp_cost<-replicate(n,xmean)
  }
  
  
  ## test
  
  
  if (var=="ltbi_c"){
    
    sim_test_cost= seq(min_sa,max_sa,length.out=n)
    
    
  } else{
    
    
    if (parameters$testcost_dist =="Gamma"){
      
      xmean<-parameters$cost_test_gamma
      sim_test_cost  <- replicate(n,xmean)
      
    } else if (parameters$testcost_dist =="PERT"){
      
      xmean<-parameters$cost_test_pert
      sim_test_cost<-replicate(n,xmean)
    }
  }
  
  
  ## TPT cost
  
  if (var=="tpt_c"){
    
    sim_tpt_cost= seq(min_sa,max_sa,length.out=n)
    
    
  } else{
    
    
    if (parameters$tptcost_dist =="Gamma"){
      
      xmean<-parameters$cost_tpt_gamma
      sim_tpt_cost  <- replicate(n,xmean)
      
    } else if (parameters$tptcost_dist =="PERT"){
      xmean<-parameters$cost_tpt_pert
      sim_tpt_cost<-replicate(n,xmean)
    }
  }
  
  ## TB Tx cost
  if (parameters$tbtxcost_dist =="Gamma"){
    xmean<-parameters$cost_tbtx_gamma
    sim_tbtx_cost  <- replicate(n,xmean)
    
  } else if (parameters$tbtxcost_dist =="PERT"){
    xmean<-parameters$cost_tbtx_pert
    sim_tbtx_cost<-replicate(n,xmean)
  }
  
  
  
  # TB QOL
  if (parameters$ptbqol_dist =="Beta"){
    xmean<-parameters$qol_ptb_beta
    sim_tb_qol <- replicate(n,xmean)
    
  } else if (parameters$ptbqol_dist =="PERT"){
    xmean<-parameters$qol_ptb_pert
    sim_tb_qol<-replicate(n,xmean)
  }
  
  
  #EPTB QoL
  if (parameters$eptbqol_dist =="Beta"){
    xmean<-parameters$qol_eptb_beta
    sim_eptb_qol  <-replicate(n,xmean)
    
  } else if (parameters$eptbqol_dist =="PERT"){
    xmean<-parameters$qol_eptb_pert
    sim_eptb_qol <-replicate(n,xmean)
    
  }
  
  # POstTB QoL
  if (parameters$postqol_dist =="Beta"){
    xmean<-parameters$qol_post_beta
    sim_post_qol <- replicate(n,xmean)
    
  } else if (parameters$postqol_dist =="PERT"){
    xmean<-parameters$qol_post_pert
    sim_post_qol<-replicate(n,xmean)
    
  }
  
  #AE TPT QOL
  if (parameters$aeqol_dist =="Beta"){
    
    xmean<-parameters$qol_ae_beta
    sim_ae_qol <- replicate(n,xmean)
    
  } else if (parameters$aeqol_dist =="PERT"){
    
    xmean<-parameters$qol_ae_pert
    sim_ae_qol<-replicate(n,xmean)
    
  }
  
  
  
  
  # Yearly predictions
  predictions<-matrix(0,nrow = cohort_size, ncol = time_horizon)
  
  
  for (ii in 2:time_horizon){
    base1$studytime <- (365*ii)-42
    preds <- as.data.frame((predict(model, base1, type="fail", se.fit=T)))
    meanx<-preds[,1] 
    sd<- (preds[,3] - preds[,2])/3.92
    
    predictions[,ii]<-rnorm(cohort_size,meanx,sd)
  }
  
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
    group_by(age) %>%
    summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  
  tmp<-data.frame(age=base1$agespl1,logical_matrix_itv)
  sim_cases_age_itv<-tmp %>%
    group_by(age) %>%
    summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  
  
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
  
  
  
  # Incremental Net Benefit
  lambda<-seq(0,50000,1000)
  inb<-lambda%*%t(qaly_margin[,time_horizon]) - t(replicate(length(lambda),cost_margin[,time_horizon]))
  
  df_inb <- as.data.frame(
    rowQuantiles(inb,
                 probs = c(0.025, 0.5, 0.975)
    )
  )
  df_inb$x<-lambda
  
  
  
  if(var=="ltbi_c"){
    lab_plot="LTBI test"
    yvals = sim_test_cost
  }else
  {
    lab_plot="TPT"
    yvals=sim_tpt_cost
  }
  
  out <-list(
    ICER =cost_margin[,time_horizon]/qaly_margin[,time_horizon],
    marginal_cost=cost_margin[,time_horizon],
    marginal_qaly=qaly_margin[,time_horizon],
    INB=df_inb,
    lab=lab_plot,
    y=yvals
  )
  
  return(out)
}