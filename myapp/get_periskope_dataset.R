
get_periskope_dataset<-function(data,prevalence_tab,age.categorical){

 
cohort_size<-data$cohort_size
test<-data$test 

cohort_type<-data$cohort_mode
highburden<-data$burden_100k
contact_type<-data$contact_type


#country <- data$country
time_horizon<-data$t_hor

prev<-prevalence_tab[,1]/100

# Global
lookage <- read.table(header = TRUE,
                      stringsAsFactors = FALSE,
                      text="Age_cat Age
  16-35         21
  36-45         36
  46-65         56
  65+         83")

# Sample from categorical distribution
samps_age<-random.Categorical(age.categorical, cohort_size)

largetable <- data.frame(
  Age_cat = samps_age,
  stringsAsFactors = FALSE)

base1 <- (merge(largetable,lookage,  by = 'Age_cat'))
base1$result<-"Negative"

na1<-which(base1$Age_cat=="16-35")
na2<-which(base1$Age_cat=="36-45")
na3<-which(base1$Age_cat=="46-65")
na4<-which(base1$Age_cat=="65+")

a1pos<-round(length(na1)*prev[1])
base1$result[na1[1:a1pos]]<-"Positive"

a2pos<-round(length(na2)*prev[2])
base1$result[na2[1:a2pos]]<-"Positive"

a3pos<-round(length(na3)*prev[3])
base1$result[na3[1:a3pos]]<-"Positive"

a4pos<-round(length(na4)*prev[4])
base1$result[na4[1:a4pos]]<-"Positive"

npositives<-length(which(base1$result=="Positive"))



# Results
base1$pct_qfn <-NA
base1$pct_tspot <- NA
base1$pct_tst <- NA
base1$qfn_result <-NA
base1$tspot_result <- NA
base1$tst_result <- NA

if(test=="QuantiFERON"){
  base1$qfn_result <-base1$result
}else if(test=="T-SPOT.TB"){
  base1$tspot_result <-base1$result
  
}else{
  base1$tst_result <-base1$result
  
}
# Impute result from qualitative result
base1 <- base1 %>% mutate(pct_testspl1 = case_when(!is.na(pct_qfn) ~ as.integer(pct_qfn),
                                                   !is.na(pct_tspot) ~ as.integer(pct_tspot),
                                                   qfn_result=="Positive" ~ as.integer(87),
                                                   qfn_result=="Negative" ~ as.integer(1),
                                                   tspot_result=="Positive" ~ as.integer(87),
                                                   tspot_result=="Borderline positive" ~ as.integer(79),
                                                   tspot_result=="Borderline negative" ~ as.integer(76),
                                                   tspot_result=="Negative" ~ as.integer(1),
                                                   !is.na(pct_tst) ~ as.integer(pct_tst)))

### Add test result splines (5 knots at fixed positions)
pct_test_spline5 <- as.data.frame(rcspline.eval(base1$pct_testspl1, knots = c(5, 27.5, 50, 72.5, 95)))
colnames(pct_test_spline5) <- c("pct_testspl2", "pct_testspl3", "pct_testspl4")
base1 <- data.frame(base1,pct_test_spline5)

## Age splines (5 knots at fixed positions)
base1 <- base1 %>% rename(agespl1=Age)
age_spline5 <- as.data.frame(rcspline.eval(base1$agespl1, knots = c(8, 25, 33.07, 45, 64)))
colnames(age_spline5) <- c("agespl2", "agespl3", "agespl4")
base1 <- data.frame(base1,age_spline5)



# Status
base1$contact<-if(cohort_type=="contact"){ "Yes" }else{ "No"}
base1$indexcase_proximity<-if(cohort_type=="contact")
{ contact_type }else{ "Not applicable"}
base1$migrant<-if(cohort_type=="new"){ "Yes" }else{ "No"}




# Exposure category 

if(cohort_type=="contact"&& contact_type=="household"){
  
  base1$exposure_cat4b <- "Household, smear+"
  
} else if(cohort_type=="contact"&& contact_type=="other"){
  
  base1$exposure_cat4b <-  "Other contacts"
}else{
  
  base1$exposure_cat4b <- "No contact, non-migrant"
  
  a1pos<-round(length(na1)*highburden)
  base1$exposure_cat4b[na1[1:a1pos]]<-"No contact, migrant"
  
  a2pos<-round(length(na2)*highburden)
  base1$exposure_cat4b[na2[1:a2pos]]<-"No contact, migrant"
  
  a3pos<-round(length(na3)*highburden)
  base1$exposure_cat4b[na3[1:a3pos]]<-"No contact, migrant"
  
  a4pos<-round(length(na4)*highburden)
  base1$exposure_cat4b[na4[1:a4pos]]<-"No contact, migrant"
  
}

base1$months_migrant<-12
base1$hivpos <- "No"
base1$transplant<-"No"
base1$ltbi_treatment<-"No"

base1 <- base1 %>% select(agespl1, agespl2, agespl3, agespl4,
                          pct_testspl1, pct_testspl2, pct_testspl3, pct_testspl4,
                          exposure_cat4b, months_migrant,
                          hivpos, transplant,
                          ltbi_treatment, qfn_result, tspot_result) %>%
  rename(transplant_assumed=transplant)

res<-list()
res$df=base1
res$npositives=npositives
return(res)
}