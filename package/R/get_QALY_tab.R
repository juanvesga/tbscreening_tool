# TODO - This seems a little odd. Let's discuss further. TT
# NOTE - Currently assumes data.table input. TT
get_QALY_tab<-function(parameters, qaly_input) {

    q.male   <- qaly_input$q.male
    q.female <- qaly_input$q.female
    qol      <- qaly_input$qol
    age_bands<- qaly_input$age_bands

    country <- "UK"
    smr <- 1
    qcm <- 1
    r <- parameters$dr
    time_horizon<-120#parameters$time_horizon Seee full LE

    shiny::validate(
        need(parameters$dr <=0.1, "Please choose a valid discount rate between 0% and 10%"),
        need(parameters$dr >=0, "Please choose a valid discount rate between 0% and 10%"),
    )

    myvector <- c("Age",country)

    l_x_est <- function(dt, countr, smr){
        ## dt = data table with q(x) vaues
        ## country = selected country
        ## smr = smr
        myvector <- c("Age",countr)

        y <- dt[, ..myvector]
        colnames(y) <- c("x","q_x")

        y[ , d_x := -log(1-y$q_x)]

        y[ 1, l_x := 100000]

        for (i in 2:nrow(y)){
            y[i, l_x := y$l_x[[i-1]] *
                  exp((-y$d_x[[i-1]])*smr)]
        }
        return(y)
    }

    q.male <- l_x_est(q.male, country, smr)
    q.female <- l_x_est(q.female, country, smr)

    q.person <- merge(q.male, q.female, by="x")
    colnames(q.person) <- c("x","q_male","d_male","l_male",
                            "q_female","d_female","l_female")
    q.person[ , p.f := l_female/(l_female+l_male)]
    q.person[ , l_person := (p.f*l_female)+
                  ((1-p.f)*l_male)]

    for (i in 1:(nrow(q.person)-1)){
        q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
    }

    q.person[nrow(q.person), bigl_x := (q.person$l_person[[nrow(q.person)]])/2]

    for (i in 1:nrow(q.person)){
        q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
    }

    q.person[ , LE_x := t_x/l_person]

    ########### calculating QALE ########
    myvector.qol <- c("low","high",country)

    dt.qol <- qol[, ..myvector.qol]
    colnames(dt.qol) <- c("low","high","qol_age")


    qale <- q.person[dt.qol, on = .(x >= low, x <= high), nomatch = 0,
                     .(x.x, l_person, bigl_x, t_x, LE_x,qol_age)]

    qale[ , z_x := bigl_x*qol_age*qcm]

    for (i in 1:nrow(qale)){
        qale[i , t_adj := sum(qale$z_x[i:nrow(qale)])]
    }

    qale[ , qale_x := t_adj/l_person]

    qaly.calc <- qale[ , c("x.x","z_x")]

    temp.q <- list()
    for (i in 1:nrow(qaly.calc)){
        temp.q[[i]] <- qaly.calc[i:nrow(qaly.calc),]
    }

    temp.q <- dplyr::bind_rows(temp.q, .id = "column_label")
    temp.q %>% setDT() ## creating a copy as otherwise there is a warning
    ## message (still runs but just for "clean" code), so this stops attempts of .internal.selfref detected
    temp.q_copy <- copy(temp.q)
    temp.q_copy[ , column_label := as.numeric(column_label)-1]

    temp.q_copy[ , b_x := z_x/((1+r))^(x.x-(column_label))] ## n.b x.x = u and column_label = x in the corresponding formulae in the CodeBook


    temp.q_copy<-temp.q_copy %>%
        dplyr::mutate(index=1*(x.x<=time_horizon))


    total.b <- temp.q_copy[ , .(bigb_x = sum(b_x),
                                bigb_xfoo = sum(b_x[index==1])),
                            by = column_label]


    #total.b <- temp.q_copy[,.(bigb_x=sum(b_x)), by=column_label]

    colnames(total.b) <- c("x.x","bigb_x","bigb_xfoo")
    qale <- merge(qale, total.b, by="x.x")

    qale[ , dQALY := bigb_xfoo/l_person]

    ######### calculating covid19 loss #######


    age_bands[ , midpoint := ceiling((low+high)/2)]

    cov <- merge(qale, age_bands, by.x="x.x", by.y="midpoint", all=FALSE)

    ### ADDING AGE GROUP BREAKDOWN TABLE
    cov[,"Age Group":=paste(cov[,low],cov[,high],sep="-")]
    setnames(cov, old=c("LE_x","qale_x","dQALY"),
             new=c("LE","QALE","dQALY"))

    agetab <- cov[ , c("Age Group",
                       "LE","QALE","dQALY")]

    return(list( agetab=agetab))
}
