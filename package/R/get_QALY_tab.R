get_QALY_tab <- function(r) {

    # TODO - I believe these are parameters in the original code but we are
    #        fixing them. If we are intending to keep them fixed then we can
    #        probably simplify some of the stuff below. TT
    smr <- 1
    qcm <- 1
    time_horizon <- 120

    # TODO - This validation probably wants shifting in to the UI part of the code. TT
    shiny::validate(
        need(r <=0.1, "Please choose a valid discount rate between 0% and 10%"),
        need(r >=0, "Please choose a valid discount rate between 0% and 10%"),
    )

    # pull out internal data frames
    q.male   <- qaly_input$q.male
    q.female <- qaly_input$q.female
    qol      <- qaly_input$qol
    age_bands<- qaly_input$age_bands

    # TODO - To me the following function is equivalent (save inclusion of
    #        data frame as part of function) to commented out version below. I'm
    #        not sure if there was a numerical reason to have the former or if
    #        there was an error in the original implementation? TT
    l_x_est <- function(qx, smr){
        mult <- c(1, (1 - qx[-length(qx)])^smr)
        100000 * cumprod(mult)
    }
    # l_x_est <- function(dat, smr){
    #     # dat = data frame with q(x) vaues
    #     colnames(dat) <- c("x","qx")
    #     dx <- -log(1 - dat$qx)
    #     lx <- numeric(length(dx))
    #     lx[1L] <- 100000
    #     for (i in seq_along(lx)[-1L]) {
    #         lx[i] <- lx[i - 1L] * exp(-dx[i - 1L] * smr)
    #     }
    # }

    q.male <- rename(q.male, q_male = UK)
    q.male <- mutate(q.male, l_male = l_x_est(q_male, smr))

    q.female <- rename(q.female, q_female = UK)
    q.female <- mutate(q.female, l_female = l_x_est(q_female, smr))

    # Note 1: The stats::filter function used below is the equivalent to the
    #         following in data.table:
    #             biglx <- frollmean(c(q.person,0),2,align="left")
    #             q.person[, biglx := ..biglx[-length(..biglx)]]
    #             q.person[, biglx := frollmean(c(q.person,0),2,align="left")
    #
    #         In the original code it was written as a for loop:
    #             for (i in 1:(nrow(q.person)-1))
    #                 q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
    #
    # Note 2: The t_x value is equivalent to the following loop from the
    #         original code:
    #                 for (i in 1:nrow(q.person))
    #                     q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
    #
    qale <- left_join(q.male, q.female, by = "Age")

    qale <- mutate(
        qale,
        p.f = l_female / (l_female + l_male),
        l_person = (p.f * l_female) + ((1 - p.f) * l_male),
        bigl_x = stats::filter(c(l_person, 0), rep(0.5, 2), sides = 1)[-1L],
        t_x = rev(cumsum(rev(bigl_x))),
        LE_x = t_x / l_person
    )

    qale <- left_join(
        select(qale, Age, l_person, bigl_x, t_x, LE_x),
        select(qol, Age, qol_age = UK),
        by = "Age"
    )

    qale <- mutate(
        qale,
        z_x = bigl_x * qol_age * qcm,
        t_adj = rev(cumsum(rev(z_x))),
        qale_x = t_adj / l_person
    )

    qaly.calc <- qale[c("Age","z_x")]
    nr <- nrow(qaly.calc)
    temp.q <- lapply(seq_len(nr), function(i) qaly.calc[i:nr, ])
    temp.q <- bind_rows(temp.q, .id = "column_label")
    temp.q <- mutate(
        temp.q,
        column_label = as.numeric(column_label) - 1,
        b_x = z_x / (1 + r) ^ (Age - column_label),
        index = as.integer(Age <= time_horizon)
    )

    total.b <- summarise(
        temp.q,
        bigb_x = sum(b_x),
        bigb_xfoo = sum(b_x[index == 1L]),
        .by = column_label
    )

    cov <- left_join(qale, total.b, by = join_by(Age == column_label))
    cov <- mutate(cov, dQALY = bigb_xfoo / l_person)
    age_bands <- mutate(age_bands, midpoint = ceiling( (low + high) / 2))
    cov <- inner_join(cov, age_bands, by = join_by(Age == midpoint))
    cov <- select(cov, "Age Group" = "Age band", LE = LE_x, QALE = qale_x, dQALY)

    list(agetab = cov)
}

