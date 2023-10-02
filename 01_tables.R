# script_01 for Replication Excercise of Guo (2018): The Costs of Patronage: Evidence from the British Empire
# author (of replication): Andreas Chmielowski
# purpose: Recreation of tables
# PhD Economics, Gothenburg University
#--------------------------------------

### table 1 (panel A): Governor characteristics

v_names_A <- c("peerage",
             "civilservant",
             "military",
             "politician",
             "eton",
             "oxford",
             "cambridge",
             "age_entry",
             "tenure",
             "no_colonies",
             "full",
             "aid")     # select variables for table

panelA_prep <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A)) |>
  filter(full == 1) |>
  summarise_all(funs(mean), na.rm = F)    # mean by governor

colSums(!is.na(panelA_prep))    # number of non-NA entries by column (to compare w Stata)

panelA_pooled <- panelA_prep |> 
  dplyr::select(-aid) |>
  group_by(full) |>
  summarise_all(funs(mean, sd), na.rm = T) # ready summary table (pooled), mean of means

panelA_pooled_obs <- nrow(panelA_prep) # no of obs pooled

panelA_prepall <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A)) |>
  summarise_all(funs(mean), na.rm = F)    # mean by governor, also non-full

colSums(!is.na(panelA_prep))    # number of non-NA entries by column (to compare w Stata)

panelA_pooledall <- panelA_prepall |> 
  dplyr::select(-aid) |>
  summarise_all(funs(mean), na.rm = T) # ready summary table (pooled), mean of means

panelA_pooled_obsall <- nrow(panelA_prep) # no of obs pooled

panelA_1860 <- patronage |> 
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1860) |>
  summarise_all(funs(mean), na.rm = T) |>
  dplyr::select(-c(full, aid, year))               # ready table 1860

panelA_1860_obs <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1860) |>
  nrow()                          # no of obs 1860

panelA_1900 <- patronage |> 
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1900) |>
  summarise_all(funs(mean), na.rm = T) |>
  dplyr::select(-c(full, aid, year))          # ready table 1900

panelA_1900_obs <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1900) |>
  nrow()                          # no of obs 1900

panelA_1930 <- patronage |> 
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1930) |>
  summarise_all(funs(mean), na.rm = T) |>
  dplyr::select(-c(full, aid, year))    # ready table 1930

panelA_1930_obs <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1930) |>
  nrow()                          # no of obs 1930

panelA_1960 <- patronage |> 
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1960) |>
  summarise_all(funs(mean), na.rm = T) |>
  dplyr::select(-c(full, aid, year))      # ready table 1960

panelA_1960_obs <- patronage |> 
  group_by(aid) |>
  dplyr::select(all_of(v_names_A), year) |>
  filter(full == 1, year == 1960) |>
  nrow()                          # no of obs 1960

 
## table 1 (panel B): Colony characteristics

# create new variable
patronage <- patronage |>
  mutate(share_customs_revenue = exp(log_rev_customs_gbp)/exp(log_rev_total_gbp))

v_names_B <- c("log_rev_total_gbp",
             "share_customs_revenue",
             "log_exp_total_gbp",
             "log_population_imputed",
             "log_salary_governor_gbp",
             "area_tropics",
             "log_dist_london",
             "full",
             "sid")     # dplyr::select variables for table

panelB_prep <- patronage |> 
  dplyr::select(all_of(v_names_B)) |>
  filter(full == 1) |>
  summarise_all(funs(mean), na.rm = T) # mean by colony (not needed, but good to know)

colSums(!is.na(panelB_prep))    # number of non-NA entries by column (to compare w Stata)

panelB_pooled <- patronage |> 
  dplyr::select(all_of(v_names_B)) |>
  filter(full == 1) |>
  summarise_all(funs(mean, sd), na.rm = T) # ready summary table (pooled) 

panelB_pooled_obs <- patronage |> 
  group_by(sid) |>
  dplyr::select(all_of(v_names_B), year) |>
  filter(full == 1) |>
  nrow()                          # no of obs pooled

panelB_pooled_col <- patronage |> 
  dplyr::select(sid, full, year) |>
  dplyr::filter(full == 1) |>
  group_by(sid) |>
  summarise(avg = mean(full)) |>
  nrow()                          # no of colonies pooled, full

# unfiltered
panelB_pooledall <- patronage |> 
  dplyr::select(all_of(v_names_B)) |>
  summarise_all(funs(mean), na.rm = T) # ready summary table (pooled) 

panelB_pooled_obsall <- patronage |> 
  group_by(sid) |>
  dplyr::select(all_of(v_names_B), year) |>
  nrow()                          # no of obs pooled

panelB_pooled_colall <- patronage |> 
  dplyr::select(sid, full, year) |>
  group_by(sid) |>
  summarise(avg = mean(full)) |>
  nrow()                          # no of colonies pooled, full


panelB_1860 <- patronage |> 
  dplyr::select(all_of(v_names_B), year) |>
  filter(full == 1, year == 1860) |>
  summarise_all(funs(mean), na.rm = T) # ready table 1860

panelB_1860_col <- patronage |> 
  dplyr::select(sid, full, year) |>
  filter(full == 1, year == 1860) |>
  group_by(sid) |>
  summarise(avg = mean(full)) |>
  nrow()                # no of colonies 1860


panelB_1900 <- patronage |> 
  dplyr::select(all_of(v_names_B), year) |>
  filter(full == 1, year == 1900) |>
  summarise_all(funs(mean), na.rm = T) # ready table 1900

panelB_1900_col <- patronage |> 
  dplyr::select(sid, full, year) |>
  filter(full == 1, year == 1900) |>
  group_by(sid) |>
  summarise(avg = mean(full)) |>
  nrow()                # no of colonies 1900


panelB_1930 <- patronage |> 
  dplyr::select(all_of(v_names_B), year) |>
  filter(full == 1, year == 1930) |>
  summarise_all(funs(mean), na.rm = T) # ready table 1930

panelB_1930_col <- patronage |> 
  dplyr::select(sid, full, year) |>
  filter(full == 1, year == 1930) |>
  group_by(sid) |>
  summarise(mean(full)) |>
  nrow()                # no of colonies 1930


panelB_1960 <- patronage |> 
  dplyr::select(all_of(v_names_B), year) |>
  filter(full == 1, year == 1960) |>
  summarise_all(funs(mean), na.rm = T) # ready table 1960

panelB_1960_col <- patronage |> 
  dplyr::select(sid, full, year) |>
  filter(full == 1, year == 1960) |>
  group_by(sid) |>
  summarise(mean(full)) |>
  nrow()                # no of colonies 1960


## use calculated stats to create whole table 1

# panel A
t1 <- panelA_pooled |>
  dplyr::select(-full) |>
  t() |>
  as.data.frame()

t1_all <- panelA_pooledall |>
  dplyr::select(-full) |>
  t() |>
  as.data.frame()

r_names1 <- c("Peerage", "Civil servant", "Military", "Politician", "Eton", 
             "Oxford", "Cambridge", "Age at entry", "Years served", "Colonies served")

t1_1 <- as.data.frame(t1[1:length(r_names1),])
t1_2 <- as.data.frame(t1[(length(r_names1) + 1):nrow(t1),])

t1860 <- panelA_1860 |> t() |> as.data.frame()
t1900 <- panelA_1900 |> t() |> as.data.frame()
t1930 <- panelA_1930 |> t() |> as.data.frame()
t1960 <- panelA_1960 |> t() |> as.data.frame()

row.names(t1_1) <- r_names1
row.names(t1_2) <- r_names1
row.names(t1_all) <- r_names1
row.names(t1860) <- r_names1
row.names(t1900) <- r_names1
row.names(t1930) <- r_names1
row.names(t1960) <- r_names1

t1_A <- cbind(t1_1, t1_2, t1860, t1900, t1930, t1960, t1_all)
colnames(t1_A) <- c("Mean", "SD", "1860", "1900", "1930", "1960", "All")

nobs <- c(as.character(panelA_pooled_obs),
          "-",
          as.character(panelA_1860_obs),
          as.character(panelA_1900_obs),
          as.character(panelA_1930_obs),
          as.character(panelA_1960_obs),
          as.character(panelA_pooled_obsall))

t1_A <- rbind(format(round(t1_A, 3), nsmall = 3), nobs)
rownames(t1_A) <- c(r_names1, "Observations")

# panel B
t2 <- panelB_pooled |>
  dplyr::select(-c(full_mean, full_sd, sid_mean, sid_sd)) |>
  t() |>
  as.data.frame()

t2_all <- panelB_pooledall |>
  dplyr::select(-c(full, sid)) |>
  t() |>
  as.data.frame()

r_names2 <- c("(log) Total revenue", "Share customs revenue", "(log) Total expenditure", 
              "(log) Population", "(log) Governorship salary", "Area tropics", 
              "(log) Distance from London")

t2_1 <- as.data.frame(t2[1:length(r_names2),])
t2_2 <- as.data.frame(t2[(length(r_names2) + 1):nrow(t2),])

t1860 <- panelB_1860 |> dplyr::select(-c(full, sid, year)) |> t() |> as.data.frame()
t1900 <- panelB_1900 |> dplyr::select(-c(full, sid, year)) |> t() |> as.data.frame()
t1930 <- panelB_1930 |> dplyr::select(-c(full, sid, year)) |> t() |> as.data.frame()
t1960 <- panelB_1960 |> dplyr::select(-c(full, sid, year)) |> t() |> as.data.frame()

row.names(t2_1) <- r_names2
row.names(t2_2) <- r_names2
row.names(t2_all) <- r_names2
row.names(t1860) <- r_names2
row.names(t1900) <- r_names2
row.names(t1930) <- r_names2
row.names(t1960) <- r_names2

t1_B <- cbind(t2_1, t2_2, t1860, t1900, t1930, t1960, t2_all)
colnames(t1_B) <- c("Mean", "SD", "1860", "1900", "1930", "1960", "All")

nobs <- c(as.character(panelB_pooled_obs),
          "-",
          "-",
          "-", 
          "-",
          "-",
          as.character(panelB_pooled_obsall))

t1_B <- rbind(round(t1_B, 3), nobs)

ncols <- c(as.character(panelB_pooled_col),
          as.character(panelB_1860_col),
          as.character(panelB_1900_col),
          as.character(panelB_1930_col), 
          as.character(panelB_1960_col),
          as.character(panelB_pooled_colall))

t1_B <- rbind(t1_B, ncols)
rownames(t1_B) <- c(r_names2, "Observations", "Colonies")

save(t1_A, t1_B, file = "./results/t1.RData")
rm(list=setdiff(ls(), "patronage"))


##------------------------------------------------------------------------------

### table 2: Salary on connectedness

# regression models
msal1 <- log_salary_governor_gbp ~ no_colonies + shared_ancestry | 
  year + aid + duration |0| bilateral

msal2 <- log_salary_governor_gbp ~ no_colonies + both_arist |
  year + aid + duration | 0 | bilateral

msal3 <- log_salary_governor_gbp ~ no_colonies + both_eton | 
  year + aid + duration | 0 | bilateral

msal4 <- log_salary_governor_gbp ~ no_colonies + both_oxbridge |
  year + aid + duration | 0 | bilateral

msal5 <- log_salary_governor_gbp ~ no_colonies + shared_ancestry + both_arist + 
  both_eton + both_oxbridge | year + aid + duration | 0 | bilateral

msal6 <- log_salary_governor_gbp ~ no_colonies + connected |
  year + aid + duration | 0 | bilateral

# adapt dataset  
patronage_fullall <- patronage |> 
  dplyr::select(log_salary_governor_gbp,
         log_rev_total_gbp,
         no_colonies,
         connected,
         shared_ancestry,
         both_arist,
         both_eton,
         both_oxbridge,
         year,
         duration,
         aid,
         sid,
         bilateral,
         area_tropics,
         log_dist_london,
         full) 

patronage_fullall$year <- as.factor(patronage_fullall$year)
patronage_fullall$aid <- as.factor(patronage_fullall$aid)

patronage_full <- patronage_fullall |>
  filter(full == 1)

# run regression
sal1 <- felm(msal1, data = patronage_full)
sal2 <- felm(msal2, data = patronage_full)
sal3 <- felm(msal3, data = patronage_full)
sal4 <- felm(msal4, data = patronage_full)
sal5 <- felm(msal5, data = patronage_full)
sal6 <- felm(msal6, data = patronage_full)
sal7 <- felm(msal6, data = patronage_fullall)

# get mean of independent variable
mean_sal <- round(mean(patronage_full$log_salary_governor_gbp), 3)
mean_salall <- round(mean(patronage_fullall$log_salary_governor_gbp, na.rm = T), 3)

# save stuff
save(sal1, sal2, sal3, sal4, sal5, sal6, sal7, mean_sal, mean_salall, 
     file = "./results/sal_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage", "patronage_full")])

##------------------------------------------------------------------------------

## table 3: transfers and connectedness
mtran1 <- log_salary_governor_gbp ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

mtran2 <- log_salary_governor_gbp ~ no_colonies + connected | 
  year + aid + duration + sid |0| bilateral

mtran2.1 <- initial_rev ~ no_colonies + connected | 
  year + duration + col_gov_ID |0| bilateral

mtran3 <- min_rev_amount_gbp ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

mtran4 <- area_tropics ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

mtran5 <- log_dist_london ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

mtran6 <- initial_rev ~ no_colonies + connected | 
  year + aid + duration |0| bilateral



# adapt dataset  
patronage_full <- patronage_full|>
  group_by(sid) |>
  mutate(min_rev_amount_gbp = min(log_rev_total_gbp)) |>                        # is the lowest rev really the "initial rev"? Do the same with min_rev
  ungroup()
  
# actual log initial revenue
patronage_full <- patronage_full |>
  mutate(col_gov_ID = paste0(sid, aid)) |>
  group_by(col_gov_ID) |>
  mutate(initial_rev = min(log_rev_total_gbp)) |>
  ungroup()

# dataset with "all"
patronage_all <- patronage|>
  group_by(sid) |>
  mutate(min_rev_amount_gbp = min(log_rev_total_gbp)) |>                        # is the lowest rev really the "initial rev"? Do the same with min_rev
  ungroup()

patronage_all <- patronage_all |>
  mutate(col_gov_ID = paste0(sid, aid)) |>
  group_by(col_gov_ID) |>
  mutate(initial_rev = min(log_rev_total_gbp)) |>
  ungroup()

# run regressions
tran1 <- felm(mtran1, data = patronage_full)
tran2 <- felm(mtran2, data = patronage_full)
tran2.1 <- felm(mtran2.1, data = patronage_full)
tran3 <- felm(mtran3, data = patronage_full)
tran4 <- felm(mtran4, data = patronage_full)
tran5 <- felm(mtran5, data = patronage_full)
tran6 <- felm(mtran6, data = patronage_full)
tran7 <- felm(mtran6, data = patronage_all)

# means of indep var
mean_rev <- round(mean(patronage_full$min_rev_amount_gbp, na.rm = T), 3)
mean_trop <- round(mean(patronage_full$area_tropics), 3)
mean_london <- round(mean(patronage_full$log_dist_london), 3)
mean_initial_rev <- round(mean(patronage_full$initial_rev, na.rm = T), 3)
mean_initial_rev_all <- round(mean(patronage_all$initial_rev, na.rm = T), 3)

# save stuff
save(tran1, tran2, tran2.1, tran3, tran4, tran5, tran6, tran7, mean_rev, mean_trop, 
     mean_initial_rev_all, mean_london, mean_initial_rev, file = "./results/tran_reg.RData")

## Mediation analysis
# model definition
mmed1 <- log_salary_governor_gbp ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

lmmed1 <- log_salary_governor_gbp ~ no_colonies + connected + factor(year) + 
  factor(aid) + factor(duration)

mmed2 <- initial_rev ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

lmmed2 <- initial_rev ~ no_colonies + connected + factor(year) + factor(aid) +
  factor(duration)

mmed3 <- log_salary_governor_gbp ~ no_colonies + connected + initial_rev | 
  year + aid + duration |0| bilateral

lmmed3 <- log_salary_governor_gbp ~ no_colonies + connected + initial_rev + 
  factor(year) + factor(aid) + factor(duration)

# run regressions
med1 <- felm(mmed1, data = patronage_full)
lmed1 <- lm(lmmed1, data = patronage_full)
med2 <- felm(mmed2, data = patronage_full)
lmed2 <- lm(lmmed2, data = patronage_full)
med3 <- felm(mmed3, data = patronage_full)
lmed3 <- lm(lmmed3, data = patronage_full)

med <- mediate(lmed2,
               lmed3,
               treat = "connected",
               mediator = "initial_rev",
               data = patronage_full)
beep(10) # calculation finished

# function for extracting summary table
extract_mediation_summary <- function (x) {
  
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) ||
                   (inherits(x$model.y, "glm") && x$model.y$family$family ==
                      "gaussian" && x$model.y$family$link == "identity") ||
                   (inherits(x$model.y, "survreg") && x$model.y$dist ==
                      "gaussian"))
  
  printone <- !x$INT && isLinear.y
  
  if (printone) {
    
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    
  } else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    
    rownames(smat) <- c("ACME (control)", "ACME (treated)",
                        "ADE (control)", "ADE (treated)", "Total Effect",
                        "Prop. Mediated (control)", "Prop. Mediated (treated)",
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
    
  }
  
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""),
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  smat
  
}

summed <- as.data.frame(extract_mediation_summary(med))

# mediate function does not report SE, therefore I derive them form the CIs
se <- (summed[,3] - summed[,2])/(2*qnorm(0.975))
summed <- round(cbind(summed[,1], se, summed[,2:4]),3)
summed <- summed |> dplyr::select(-"p-value")
colnames(summed) <- c("Estimate", "SE", "95% CI upper", "95% CI lower")

# save stuff
save(med1, med2, med3, summed, file = "./results/med_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage")])

##------------------------------------------------------------------------------
## table 4: removal of patronage 1930

# check for missings in data
varlist <- c("civilservant", "politician", "military", "peerage", "eton")       # why only these variables?
patronage |> 
  dplyr::select(all_of(varlist)) |>
  summarise_all(funs(sum(is.na(.))))

# replace NA values with zero
patronage_clean <- patronage |>
  replace_na(list(civilservant = 0,
                  politician = 0,
                  military = 0,
                  peerage = 0,
                  eton = 0))

# center variables
varlist <- c("civilservant",
             "politician",
             "military",
             "peerage",
             "no_colonies",
             "min_revenue",
             "landlocked",
             "min_year",
             "log_dist_london",
             "area_tropics")                                                    # why center only these, and why center dummies?

patronage_clean <- patronage_clean |>
  mutate_at(all_of(varlist), funs(c(scale(.))))

# adapt for full
patronage_clean <- patronage_clean |>
  filter(full == 1)

# define models
mabol1 <- log_salary_governor_gbp ~ no_colonies + connected | 
  year + aid + duration |0| bilateral

mabol2 <- log_salary_governor_gbp ~ no_colonies + connected + post1930_connected| 
  year + aid + duration |0| bilateral

mabol3 <- log_salary_governor_gbp ~ no_colonies + connected + post1930_connected + 
  connected_year1930 | year + aid + duration |0| bilateral

mabol4 <- log_salary_governor_gbp ~ no_colonies + connected + post1930_connected + 
  I(connected*no_colonies) + I(connected*civilservant) + I(connected*military) + 
  I(connected*politician)| year + aid + duration |0| bilateral

mabol5 <- log_salary_governor_gbp ~ no_colonies + connected + post1930_connected + 
  I(connected*min_revenue) + I(connected*min_revenue) + I(connected*log_dist_london) +
  I(connected*area_tropics) + I(connected*min_year) + I(connected*landlocked)| 
  year + aid + duration |0| bilateral

mabol7 <- log_salary_governor_gbp ~ no_colonies + connected + post1930_connected + 
  connected_year1930 + I(connected*min_revenue) + I(connected*min_revenue) + 
  I(connected*log_dist_london) + I(connected*area_tropics) + I(connected*min_year) + 
  I(connected*landlocked) + I(connected*no_colonies) + I(connected*civilservant) + 
  I(connected*military) + I(connected*politician)| 
  year + aid + duration |0| bilateral

# run regressions (and replicate lincom from stata, i.e. the lin combination of coef)
abol1 <- felm(mabol1, data = patronage_clean)

abol2 <- felm(mabol2, data = patronage_clean)
lincom2 <- summary(glht(abol2, linfct = c("connected + post1930_connected = 0")))
com2 <- lincom2$test$coefficients
com2_se <- lincom2$test$sigma

abol3 <- felm(mabol3, data = patronage_clean)
lincom3 <- summary(glht(abol3, linfct = c("connected + post1930_connected = 0")))
com3 <- lincom3$test$coefficients
com3_se <- lincom3$test$sigma

abol4 <- felm(mabol4, data = patronage_clean)
lincom4 <- summary(glht(abol4, linfct = c("connected + post1930_connected = 0")))
com4 <- lincom4$test$coefficients
com4_se <- lincom4$test$sigma

abol5 <- felm(mabol5, data = patronage_clean)
lincom5 <- summary(glht(abol5, linfct = c("connected + post1930_connected = 0")))
com5 <- lincom5$test$coefficients
com5_se <- lincom5$test$sigma

# for abol6, same model as in abol2 but altered subsample
patronage_clean_prepost <- subset(patronage_clean, sum_d_pre_post == 2)
abol6 <- felm(mabol2, data = patronage_clean_prepost)
lincom6 <- summary(glht(abol6, linfct = c("connected + post1930_connected = 0")))
com6 <- lincom6$test$coefficients
com6_se <- lincom6$test$sigma

# calculate mean
mean_sal_prepost <- round(mean(patronage_clean_prepost$log_salary_governor_gbp), 3)

abol7 <- felm(mabol7, data = patronage_clean)
lincom7 <- summary(glht(abol7, linfct = c("connected + post1930_connected = 0")))
com7 <- lincom7$test$coefficients
com7_se <- lincom7$test$sigma


# save stuff
save(abol1, abol2, abol3, abol4, abol5, abol6, abol7, mean_sal_prepost, com2, com2_se,
     com3, com3_se, com4, com4_se, com5, com5_se, com6, com6_se, com7, com7_se,
     file = "./results/abol_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage")])

##------------------------------------------------------------------------------

## table 5: fiscal performance and connectedness

# adapt for full
patronage_full <- patronage |>
  filter(full == 1)

# panel A

# define models
mrev1 <- log_rev_total_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

mrev2 <- log_rev_total_gbp ~ no_colonies + connected + post1930_connected | 
  year + state_aid + duration |0| bilateral

mrev3 <- log_rev_customs_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

mrev4 <- log_rev_internal_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

# run regressions (and replicate lincom from stata, i.e. the lin combination of coef)
rev1 <- felm(mrev1, data = patronage_full)

rev2 <- felm(mrev2, data = patronage_full)
lincom_rev <- summary(glht(rev2, linfct = c("connected + post1930_connected = 0")))
com_rev <- lincom_rev$test$coefficients
com_rev_se <- lincom_rev$test$sigma

rev3 <- felm(mrev3, data = patronage_full)
rev4 <- felm(mrev4, data = patronage_full)

rev5 <- felm(mrev2, data = patronage)
lincom_revall <- summary(glht(rev5, linfct = c("connected + post1930_connected = 0")))
com_revall <- lincom_revall$test$coefficients
com_revall_se <- lincom_revall$test$sigma

# panel B

# define models
mexp1 <- log_exp_total_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

mexp2 <- log_exp_total_gbp ~ no_colonies + connected + post1930_connected | 
  year + state_aid + duration |0| bilateral

mexp3 <- log_exp_tax_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

mexp4 <- log_exp_pubworks_gbp ~ no_colonies + connected | 
  year + state_aid + duration |0| bilateral

# run regressions (and replicate lincom from stata, i.e. the lin combination of coef)
exp1 <- felm(mexp1, data = patronage_full)

exp2 <- felm(mexp2, data = patronage_full)
lincom_exp <- summary(glht(exp2, linfct = c("connected + post1930_connected = 0")))
com_exp <- lincom_exp$test$coefficients
com_exp_se <- lincom_exp$test$sigma

exp3 <- felm(mexp3, data = patronage_full)
exp4 <- felm(mexp4, data = patronage_full)

exp5 <- felm(mexp2, data = patronage)
lincom_expall <- summary(glht(exp5, linfct = c("connected + post1930_connected = 0")))
com_expall <- lincom_expall$test$coefficients
com_expall_se <- lincom_expall$test$sigma

# calculate means
mean_rev_total <- round(mean(patronage_full$log_rev_total_gbp), 3)
mean_revall_total <- round(mean(patronage$log_rev_total_gbp), 3)
mean_rev_customs <- round(mean(patronage_full$log_rev_customs_gbp, na.rm = T), 3)
mean_rev_internal <- round(mean(patronage_full$log_rev_internal_gbp, na.rm = T), 3)
mean_exp_total <- round(mean(patronage_full$log_exp_total_gbp, na.rm = T), 3)
mean_expall_total <- round(mean(patronage$log_exp_total_gbp, na.rm = T), 3)
mean_exp_tax <- round(mean(patronage_full$log_exp_tax_gbp, na.rm = T), 3)
mean_exp_pubworks <- round(mean(patronage_full$log_exp_pubworks_gbp, na.rm = T), 3)

# save stuff
save(rev1, rev2, rev3, rev4, rev5, exp1, exp2, exp3, exp4, exp5, mean_rev_total, 
     mean_revall_total, mean_rev_customs, mean_rev_internal, mean_exp_total, 
     mean_expall_total, mean_exp_tax, mean_exp_pubworks,
     com_rev, com_rev_se, com_exp, com_exp_se, com_revall, com_revall_se, 
     com_expall, com_expall_se, file = "./results/perf_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage")])

##------------------------------------------------------------------------------

## table 6: tax ordinance, exemptions and connectedness

# adapt data
patronage_full <- patronage |>
  filter(full == 1) |>
  mutate(ordinance_social = ordinance_health + ordinance_education + ordinance_welfare)

# define models
mord1 <- ordinance_total ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

mord2 <- ordinance_tax ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

mord3 <- ordinance_trade ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

mord4 <- customs_exemptions ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

mord5 <- ordinance_social ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

mord6 <- ordinance_pubworks ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

# run regressions (and replicate lincom from stata, i.e. the lin combination of coef)
ord1 <- felm(mord1, data = patronage_full)
lincom_ord1 <- summary(glht(ord1, linfct = c("connected + post1930_connected = 0")))
com_ord1 <- lincom_ord1$test$coefficients
com_ord1_se <- lincom_ord1$test$sigma

ord2 <- felm(mord2, data = patronage_full)
lincom_ord2 <- summary(glht(ord2, linfct = c("connected + post1930_connected = 0")))
com_ord2 <- lincom_ord2$test$coefficients
com_ord2_se <- lincom_ord2$test$sigma

ord3 <- felm(mord3, data = patronage_full)
lincom_ord3 <- summary(glht(ord3, linfct = c("connected + post1930_connected = 0")))
com_ord3 <- lincom_ord3$test$coefficients
com_ord3_se <- lincom_ord3$test$sigma

ord4 <- felm(mord4, data = patronage_full)
lincom_ord4 <- summary(glht(ord4, linfct = c("connected + post1930_connected = 0")))
com_ord4 <- lincom_ord4$test$coefficients
com_ord4_se <- lincom_ord4$test$sigma

ord5 <- felm(mord5, data = patronage_full)
lincom_ord5 <- summary(glht(ord5, linfct = c("connected + post1930_connected = 0")))
com_ord5 <- lincom_ord5$test$coefficients
com_ord5_se <- lincom_ord5$test$sigma

ord6 <- felm(mord6, data = patronage_full)
lincom_ord6 <- summary(glht(ord6, linfct = c("connected + post1930_connected = 0")))
com_ord6 <- lincom_ord6$test$coefficients
com_ord6_se <- lincom_ord6$test$sigma

mean_ord_total <- round(mean(patronage_full$ordinance_total, na.rm = T), 3)
mean_ord_tax <- round(mean(patronage_full$ordinance_tax, na.rm = T), 3)
mean_ord_trade <- round(mean(patronage_full$ordinance_trade, na.rm = T), 3)
mean_cus_exemp <- round(mean(patronage_full$customs_exemptions, na.rm = T), 3)
mean_ord_social <- round(mean(patronage_full$ordinance_social, na.rm = T), 3)
mean_ord_pubworks <- round(mean(patronage_full$ordinance_pubworks, na.rm = T), 3)

# save stuff
save(ord1, com_ord1, com_ord1_se, ord2, com_ord2, com_ord2_se, ord3, com_ord3, com_ord3_se,
     ord4, com_ord4, com_ord4_se, ord5, com_ord5, com_ord5_se, ord6, com_ord6, com_ord6_se,
     mean_ord_total, mean_ord_tax, mean_ord_trade, mean_cus_exemp, mean_ord_social, 
     mean_ord_pubworks, file = "./results/ord_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage")])

##------------------------------------------------------------------------------

## table 7: alternative performance measures and connectedness

patronage_full <- patronage |>
  filter(full == 1)

# define models
malt1 <- social_unrest ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

malt2 <- hansard_mention ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

malt3 <- polarity ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

malt4 <- award_highest ~ no_colonies + connected + post1930_connected| 
  year + state_aid + duration |0| bilateral

# run regressions (and replicate lincom from stata, i.e. the lin combination of coef)
alt1 <- felm(malt1, data = patronage_full)
lincom_alt1 <- summary(glht(alt1, linfct = c("connected + post1930_connected = 0")))
com_alt1 <- lincom_alt1$test$coefficients
com_alt1_se <- lincom_alt1$test$sigma

alt2 <- felm(malt2, data = patronage_full)
lincom_alt2 <- summary(glht(alt2, linfct = c("connected + post1930_connected = 0")))
com_alt2 <- lincom_alt2$test$coefficients
com_alt2_se <- lincom_alt2$test$sigma

alt3 <- felm(malt3, data = patronage_full)
lincom_alt3 <- summary(glht(alt3, linfct = c("connected + post1930_connected = 0")))
com_alt3 <- lincom_alt3$test$coefficients
com_alt3_se <- lincom_alt3$test$sigma

alt4 <- felm(malt4, data = patronage_full)
lincom_alt4 <- summary(glht(alt4, linfct = c("connected + post1930_connected = 0")))
com_alt4 <- lincom_alt4$test$coefficients
com_alt4_se <- lincom_alt4$test$sigma

alt5 <- felm(malt1, data = patronage)
lincom_alt5 <- summary(glht(alt5, linfct = c("connected + post1930_connected = 0")))
com_alt5 <- lincom_alt5$test$coefficients
com_alt5_se <- lincom_alt5$test$sigma

alt6 <- felm(malt3, data = patronage)
lincom_alt6 <- summary(glht(alt6, linfct = c("connected + post1930_connected = 0")))
com_alt6 <- lincom_alt6$test$coefficients
com_alt6_se <- lincom_alt6$test$sigma

mean_alt_unrest <- round(mean(patronage_full$social_unrest, na.rm = T), 3)
mean_alt_unrestall <- round(mean(patronage$social_unrest, na.rm = T), 3)
mean_alt_hansard <- round(mean(patronage_full$hansard_mention, na.rm = T), 3)
mean_alt_polarity <- round(mean(patronage_full$polarity, na.rm = T), 3)
mean_alt_polarityall <- round(mean(patronage$polarity, na.rm = T), 3)
mean_alt_award <- round(mean(patronage_full$award_highest, na.rm = T), 3)

# save stuff
save(alt1, com_alt1, com_alt1_se,
     alt2, com_alt2, com_alt2_se, 
     alt3, com_alt3, com_alt3_se,
     alt4, com_alt4, com_alt4_se,
     alt5, com_alt5, com_alt5_se,
     alt6, com_alt6, com_alt6_se,
     mean_alt_unrest, mean_alt_hansard, 
     mean_alt_polarity, mean_alt_award,
     mean_alt_unrestall, mean_alt_polarityall,
     file = "./results/alt_reg.RData")

# clear environment
rm(list=ls()[! ls() %in% c("patronage")])

##------------------------------------------------------------------------------



