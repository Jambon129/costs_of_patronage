# script_02 for Replication Excercise of Guo (2018): The Costs of Patronage: Evidence from the British Empire
# author (of replication): Andreas Chmielowski
# purpose: Recreate figures
# PhD Economics, Gothenburg University
#--------------------------------------

## Figure 2: share of connected governors

plot1 <- patronage |>
  mutate(connected_excl_school = ifelse(both_arist == 1, 1,
                                        ifelse(shared_ancestry == 1, 1, 0))) |>
  group_by(quinquennial) |>
  summarise(share = mean(connected),
            share_ex_school = mean(connected_excl_school)) |>
  ggplot() +
  geom_line(aes(x = quinquennial, y = share, color = "Connected"), size = 0.5) +
  geom_point(aes(x = quinquennial, y = share, color = "Connected"), size = 1) +
  geom_line(aes(x = quinquennial, y = share_ex_school, 
                color = "Both aristocrates \n and/or ancestry"), size = 0.5) +
  geom_point(aes(x = quinquennial, y = share_ex_school, 
                 color = "Both aristocrates \n and/or ancestry"), size = 1) +
  labs(x = "", y = "Share connected") +
  ggtitle("All") +
  theme_minimal() +
  scale_color_brewer(palette="Paired") +
  theme(legend.position = c(0.15, 0.15), legend.title = element_blank())

plot1_all <- patronage |>
  filter(full == 1) |>
  mutate(connected_excl_school = ifelse(both_arist == 1, 1,
                                        ifelse(shared_ancestry == 1, 1, 0))) |>
  group_by(quinquennial) |>
  summarise(share = mean(connected),
            share_ex_school = mean(connected_excl_school)) |>
  ggplot() +
  geom_line(aes(x = quinquennial, y = share, color = "Connected"), size = 0.5) +
  geom_point(aes(x = quinquennial, y = share, color = "Connected"), size = 1) +
  geom_line(aes(x = quinquennial, y = share_ex_school, 
                color = "Both aristocrates \n and/or ancestry"), size = 0.5) +
  geom_point(aes(x = quinquennial, y = share_ex_school, 
                 color = "Both aristocrates \n and/or ancestry"), size = 1) +
  labs(x = "", y = "Share connected") +
  ggtitle("Full") +
  theme_minimal() +
  scale_color_brewer(palette="Reds") +
  theme(legend.position = c(0.15, 0.15), legend.title = element_blank())
  
save(plot1, plot1_all, file = "./results/plot1.RData")

###-----------------------------------------------------------------------------
## Figure 3: average salary connected vs unconnected over time

patronage$decade[patronage$year >= 1850 & patronage$year <= 1859] <- 1850

plot2 <- patronage |>
  filter(!is.na(log_salary_governor_gbp)) |>
  mutate(salary_gov = exp(log_salary_governor_gbp)) |>
  group_by(decade, connected) |>
  summarise(avg_sal = mean(salary_gov)) |>
  mutate(connected = factor(connected, labels = c("unconnected", "connected")))|>
  ggplot(aes(x = decade, y = avg_sal, group = connected)) +
  geom_line(aes(color = connected), size = 0.5) +
  geom_point(aes(color = connected), size = 1) +
  ylim(2000, 7000) + 
  labs(x = "", y = "Average salary (GBP)") +
  ggtitle("All") +
  theme_minimal() +
  scale_color_brewer(palette="Paired") +
  theme(legend.position = c(0.90, 0.15), legend.title = element_blank())

plot2_all <- patronage |>
  filter(!is.na(log_salary_governor_gbp),
         full == 1) |>
  mutate(salary_gov = exp(log_salary_governor_gbp)) |>
  group_by(decade, connected) |>
  summarise(avg_sal = mean(salary_gov)) |>
  mutate(connected = factor(connected, labels = c("unconnected", "connected")))|>
  ggplot(aes(x = decade, y = avg_sal, group = connected)) +
  geom_line(aes(color = connected), size = 0.5) +
  geom_point(aes(color = connected), size = 1) +
  ylim(2000, 7000) + 
  labs(x = "", y = "Average salary (GBP)") +
  ggtitle("Full") +
  theme_minimal() +
  scale_color_brewer(palette="Reds") +
  theme(legend.position = c(0.90, 0.15), legend.title = element_blank())
save(plot2, plot2_all, file = "./results/plot2.RData")

###-----------------------------------------------------------------------------
## Figure 4: Salary gap and the removal of patronage


patronage$q_con1854 <- 0 
patronage$q_con1860 <- 0 
patronage$q_con1865 <- 0 
patronage$q_con1870 <- 0
patronage$q_con1875 <- 0
patronage$q_con1880 <- 0 
patronage$q_con1885 <- 0
patronage$q_con1890 <- 0 
patronage$q_con1895 <- 0
patronage$q_con1900 <- 0 
patronage$q_con1905 <- 0 
patronage$q_con1910 <- 0 
patronage$q_con1915 <- 0
patronage$q_con1920 <- 0
patronage$q_con1925 <- 0
patronage$q_con1930 <- 0 
patronage$q_con1935 <- 0
patronage$q_con1940 <- 0
patronage$q_con1945 <- 0
patronage$q_con1950 <- 0
patronage$q_con1955 <- 0 
patronage$q_con1960 <- 0 

patronage$q_con1854[patronage$quinquennial == 1854 & 
        patronage$connected == 1] <- 1

patronage$q_con1860[patronage$quinquennial == 1860 & 
                          patronage$connected == 1] <- 1

patronage$q_con1865[patronage$quinquennial == 1865 & 
                          patronage$connected == 1] <- 1

patronage$q_con1870[patronage$quinquennial == 1870 & 
                          patronage$connected == 1] <- 1

patronage$q_con1875[patronage$quinquennial == 1875 & 
                          patronage$connected == 1] <- 1

patronage$q_con1880[patronage$quinquennial == 1880 & 
                          patronage$connected == 1] <- 1

patronage$q_con1885[patronage$quinquennial == 1885 & 
                          patronage$connected == 1] <- 1

patronage$q_con1890[patronage$quinquennial == 1890 & 
                          patronage$connected == 1] <- 1

patronage$q_con1895[patronage$quinquennial == 1895 & 
                          patronage$connected == 1] <- 1

patronage$q_con1900[patronage$quinquennial == 1900 & 
                          patronage$connected == 1] <- 1

patronage$q_con1905[patronage$quinquennial == 1905 & 
                          patronage$connected == 1] <- 1

patronage$q_con1910[patronage$quinquennial == 1910 & 
                          patronage$connected == 1] <- 1

patronage$q_con1915[patronage$quinquennial == 1915 & 
                          patronage$connected == 1] <- 1

patronage$q_con1920[patronage$quinquennial == 1920 & 
                          patronage$connected == 1] <- 1

patronage$q_con1925[patronage$quinquennial == 1925 & 
                          patronage$connected == 1] <- 1

patronage$q_con1930[patronage$quinquennial == 1930 & 
                          patronage$connected == 1] <- 1

patronage$q_con1935[patronage$quinquennial == 1935 & 
                          patronage$connected == 1] <- 1

patronage$q_con1940[patronage$quinquennial == 1940 & 
                          patronage$connected == 1] <- 1

patronage$q_con1945[patronage$quinquennial == 1945 & 
                          patronage$connected == 1] <- 1

patronage$q_con1950[patronage$quinquennial == 1950 & 
                          patronage$connected == 1] <- 1

patronage$q_con1955[patronage$quinquennial == 1955 & 
                          patronage$connected == 1] <- 1

# necessary to get to the same results as in stata
patronage_fac <- patronage |>
  filter(!is.na(quinquennial))

# define model
mod1 <- log_salary_governor_gbp ~ no_colonies + connected_year1930 + q_con1854 +
  q_con1860 + q_con1865 + q_con1870 + q_con1875 + q_con1880 + q_con1885 +
  q_con1890 + q_con1895 + q_con1900 + q_con1905 + q_con1910 + q_con1915 + q_con1920 +
  q_con1925 + q_con1930 + q_con1935 + q_con1940 + q_con1945 + q_con1950 + q_con1955| 
  year + aid + duration |0| bilateral

# run regression
fit1 <- felm(mod1, data = subset(patronage_fac, year >= 1854))

# exctract estimates
tab_fit <- as.data.frame(cbind(fit1$coefficients, fit1$se, confint(fit1, level = 0.90)))
tab_fit <- tab_fit |> filter(row.names(tab_fit) %in% c("q_con1860",
                                                       "q_con1865",
                                                       "q_con1870",
                                                       "q_con1875",
                                                       "q_con1880",
                                                       "q_con1885",
                                                       "q_con1890",
                                                       "q_con1895",
                                                       "q_con1900",
                                                       "q_con1905",
                                                       "q_con1910",
                                                       "q_con1915",
                                                       "q_con1920",
                                                       "q_con1925",
                                                       "q_con1930",
                                                       "q_con1935",
                                                       "q_con1940",
                                                       "q_con1945",
                                                       "q_con1950",
                                                       "q_con1955"))


tab_fit$year <- c(1860, 1865, 1870, 1875, 1880, 1885, 1890, 1895, 1900, 1905, 
                  1910, 1915, 1920, 1925, 1930, 1935, 1940, 1945, 1950, 1955)
colnames(tab_fit) <- c("Estimate", "SE", "lower", "upper", "year")

addendum <- patronage_fac |>
  filter(!is.na(log_salary_governor_gbp)) |>
  mutate(salary_gov = exp(log_salary_governor_gbp)) |>
  group_by(quinquennial, connected) |>
  summarise(avg_sal = mean(salary_gov)) |>
  mutate(connected = factor(connected, labels = c("unconnected", "connected"))) |>
  dplyr::rename(year = quinquennial)

addcon <- addendum |> filter(connected == "connected") 
adduncon <- addendum |> filter(connected == "unconnected") 

tab_fit <- left_join(tab_fit, addcon, by = "year") |>
  dplyr::select(!connected) |>
  dplyr::rename(con = avg_sal)

tab_fit <- left_join(tab_fit, adduncon, by = "year") |>
  dplyr::select(!connected) |>
  dplyr::rename(uncon = avg_sal)

tab_fit <- tab_fit |>
  mutate(gap = con - uncon)

plot3 <- ggplot(tab_fit) +
  geom_errorbar(aes(y = Estimate, 
                    x = year,
                    ymin=lower,
                    ymax=upper), width=.3, color = "#003399") +
  geom_point(aes(y = Estimate, 
                 x = year), color = "#003399") +
  #geom_line(aes(x = year, y = gap/10000), color = "#FF0000") +
  #ylim(-0.4, 0.6) +
  labs(x = "", y = "log salary gap") +
  theme_minimal() +
  geom_vline(xintercept = 1930) +
  geom_hline(yintercept = 0) +
  geom_rect(aes(xmin = 1860, xmax = 1905, ymin=-Inf,ymax=Inf), 
            fill = "red", 
            alpha = .006)
plot3

# get variable number of colonies
patronage_fac <- patronage_fac |>
  group_by(quinquennial) |>
  mutate(count = n_distinct(sid)) |>
  ungroup()

# actual log initial revenue
patronage_fac <- patronage_fac |>
  mutate(col_gov_ID = paste0(sid, aid)) |>
  group_by(col_gov_ID) |>
  mutate(initial_rev = min(log_rev_total_gbp)) |>
  ungroup()

# define model
mod1.1 <- log_salary_governor_gbp ~ no_colonies +
  connected_year1930 + q_con1854 + I(count*connected) +
  q_con1860 + q_con1865 + q_con1870 + q_con1875 + q_con1880 + q_con1885 +
  q_con1890 + q_con1895 + q_con1900 + q_con1905 + q_con1910 + q_con1915 + q_con1920 +
  q_con1925 + q_con1930 + q_con1935 + q_con1940 + q_con1945 + q_con1950 + q_con1955|
  year + aid + duration |0| bilateral

# I(connected*civilservant) + I(connected*military) + I(connected*politician) + I(connected*no_colonies) + I(connected*min_year) +
#  I(connected*log_dist_london) + I(connected*area_tropics) + I(connected*landlocked) +
fit1.1 <- felm(mod1.1, data = subset(patronage_fac, year >= 1854))
summary(fit1.1)


# exctract estimates for plot 3.1
tab_fit2 <- as.data.frame(cbind(fit1.1$coefficients, fit1.1$se, confint(fit1.1, level = 0.90)))
tab_fit2 <- tab_fit2 |> filter(row.names(tab_fit2) %in% c("q_con1860",
                                                       "q_con1865",
                                                       "q_con1870",
                                                       "q_con1875",
                                                       "q_con1880",
                                                       "q_con1885",
                                                       "q_con1890",
                                                       "q_con1895",
                                                       "q_con1900",
                                                       "q_con1905",
                                                       "q_con1910",
                                                       "q_con1915",
                                                       "q_con1920",
                                                       "q_con1925",
                                                       "q_con1930",
                                                       "q_con1935",
                                                       "q_con1940",
                                                       "q_con1945",
                                                       "q_con1950",
                                                       "q_con1955"))


tab_fit2$year <- c(1860, 1865, 1870, 1875, 1880, 1885, 1890, 1895, 1900, 1905,
                  1910, 1915, 1920, 1925, 1930, 1935, 1940, 1945, 1950, 1955)
colnames(tab_fit2) <- c("Estimate", "SE", "lower", "upper", "year")

plot3.1 <- ggplot(tab_fit2, aes(y = Estimate,
                             x = year,
                             ymin=lower,
                             ymax=upper)) +
  geom_errorbar(width=.3, color = "#003399") +
  geom_point(color = "#003399") +
  #ylim(-0.4, 0.6) +
  labs(x = "", y = "log salary gap") +
  theme_minimal() +
  geom_vline(xintercept = 1930) +
  geom_hline(yintercept = 0) +
  geom_rect(aes(xmin = 1860, xmax = 1905, ymin=-Inf,ymax=Inf),
            fill = "red",
            alpha = .006)
plot3.1
save(plot3, plot3.1, file = "./results/plot3.RData")

###-----------------------------------------------------------------------------
## Figure 5: Event study

# create leads and lags
patronage_lead_lag <- patronage |> 
  group_by(aid) |>
  arrange(year, .by_group = TRUE) |>
  dplyr::mutate(lag1 = dplyr::lag(connected),
         lag1 = ifelse(is.na(lag1), 0, lag1),
         lag2 = dplyr::lag(lag1),
         lag2 = ifelse(is.na(lag2), 0, lag2),
         lag3 = dplyr::lag(lag2),
         lag3 = ifelse(is.na(lag3), 0, lag3),
         lead1 = dplyr::lead(connected),
         lead1 = ifelse(is.na(lead1), 0, lead1),
         lead2 = dplyr::lead(lead1),
         lead2 = ifelse(is.na(lead2), 0, lead2),
         lead3 = dplyr::lead(lead2),
         lead3 = ifelse(is.na(lead3), 0, lead3)
         )


# define models
mod2 <- log_rev_total_gbp ~ no_colonies + lead3 + lead2 + lead1 + connected + 
  lag1 + lag2 + lag3 | year + state_aid + duration |0| bilateral

mod3 <- log_exp_total_gbp ~ no_colonies + lead3 + lead2 + lead1 + connected + 
  lag1 + lag2+ lag3 | year + state_aid + duration |0| bilateral

# run regressions
fit2 <- felm(mod2, data = patronage_lead_lag)
fit3 <- felm(mod3, data = patronage_lead_lag)


# exctract estimates
tab_fit2 <- as.data.frame(cbind(fit2$coefficients, 
                                fit2$se, 
                                confint(fit2, level = 0.90),
                                fit3$coefficients,
                                fit3$se,
                                confint(fit3, level = 0.90)))
colnames(tab_fit2) <- c("Estimate.rev", 
                        "SE.rev", 
                        "lower.rev", 
                        "upper.rev",
                        "Estimate.exp", 
                        "SE.exp", 
                        "lower.exp", 
                        "upper.exp")

tab_fit2 <- tab_fit2 |> filter(row.names(tab_fit2) %in% c("lead3",
                                                       "lead2",
                                                       "lead1",
                                                       "connected",
                                                       "lag1",
                                                       "lag2",
                                                       "lag3"))

tab_fit2$lags <- c("-3","-2", "-1", "0", "+1", "+2", "+3")

plot4 <- ggplot(tab_fit2) +
  geom_point(aes(y = Estimate.rev, x = as.numeric(lags) -0.03), color = "#003399") +
  geom_errorbar(aes(y = Estimate.rev, 
                    x = as.numeric(lags) -0.03, 
                    ymin = lower.rev, 
                    ymax = upper.rev), 
                width=.3, color = "#003399") +
  geom_point(aes(y = Estimate.exp, x = as.numeric(lags) + 0.03), color = "#003399") +
  geom_errorbar(aes(y = Estimate.exp, 
                    x = as.numeric(lags) + 0.03, 
                    ymin = lower.exp, 
                    ymax = upper.exp), 
                width=.3, color = "#0099FF") +
  labs(x = "Leads and lags", y = "log diff. connected vs. unconnected") +
  geom_hline(yintercept = 0) +
  theme_minimal()
save(plot4, file = "./results/plot4.RData")

##------------------------------------------------------------------------------
### additional stuff (not included)
  
### use callaway and Sant'Anna's estimator

# # define a never-treated variable
# patronage_did <- patronage |>
#   group_by(aid) |>
#   dplyr::mutate(ever_treated = max(connected)) |> # if max value is 0, this governor was never treated
#   ungroup() |>
#   dplyr::mutate(never_treated = if_else(ever_treated == 0, 1, 0))
# 
# # restrict dataset to cases where treatment happens once
# patronage_did <- patronage_did |>
#   group_by(ever_treated, aid) |>
#   arrange(year, .by_group = TRUE) |>
#   mutate(lag1 = dplyr::lag(connected), # lag connected-indicator
#          lag1 = ifelse(is.na(lag1), 0, lag1), # replace na values by zero
#          lag2 = dplyr::lag(lag1),             # create 2nd lags to be sure
#          lag2 = ifelse(is.na(lag2), 0, lag2), 
#          lead1 = dplyr::lead(connected), 
#          lead1 = ifelse(is.na(lead1), 0, lead1), # lead, just to be sure 
#          lead2 = dplyr::lead(lead1), 
#          lead2 = ifelse(is.na(lead2), 0, lead2), 
#          switch = connected - lag1,  # indicates the direction of switch
#          untreated = min(switch)) |>  # indicates if any governor was ever untreated (i.e. wrong direction)
#   ungroup() |>
#   filter(!untreated == -1) # filter out governors with "wrong" switch direction
#   
# # create a variable indicating when an observation has been treated first
# patronage_did <- patronage_did |>
#   group_by(ever_treated, aid) |>
#   mutate(first = ifelse(switch == 1, year, 0)) |>
#   ungroup()

# # delete observations that are treatet from the beginning
# patronage_did <- patronage_did |>
#   filter(!first %in% c(1850, 1851, 1852, 1854, 1856))

# # Callarway-Sant'Anna estimator (takes 20 minutes, reports "no pre-treatment periods to test")
# event_study <- did::att_gt(yname = "log_rev_total_gbp",
#                 tname = "year",
#                 idname = "aid",
#                 gname = "first",
#                 xformla = ~ no_colonies,
#                 data = patronage_did,
#                 control_group = "nevertreated",
#                 allow_unbalanced_panel = F,
#                 panel = F)
# beepr::beep(2)

# 

