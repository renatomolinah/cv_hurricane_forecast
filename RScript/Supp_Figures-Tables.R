######### Hurricane Survey Project -- Supplement Paper Tables and Figures 

######################################################
# Table S3/S4. Bid Amount for Florence/Michael --------
#####################################################

##Libraries
library(tidyverse)
library(tidylog)
library(kableExtra)
library(here)

#data 
data = data.table::fread(here("OutputData", "Survey_Master.csv")) %>%
  mutate(id = row_number())

##separate table by question type 
track = data %>%
  select(id, hurricane, answer1 = track_answer1, answer2 = track_answer2, bid = track_bid1) %>%
  mutate(question = "track") 
wind = data %>%
  select(id, hurricane, answer1 = wind_answer1, answer2 = wind_answer2, bid = wind_bid1) %>%
  mutate(question = "wind")
rain = data %>%
  select(id, hurricane, answer1 = rain_answer1, answer2 = rain_answer2, bid = rain_bid1) %>% 
  mutate(question = "rain")
stack = rbind(track, wind, rain)

##create bid and answer categories 
stack.yn = stack %>%
  mutate(yesno = ifelse(answer1 == 1 & answer2 == 1, "Y/Y", 
                        ifelse(answer1 == 1 & answer2 == 0, "Y/N",  
                               ifelse(answer1 == 0 & answer2 == 1, "N/Y", "N/N"))),
         bid.bin = cut(bid, breaks = c(0, 10, 20, 30, 40, 50), labels = c("1-10", "11-20", "21-30",
                                                                          "31-40", "41-50")))
##florence yes/no total 
stack.yn %>%
  filter(hurricane == "florence") %>%
  group_by(question,yesno, bid.bin) %>%
  summarize(no.ans = n()) %>% 
  pivot_wider(id_cols = c(question, bid.bin), names_from = yesno, values_from = no.ans) %>%
  mutate(question = " ") 
  kbl(format = "latex", booktabs = T, caption = "Florence Bid Answers N = 3150",
      col.names = c(" ", "Bid ($)", "No/No", "No/Yes", "Yes/No", "Yes/Yes")) %>%
  kable_styling() %>%
  pack_rows("Rain", 1, 5) %>%
  pack_rows("Track", 6, 10) %>%
  pack_rows("Wind", 11, 15) 
  save_kable(file = here("Flor_Yes-No.tex"))

  ##Michael yes/no total
  stack.yn %>%
    filter(hurricane == "michael") %>%
    group_by(question,yesno, bid.bin) %>%
    summarize(no.ans = n()) %>%
    pivot_wider(id_cols = c(question, bid.bin), names_from = yesno, values_from = no.ans) %>%
    mutate(question = " ") %>%
    kbl(format = "latex", booktabs = T, caption = "Michael Bid Answers N = 1500",
        col.names = c(" ", "Bid ($)", "No/No", "No/Yes", "Yes/No", "Yes/Yes")) %>%
    kable_styling() %>%
    pack_rows("Rain", 1, 5) %>%
    pack_rows("Track", 6, 10) %>%
    pack_rows("Wind", 11, 15)  %>%
    save_kable(file = here("Mich_Yes-No.tex"))
  
####################################################
# Table S10. Representative Survey Response --------
####################################################

##census data 
##data "var_.." are MOE originally and transformed into variance measures in first step of census setup 
census.data = readxl::read_excel(here("InputData", "ACSSPP1Y2018", "representation_table.xlsx"), skip = 2,
                            col_names = c("state", "tot.pop",
                                          "pop18_24", "var_pop18_24", "pop25_34", "var_pop25_34", "pop35_44", "var_pop35_44",
                                          "pop45_54", "var_pop45_54", "pop55_64", "var_pop55_64", "pop65_74", "var_pop65_74",
                                          "pop75up", "var_pop75up", "male18up", "var_male18up", "female18up", "var_female18up",
                                          "avg_housesize", "var_avg_housesize", "houses", "var_houses",
                                          "ownr_occ_per", "var_ownr_occ_per", 
                                          "rntr_occ_per", "var_rntr_occ_per", "median.age")) %>%
    select(-median.age) %>%
    mutate(state = str_to_lower(state))
survey = data.table::fread(here("OutputData","Survey_Master.csv"),
                           colClasses = c(zcta = "character")) %>%
  select(hurricane, zcta, age, female, owner, hh_size, mean.inc18) 
  
wind = data.table::fread(here("InputData", "WindSwathMaster.csv"),
                         colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = "")) %>%
  filter(bt_speed_max >= 30) ##Exposed Areas experience >= 30 WS
  
##zcta income 
zcta_atts = data.table::fread(here("OutputData", "ZCTA_AvgData.csv"), 
                              colClasses = c(zcta = "character")) %>%
  select(zcta, mean.inc18, mean.inc18.moe) %>%
  filter(zcta %in% wind$zcta)
##create variance column to find average variance (converts MOE: margin of error) 
census.data.var = census.data %>%
  mutate_at(vars(starts_with("var")), function(x, na.rm = T) (x/1.645)^2)

##data frame
census.data.4.table = census.data.var %>%
  filter(state %in% wind$state) %>%
  mutate(pop55up = pop55_64 + pop65_74 + pop75up,
         var55up = var_pop55_64 + var_pop65_74 + var_pop75up) %>%
  select(-c(pop55_64, pop65_74, pop75up, var_pop55_64, var_pop65_74, var_pop75up))


##US Average of 51 "states" (DC included) 
US_AVG =  apply(census.data.4.table[,-1], 2, function(x) mean(x, na.rm = T))

##create vector of means 
us_means = c(US_AVG[c("pop18_24", "pop25_34", "pop35_44", "pop45_54", "pop55up", "male18up", "female18up", 
                      "avg_housesize", "ownr_occ_per", "rntr_occ_per")], 
             mean(zcta_atts$mean.inc18, na.rm = T))
names.mean = c(names(us_means[-11]), "mean.inc")
names(us_means) = names.mean

tibble(variables = c("Population 18 - 24", "Population 24 - 34", "Population 35 - 44", "Population 45 - 54", "Population 55+",
                     "Male Population", "Female Population", "Housesize", "Owner Occupied Housing", "Renter Occupied Housing",
                     "Income"),
       means = us_means)


##Full Survey Set Up 
surv.age = survey %>%
  mutate(pop18_24 = ifelse(age >= 18 & age <= 24, 1, 0),
         pop25_34 = ifelse(age >= 25 & age <= 34, 1, 0),
         pop35_44 = ifelse(age >= 35 & age <= 44, 1, 0), 
         pop45_54 = ifelse(age >= 45 & age <= 54, 1, 0), 
         pop55up = ifelse(age >= 55, 1, 0),
         male = ifelse(female == 0, 1, 0),
         renter = ifelse(owner == 0, 1, 0)) %>%
  select(pop18_24, pop25_34, pop35_44, pop45_54, pop55up, male, female, hh_size, owner, renter, inc = mean.inc18) %>%
  mutate(pop18_24 = pop18_24*100, 
         pop25_34 = pop25_34*100, 
         pop35_44 = pop35_44*100, 
         pop45_54 = pop45_54*100, 
         pop55up = pop55up*100, 
         male = male*100, 
         female = female*100,
         owner = owner*100, 
         renter = renter*100)

surv.means = apply(surv.age, 2, function(x) mean(x, na.rm = T))
surv.sd = apply(surv.age, 2, function(x) sd(x, na.rm = T))


tibble(c("Population 18 - 24", "Population 24 - 34", "Population 35 - 44", "Population 45 - 54", "Population 55+",
         "Male Population", "Female Population", "Housesize", "Owner Occupied Housing", "Renter Occupied Housing",
         "Income"),
       surv.means, 
       surv.sd)


##Michael Set Up  
mich.age = survey %>%
  filter(hurricane == "michael") %>%
  mutate(pop18_24 = ifelse(age >= 18 & age <= 24, 1, 0),
         pop25_34 = ifelse(age >= 25 & age <= 34, 1, 0),
         pop35_44 = ifelse(age >= 35 & age <= 44, 1, 0), 
         pop45_54 = ifelse(age >= 45 & age <= 54, 1, 0), 
         pop55up = ifelse(age >= 55, 1, 0),
         male = ifelse(female == 0, 1, 0),
         renter = ifelse(owner == 0, 1, 0)) %>%
  select(pop18_24, pop25_34, pop35_44, pop45_54, pop55up, male, female, hh_size, owner, renter, inc = mean.inc18) %>%
  mutate(pop18_24 = pop18_24*100, 
         pop25_34 = pop25_34*100, 
         pop35_44 = pop35_44*100, 
         pop45_54 = pop45_54*100, 
         pop55up = pop55up*100, 
         male = male*100, 
         female = female*100,
         owner = owner*100,
         renter = renter*100)

mich.means= apply(mich.age, 2, function(x) mean(x, na.rm = T))
mich.sd = apply(mich.age, 2, function(x) sd(x, na.rm = T))

tibble(c("Population 18 - 24", "Population 24 - 34", "Population 35 - 44", "Population 45 - 54", "Population 55+",
         "Male Population", "Female Population", "Housesize", "Owner Occupied Housing","Renter Occupied Housing",
         "Income"),
       mich.means, 
       mich.sd)

##Florence Set Up 
flo.age = survey %>%
  filter(hurricane == "florence") %>%
  mutate(pop18_24 = ifelse(age >= 18 & age <= 24, 1, 0),
         pop25_34 = ifelse(age >= 25 & age <= 34, 1, 0),
         pop35_44 = ifelse(age >= 35 & age <= 44, 1, 0), 
         pop45_54 = ifelse(age >= 45 & age <= 54, 1, 0), 
         pop55up = ifelse(age >= 55, 1, 0),
         male = ifelse(female == 0, 1, 0),
         renter = ifelse(owner == 0, 1, 0)) %>%
  select(-c(age, hurricane, zcta)) %>%
  select(pop18_24, pop25_34, pop35_44, pop45_54, pop55up, male, female, hh_size, owner, renter, inc = mean.inc18)  %>%
  mutate(pop18_24 = pop18_24*100, 
         pop25_34 = pop25_34*100, 
         pop35_44 = pop35_44*100, 
         pop45_54 = pop45_54*100, 
         pop55up = pop55up*100, 
         male = male*100, 
         female = female*100,
         owner = owner*100,
         renter = renter*100)

flo.means = apply(flo.age, 2, function(x) mean(x, na.rm = T))
flo.sd = apply(flo.age, 2, function(x) sd(x, na.rm = T))

tibble(c("Population 18 - 24", "Population 24 - 34", "Population 35 - 44", "Population 45 - 54", "Population 55+",
         "Male Population", "Female Population", "Housesize", "Owner Occupied Housing", "Renter Occupied Housing",
         "Income") ,
       flo.means, 
       flo.sd)


##Creating OutPut table with t State and p-value 
rep.table = tibble("variables" = c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55","PopMale", 
                                   "PopFem", "Housesize", "Owner", "Renter", "Income"), 
                   "adult_mean" = us_means,
                   "full_mean" = surv.means,
                   "full_sd" = surv.sd,
                   "florence_mean" = flo.means,
                   "florence_sd" = flo.sd, 
                   "michael_mean" = mich.means,
                   "michael_sd" = mich.sd)

rep.table.out = rep.table %>%
  mutate(diff.mean_full = (full_mean - adult_mean),
         diff.mean.flo = (florence_mean - adult_mean),
         diff.mean.mich = (michael_mean - adult_mean),
         se_full = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                          full_sd/sqrt(4587),
                          full_sd/sqrt(4650)),
         se_flo = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                         florence_sd/sqrt(3119),
                         florence_sd/sqrt(3150)),
         se_mich = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                          michael_sd/sqrt(1468),
                          michael_sd/sqrt(1500)),
         t_stat_full = diff.mean_full / se_full,
         t_stat_flo = diff.mean.flo / se_flo,
         t_stat_mich = diff.mean.mich / se_mich, 
         pval_full = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                            2*pt(-abs(t_stat_full), df = 4587 - 1),
                            2*pt(-abs(t_stat_full), df = 4650 - 1)),
         pval_flo = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                           2*pt(-abs(t_stat_flo), df = 3119 - 1),
                           2*pt(-abs(t_stat_flo), df = 3150 - 1)),
         pval_mich = ifelse(variables %in% c("Pop18", "Pop25", "Pop35", "Pop45", "Pop55"),
                            2*pt(-abs(t_stat_mich), df = 1468 - 1),
                            2*pt(-abs(t_stat_mich), df = 1500 - 1)))

##use p-values in this table to manually input starts in output table 
rep.table.out %>%
  select(adult_mean, full_mean, diff.mean_full, florence_mean, diff.mean.flo, michael_mean, diff.mean.mich,
         pval_full, pval_flo, pval_mich)

rep.table.out %>%
  select(adult_mean, full_mean, diff.mean_full, florence_mean, diff.mean.flo, michael_mean, diff.mean.mich) %>%
  mutate(demos = c("Population 18-24 (%)", "Population 25-34 (%)", "Population 35-44 (%)", "Population 45-54 (%)",
                   "Population 55+ (%)", "Male (%)", "Female (%)", "Household Size (#)", "Owner (%)", "Renter (%)",
                   "Income ($)")) %>%
  kbl(format = "latex", booktabs = T, col.names = c("Demographics", "Mean", "Mean", "Difference", "Mean", "Difference",
                                                    "Mean", "Difference")) %>%
  add_header_above(linebreak(c(" " = 1, "Exposed Population" = 1, "Full Survey" = 2, "Florence" = 2, "Michael" = 2))) %>%
  save_kable(here("Representative-Table.tex"))

##Notes - Need to add stars based on p-values and adjust Income to be 1e3 

################################################
# Figure S1. Sample vs Exposed Pop Means-------
################################################
#clear data
rm(list = ls())
#data from Survey Estimates vs Estimates with Exposed Population Stats
exposed.wtp = tribble(
  ~Method, ~Sample, ~WTP, ~Beta, ~SE,
  "Main", "Full Sample", "Track", 26.07, 3.57,
  "Main", "Full Sample", "Wind", 28.89, 3.62, 
  "Main", "Full Sample", "Rain", 21.63, 3, #3
  "Main", "Florence", "Track", 24.24, 4.26, 
  "Main", "Florence", "Wind", 30.38, 4.52, 
  "Main", "Florence", "Rain", 25.67, 3.84, #3
  "Main", "Michael", "Track", 10.19, 4.36, 
  "Main", "Michael", "Wind", 24.11, 5.20, 
  "Main", "Michael", "Rain", 10.08, 4.10, #3
  "Exposed30", "Full Sample", "Track", 25.85, 3.56,
  "Exposed30", "Full Sample", "Wind", 28.89, 3.62, 
  "Exposed30", "Full Sample", "Rain", 20.90, 2.91, #3 
  "Exposed30", "Florence", "Track", 24.50, 4.24, 
  "Exposed30", "Florence", "Wind", 30.90, 4.55, 
  "Exposed30", "Florence", "Rain", 25.26, 3.75, #3
  "Exposed30", "Michael", "Track", 10.94, 4.40, 
  "Exposed30", "Michael", "Wind", 23.67, 5.09, 
  "Exposed30", "Michael", "Rain", 9.11, 4.01)  #3

dodge = position_dodge(width = .5) 
exposed.plot = ggplot(exposed.wtp, 
                      aes(x = factor(WTP, levels = c("Track", "Wind", "Rain")),
                          y = Beta, color = factor(Method, levels = c("Main", "Exposed30")))) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = Beta - 1.96*SE, ymax = Beta + 1.96*SE) , 
                width = .2, position = dodge) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = c("#000000", "#6c6c6c"), labels = c("Sample", 
                                                                  "Exposed (>30 mph)")) + 
  facet_wrap(~ factor(Sample, levels = c("Full Sample", "Florence", "Michael"))) + 
  ylim(-2, 45) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL, color = "Means Used:") +
  theme_bw() + 
  theme(
    text = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, size = 5, hjust = 1),
    legend.position = "bottom", 
    legend.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5)) 

ggsave(here("WTP-SamplevsExposed_Plot.eps"), exposed.plot, width = 4, height = 3)

################################################
# Figure S2. SB vs DB Estimates ----------------
################################################
#clear data 
rm(list = ls())

#data
#use full control estimates for DBDC and SBDC 
estimates2 = tribble(
  ~samp, ~est, ~wtp, ~beta, ~se, 
  "Full Sample", "Single Bound", "Track", 20.56, 4.12,
  "Full Sample", "Single Bound", "Wind", 43.56, 6.60,
  "Full Sample", "Single Bound", "Rain", 28.89, 8.42, #3
  "Florence", "Single Bound", "Track", 24.61, 7.51,
  "Florence", "Single Bound", "Wind", 23.88, 7.17,
  "Florence", "Single Bound", "Rain", 28.14, 10.23, #3
  "Michael", "Single Bound", "Track", 11.43, 7.22,
  "Michael", "Single Bound", "Wind", 44.97, 10.91, 
  "Michael", "Single Bound", "Rain", 25.01, 11.10, #3
  "Full Sample", "Double Bound", "Track", 26.07, 3.57, 
  "Full Sample", "Double Bound", "Wind", 28.89, 3.62, 
  "Full Sample", "Double Bound", "Rain", 21.63, 3.00, #3
  "Florence", "Double Bound", "Track", 24.24, 4.26, 
  "Florence", "Double Bound", "Wind", 30.38, 4.52, 
  "Florence", "Double Bound", "Rain", 25.67, 3.84, #3
  "Michael", "Double Bound", "Track", 10.19, 4.36, 
  "Michael", "Double Bound", "Wind", 24.11, 5.20, 
  "Michael", "Double Bound", "Rain", 10.08, 4.10  #3
)

dodge = position_dodge(width = 0.5) 
myplot2 = ggplot(estimates2, aes(x = factor(wtp, levels = c("Track", "Wind", "Rain")), y = beta, 
                                 group = factor(est, levels = c("Double Bound", "Single Bound"))
                                 , color = factor(est, levels = c("Double Bound", "Single Bound")))) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se) , 
                width = .2, position = dodge) + 
  facet_wrap(~ factor(samp, levels = c("Full Sample", "Florence", "Michael"))) + 
  scale_color_manual(values = c("#000000", "#6c6c6c")) + 
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() + 
  # ylim(-2, 40) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL, color = "Elicitation Method:") +
  theme(text = element_text(size = 6), 
        axis.text.x = element_text(angle = 45, size = 5, hjust = 1),
        legend.position = "bottom", 
        legend.title = element_text(size = 5.5),
        legend.text = element_text(size = 5.5)) 

ggsave(here("figure_comparison_single.eps"), myplot2, width = 4, height = 3) 

################################################
# Figure S3. Compare Income Levels ------------
################################################

#clear data 
rm(list = ls())
#data with 2018 and 2008 incomes 
income.wtp = tribble(
  ~Method, ~Sample, ~WTP, ~Beta, ~SE,
  "2018 Income", "Full Sample", "Track", 26.07, 3.57,
  "2018 Income", "Full Sample", "Wind", 28.89, 3.62, 
  "2018 Income", "Full Sample", "Rain", 21.63, 3, #3
  "2018 Income", "Florence", "Track", 24.24, 4.26, 
  "2018 Income", "Florence", "Wind", 30.38, 4.52, 
  "2018 Income", "Florence", "Rain", 25.67, 3.84, #3
  "2018 Income", "Michael", "Track", 10.19, 4.36, 
  "2018 Income", "Michael", "Wind", 24.11, 5.20, 
  "2018 Income", "Michael", "Rain", 10.08, 4.10,  #3
  "2008 Income", "Full Sample", "Track", 24.47, 3.58,
  "2008 Income", "Full Sample", "Wind", 22.23, 2.73,
  "2008 Income", "Full Sample", "Rain", 21.48, 3, #3
  "2008 Income", "Florence", "Track", 18.21, 3.17,  
  "2008 Income", "Florence", "Wind" , 22.76, 3.31,
  "2008 Income", "Florence", "Rain", 25.66, 3.84, 
  "2008 Income", "Michael", "Track", 10.21, 4.36,
  "2008 Income", "Michael", "Wind", 24.18, 5.20, 
  "2008 Income", "Michael", "Rain", 10.07, 4.10
)
dodge = position_dodge(width = .5) 
inc.plot = ggplot(income.wtp, 
                  aes(x = factor(WTP, levels = c("Track", "Wind", "Rain")),
                      y = Beta, color = factor(Method, levels = c("2018 Income", "2008 Income")))) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = Beta - 1.96*SE, ymax = Beta + 1.96*SE) , 
                width = .2, position = dodge) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = c("#000000", "#6c6c6c"), labels = c("2018 ACS", "2007 - 2011 ACS")) + 
  facet_wrap(~ factor(Sample, levels = c("Full Sample", "Florence", "Michael"))) + 
  ylim(-2, 40) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL, color = "Income Source:") +
  theme_bw() + 
  theme(
    text = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, size = 5, hjust = 1),
    legend.position = "bottom", 
    legend.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5)) 

ggsave(here("income.source.eps"), inc.plot, width = 4, height = 3)

################################################
# Figure S4. Compare Order Specification -------
################################################

#clear data 
rm(list = ls())

#data 
disc.est = tribble(
  ~Method, ~Sample, ~WTP, ~Beta, ~SE,
  "Continuous Order", "Full Sample", "Track", 26.07, 3.57,
  "Continuous Order", "Full Sample", "Wind", 28.89, 3.62, 
  "Continuous Order", "Full Sample", "Rain", 21.63, 3, 
  "Continuous Order", "Florence", "Track", 24.24, 4.26, 
  "Continuous Order", "Florence", "Wind", 30.38, 4.52, 
  "Continuous Order", "Florence", "Rain", 25.67, 3.84, #3
  "Continuous Order", "Michael", "Track", 10.19, 4.36, 
  "Continuous Order", "Michael", "Wind", 24.11, 5.20, 
  "Continuous Order", "Michael", "Rain", 10.08, 4.10,  #3
  "Discrete Order", "Full Sample", "Track", 29.97, 3.41, 
  "Discrete Order", "Full Sample", "Wind", 25.72, 2.44, 
  "Discrete Order", "Full Sample", "Rain", 25.75, 2.74, #3
  "Discrete Order", "Florence", "Track", 28.47, 4.06, 
  "Discrete Order", "Florence", "Wind", 35.69, 4.27, 
  "Discrete Order", "Florence", "Rain", 29.00, 3.53,  
  "Discrete Order", "Michael", "Track", 13.41, 3.80, 
  "Discrete Order", "Michael", "Wind", 25.46, 4.77, 
  "Discrete Order", "Michael", "Rain", 15.83, 3.42)

dodge = position_dodge(width = .5) 
comp.plot = ggplot(disc.est %>% filter(Method != "Single Bound"), 
                   aes(x = factor(WTP, levels = c("Track", "Wind", "Rain")),
                       y = Beta, color = Method)) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = Beta - 1.96*SE, ymax = Beta + 1.96*SE) , 
                width = .2, position = dodge) + 
  facet_wrap(~ factor(Sample, levels = c("Full Sample", "Florence", "Michael"))) + 
  scale_color_manual(values = c("#000000", "#6c6c6c"), labels = c("As Continuous", "As Factor")) + 
  geom_hline(yintercept = 0, lty = 2) +
  ylim(-2, 45) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL, color = "Order Specification:") +
  theme_bw() + 
  theme(
    text = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, size = 5, hjust = 1),
    legend.position = "bottom", 
    legend.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5)) 

ggsave(here("figure_comparison_order.eps"), comp.plot, width = 4, height = 3)

################################################
# Figure S5. Compare with Extra Controls -------
################################################

#clear data 
rm(list = ls())

#data 
controls.wtp = tribble(
  ~Method, ~Sample, ~WTP, ~Beta, ~SE,
  "Main", "Full Sample", "Track", 26.07, 3.57,
  "Main", "Full Sample", "Wind", 28.89, 3.62, 
  "Main", "Full Sample", "Rain", 21.63, 3, #3
  "Main", "Florence", "Track", 24.24, 4.26, 
  "Main", "Florence", "Wind", 30.38, 4.52, 
  "Main", "Florence", "Rain", 25.67, 3.84, #3
  "Main", "Michael", "Track", 10.19, 4.36, 
  "Main", "Michael", "Wind", 24.11, 5.20, 
  "Main", "Michael", "Rain", 10.08, 4.10,  #3
  "Extra Controls", "Full Sample", "Track", 28.90, 3.59,
  "Extra Controls", "Full Sample", "Wind", 23.28, 3.65,
  "Extra Controls", "Full Sample", "Rain", 25.27, 3.41, #3
  "Extra Controls", "Florence", "Track", 31.72, 4.40,  
  "Extra Controls", "Florence", "Wind" , 24.78, 4.70,
  "Extra Controls", "Florence", "Rain", 29.84, 4.28, 
  "Extra Controls", "Michael", "Track", 30.14, 7.15,
  "Extra Controls", "Michael", "Wind", 27.04, 6.68, 
  "Extra Controls", "Michael", "Rain", 22.94, 8.08
)
dodge = position_dodge(width = .5) 
control.plot = ggplot(controls.wtp, 
                      aes(x = factor(WTP, levels = c("Track", "Wind", "Rain")),
                          y = Beta, color = factor(Method, levels = c("Main", "Extra Controls")))) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = Beta - 1.96*SE, ymax = Beta + 1.96*SE) , 
                width = .2, position = dodge) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = c("#000000", "#6c6c6c"), labels = c("1 + 2", 
                                                                  "1 + 2 + 3")) + 
  facet_wrap(~ factor(Sample, levels = c("Full Sample", "Florence", "Michael"))) + 
  ylim(-2, 45) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL, color = "Control Sets Used:") +
  theme_bw() + 
  theme(
    text = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, size = 5, hjust = 1),
    legend.position = "bottom", 
    legend.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5)) 

ggsave(here("WTP-Inc2018vsExtraControls_Plot.eps"), control.plot, width = 4, height = 3)


