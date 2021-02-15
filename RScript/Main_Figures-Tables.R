######### Hurricane Survey Project -- Main Paper Tables and Figures 

############################################
# Table 1. Extrapolation of WTP -----------
###########################################

#library
library(tidyverse)
library(tidylog)
library(here)
library(kableExtra)

#data 
#survey data 
survey = data.table::fread(here("OutputData", "Survey_Master.csv"),
                           colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""))
##total windswath 
wind = data.table::fread(here("InputData", "WindSwathMaster.csv"),
                         colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""),
         med.inc = as.numeric(med.inc), 
         mean.inc = as.numeric(mean.inc))

##population count per county 
county.pop = data.table::fread(here("OutputData/CountyPopTotal.csv"),
                               colClasses = c(GEOID = "character"))
##occupied housing count per county 
county.house = data.table::fread(here("OutputData/CountyHousingTotal.csv"), 
                                 colClasses = c(GEOID = "character")) 

county.total = inner_join(county.pop, 
                          county.house %>%
                            select(GEOID, county.occ), 
                          by = "GEOID")

wind.counties = inner_join(wind, 
                           county.total, 
                           by = c("geoid" = "GEOID")) %>%
  rename(count.pop = county.pop, count.home = county.occ) %>%
  mutate(o50 = ifelse(bt_speed_max > 50, 1, 0),
         o40 = ifelse(bt_speed_max > 40, 1, 0), 
         o30 = ifelse(bt_speed_max > 30, 1, 0), 
         o20 = ifelse(bt_speed_max > 20, 1, 0)) %>%
  group_by(geoid) %>%
  slice(which.max(bt_speed_max)) %>% ##assigns each GEOID to highest experienced wind speed bin 
  ungroup() 

survey.counties = wind.counties %>%
  filter(geoid %in% survey$geoid)

##estimates for {Sample, 50,40,30,20} (order of estimates) thresholds 
track.est = c(26.07, 23.95, 23.95, 23.95, 24.05)
track.se = c(3.57, 3.21, 3.21, 3.21, 3.21) 
wind.est = c(28.89, 27.29, 27.29, 27.29, 27.29) 
wind.se = c(3.62, 3.28, 3.28, 3.29, 3.28)
rain.est = c(21.63, 20.86, 20.86, 20.86, 20.88)
rain.se = c(3.00, 2.91, 2.91, 2.91, 2.91)
##bind estimates
estimates = cbind(track.est, wind.est, rain.est) 
std.er = cbind(track.se, wind.se, rain.se) 
##empty data frames
wtp = matrix(0, length(track.est), 3) 
wtp.se = matrix(0,length(track.est),3)
##wtp extrapolation 
for(j in 1:3){
  wtp[1,j] = sum(estimates[1,j]*survey.counties$count.home)
  wtp[2,j] = sum(estimates[2,j]*wind.counties[wind.counties$o50 == 1,]$count.home)
  wtp[3,j] = sum(estimates[3,j]*wind.counties[wind.counties$o40 == 1,]$count.home) 
  wtp[4,j] = sum(estimates[4,j]*wind.counties[wind.counties$o30 == 1,]$count.home)
  wtp[5,j] = sum(estimates[5,j]*wind.counties[wind.counties$o20 == 1,]$count.home)
}
wtp
##wtp extrap CI (delta method) 
for(j in 1:3){
  wtp.se[1,j] = sqrt(sum(survey.counties$count.home)^2*(std.er[1,j]^2))*1.96
  wtp.se[2,j] = sqrt(sum(wind.counties[wind.counties$o50 == 1,]$count.home)^2*(std.er[2,j]^2))*1.96
  wtp.se[3,j] = sqrt(sum(wind.counties[wind.counties$o40 == 1,]$count.home)^2*(std.er[3,j]^2))*1.96
  wtp.se[4,j] = sqrt(sum(wind.counties[wind.counties$o30 == 1,]$count.home)^2*(std.er[4,j]^2))*1.96
  wtp.se[5,j] = sqrt(sum(wind.counties[wind.counties$o20 == 1,]$count.home)^2*(std.er[5,j]^2))*1.96
}
wtp.se

##wtp table section creation 
twtp = t(wtp) %>% data.frame() %>% rename(Sample = X1, WS50 = X2, WS40 = X3, WS30 = X4, WS20 = X5)
twtp.se = t(wtp.se) %>% data.frame() %>% rename(Sample.se = X1, WS50.se = X2, WS40.se = X3, WS30.se = X4, WS20.se = X5)
wtp.section = cbind(twtp, twtp.se) %>%
  select(Sample, Sample.se, WS50, WS50.se, WS40, WS40.se, WS30, WS30.se, WS20, WS20.se) 

##wtp per person extrapolation 
wtpp = matrix(0, length(track.est), 3) %>% data.frame()
wtpp.se = matrix(0,length(track.est),3) %>% data.frame()
for(j in 1:3){
  wtpp[1,j] = round(mean((estimates[1,j]*survey.counties$count.home)/
                           (survey.counties$count.pop)),2)
  wtpp[2,j] = round(mean((estimates[2,j]*wind.counties[wind.counties$o50 == 1,]$count.home)/
                           (wind.counties[wind.counties$o50 == 1,]$count.pop)),2)
  wtpp[3,j] = round(mean((estimates[3,j]*wind.counties[wind.counties$o40 == 1,]$count.home)/
                           (wind.counties[wind.counties$o40 == 1,]$count.pop)),2)
  wtpp[4,j] = round(mean((estimates[4,j]*wind.counties[wind.counties$o30 == 1,]$count.home)/
                           (wind.counties[wind.counties$o30 == 1,]$count.pop)),2)
  wtpp[5,j] = round(mean((estimates[5,j]*wind.counties[wind.counties$o20 == 1,]$count.home)/
                           (wind.counties[wind.counties$o20 == 1,]$count.pop)),2)
}
wtpp
##wtp per person extrapolation CI (delta method) 
for(j in 1:3){
  wtpp.se[1,j] = sqrt(mean((survey.counties$count.home)/
                             (survey.counties$count.pop))^2*(std.er[1,j]^2))*1.96
  wtpp.se[2,j] = sqrt(mean((wind.counties[wind.counties$o50 == 1,]$count.home)/
                             (wind.counties[wind.counties$o50 == 1,]$count.pop))^2*(std.er[2,j]^2))*1.96
  wtpp.se[3,j] = sqrt(mean((wind.counties[wind.counties$o40 == 1,]$count.home)/
                             (wind.counties[wind.counties$o40 == 1,]$count.pop))^2*(std.er[3,j]^2))*1.96
  wtpp.se[4,j] = sqrt(mean((wind.counties[wind.counties$o30 == 1,]$count.home)/
                             (wind.counties[wind.counties$o30 == 1,]$count.pop))^2*(std.er[4,j]^2))*1.96
  wtpp.se[5,j] = sqrt(mean((wind.counties[wind.counties$o20 == 1,]$count.home)/
                             (wind.counties[wind.counties$o20 == 1,]$count.pop))^2*(std.er[5,j]^2))*1.96
  
}
wtpp.se
##wtpp table section creation 
twtpp = t(wtpp) %>% data.frame() %>% rename(Sample = X1, WS50 = X2, WS40 = X3, WS30 = X4, WS20 = X5)
twtpp.se = t(wtpp.se) %>% data.frame() %>% rename(Sample.se = X1, WS50.se = X2, WS40.se = X3, WS30.se = X4, WS20.se = X5)
wtpp.section = cbind(twtpp, twtpp.se) %>%
  select(Sample, Sample.se, WS50, WS50.se, WS40, WS40.se, WS30, WS30.se, WS20, WS20.se) 

##npv extrapolation 
npv = function(w, r){
  w + w/r*(1-(1/(1+r))^9)
}
r = 0.02 #discount rate 
npv.df = matrix(0, length(track.est), 3) %>% data.frame()
npv.df.se = matrix(0,length(track.est),3) %>% data.frame()
for(j in 1:3){
  npv.df[1,j] = npv(wtpp[1,j], r)
  npv.df[2,j] = npv(wtpp[2,j], r)
  npv.df[3,j] = npv(wtpp[3,j], r)
  npv.df[4,j] = npv(wtpp[4,j], r)
  npv.df[5,j] = npv(wtpp[5,j], r)
}

##npv extrapolation CI (delta method) 
multiplier = function(r){ #for g'() 
  ((1-(1/((1+r)^9)))/r) + 1
}
for(j in 1:3){
  npv.df.se[1,j] = sqrt((multiplier(r)*mean((survey.counties$count.home)/
                                              survey.counties$count.pop))^2*
                          (std.er[1,j]^2))*1.96
  npv.df.se[2,j] = sqrt((multiplier(r)*mean((wind.counties[wind.counties$o20 == 1,]$count.home)/
                                              wind.counties[wind.counties$o20 == 1,]$count.pop))^2*
                          (std.er[2,j]^2))*1.96
  npv.df.se[3,j] = sqrt((multiplier(r)*mean((wind.counties[wind.counties$o30 == 1,]$count.home)/
                                              wind.counties[wind.counties$o30 == 1,]$count.pop))^2*
                          (std.er[3,j]^2))*1.96
  npv.df.se[4,j] = sqrt((multiplier(r)*mean((wind.counties[wind.counties$o40 == 1,]$count.home)/
                                              wind.counties[wind.counties$o40 == 1,]$count.pop))^2*
                          (std.er[4,j]^2))*1.96
  npv.df.se[5,j] = sqrt((multiplier(r)*mean((wind.counties[wind.counties$o50 == 1,]$count.home)/
                                              wind.counties[wind.counties$o50 == 1,]$count.pop))^2*
                          (std.er[5,j]^2))*1.96
}
npv.df.se

##npv table section creation 
tnpv = t(npv.df) %>% data.frame() %>% rename(Sample = X1, WS50 = X2, WS40 = X3, WS30 = X4, WS20 = X5)
tnpv.se = t(npv.df.se) %>% data.frame() %>% rename(Sample.se = X1, WS50.se = X2, WS40.se = X3, WS30.se = X4, WS20.se = X5)
npv.section = cbind(tnpv, tnpv.se) %>%
    select(Sample, Sample.se, WS50, WS50.se, WS40, WS40.se, WS30, WS30.se, WS20, WS20.se) 


extrap.table = rbind(wtp.section, wtpp.section, npv.section) 
table1 = matrix(0, ncol = 5, nrow = 9)

for(i in 1:nrow(extrap.table)){
  table1[i,1] = paste(extrap.table$Sample[i], "\\pm", extrap.table$Sample.se[i], sep = " ")
  table1[i,2] = paste(extrap.table$WS50[i], "\\pm", extrap.table$WS50.se[i], sep = " ")
  table1[i,3] = paste(extrap.table$WS40[i], "\\pm", extrap.table$WS40.se[i], sep = " ")
  table1[i,4] = paste(extrap.table$WS30[i], "\\pm", extrap.table$WS30.se[i], sep = " ") 
  table1[i,5] = paste(extrap.table$WS20[i], "\\pm", extrap.table$WS20.se[i], sep = " ")
}
table.out = cbind(rep(c("Storm Track", "Wind Speed", "Precipitation"),3), table1)

#creates table; tidy by hand 
kbl(table.out, format = "latex", booktabs = T,
    col.names = c(" ", "Sampled", ">50 mph", ">40 mph", ">30 mph", ">20 mph")) %>%
  group_rows("Total WTP USD", 1, 3) %>%
  group_rows("Per-Capita WTP", 4, 6) %>%
  group_rows("PV USD", 7, 9) %>%
  write(here("Figures", "Table1-Main.tex"))
  
  
##################################################
# Fig 4. Losses and Evacuation Maps --------------
##################################################

#restart R
.rs.restartR()

##libraries
library(tidyverse)
library(tidylog)
library(here)
library(tmap)
library(tmaptools)
library(sf)

##data 
survey = data.table::fread(here("OutputData", "Survey_Master.csv"),
                           colClasses = c(zcta = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"), 
         geoid = paste(statefp, countyfp, sep = ""),
         loss = ifelse(is.na(loss), "", loss),
         loss = as.factor(loss))

county = st_read(here("InputData", "tl_2019_us_county", "tl_2019_us_county.shp"))
county$GEOID = as.character(county$GEOID)

survey.sf = inner_join(survey, 
                       county %>%
                         select(GEOID, geometry), 
                       by = c("geoid" = "GEOID")) %>%
  st_as_sf() %>%
  st_transform('ESRI:102003') ##epsg:102003 is USA Contiguous Albers Equal Area Conic

states = st_read(here("InputData", "tl_2019_us_state", "tl_2019_us_state.shp")) %>%
  mutate(NAME = str_to_lower(NAME))
state.survey = states %>%
  filter(STATEFP %in% c(12, 13, 37, 45)) %>% #states in survey FL, GA, NC, SC
  st_transform('ESRI:102003')


florence.path = st_read(here("InputData", "Florence_TRACK", "AL062018_lin.shp")) %>%
  st_transform('ESRI:102003')

michael.path = st_read(here("InputData", "Michael_TRACK", "AL142018_lin.shp")) %>%
  st_transform('ESRI:102003')

##Covariate Map Set Up

##evacuation 
evac.sf = survey.sf %>%
  add_count(unique.name) %>% #county.state is unique 
  group_by(unique.name) %>%
  mutate(tot.evac = sum(evacuate), 
         perc.evac = tot.evac/n) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf() %>%
  st_transform('ESRI:102003') 

##loss

loss.sf = survey.sf %>%
  group_by(unique.name, loss) %>%
  summarise(count = n())%>%
  filter(loss != "") %>%
  slice(which.max(count)) %>%
  ungroup()
loss.sf$loss = as.numeric(as.character(loss.sf$loss))

##percent evacuate by county responses map 

evac.map = tm_shape(state.survey) + 
  tm_borders(col = "black", lwd = 2) +
  tm_shape(evac.sf) + 
  tm_fill("perc.evac", title = "% Respondents Evacuated", style = "cont",
          palette = "Greens", legend.reverse = T) +
  tm_borders(lwd = 1) +
  tm_shape(florence.path) +
  tm_lines(col = "darkblue", lwd = 1, lty = "dashed") + 
  tm_shape(michael.path) +
  tm_lines(col = "black", lwd = 1, lty = "dashed") +
  tm_graticules(n.x = 1, n.y = 3, labels.size = .6, lines = T, alpha = .2) + 
  tm_credits("Michael Path", size = .5, col = "black", position = c(.21, .02)) +
  tm_credits("Florence Path", size = .5, col = "darkblue", position = c(.17, .9)) +  
  tm_layout(main.title = "                   a)    % Survey Respondents Evacuated By County", 
            main.title.size = .7, 
            main.title.position = "left", legend.title.size = .55, legend.text.size = .45,
            legend.position = c(.61, .41), inner.margins = c(.01, .01, .01, .01))
##loss
loss.map = tm_shape(state.survey) + 
  tm_borders(col = "black", lwd = 2) +
  tm_shape(loss.sf) + 
  tm_fill("loss", palette = c(tmaptools::get_brewer_pal("Purples", 3, plot = F), "grey75"), 
          title = "Most Frequent Loss Category", style = "cat",
          legend.format = list(fun = function(x) paste0(c("$0 - $999", "$1,000 - $4,999",
                                                          "$5,000 - $9,999", "Total Loss")))) +
  tm_borders(lwd = 1) + 
  tm_shape(florence.path) +
  tm_lines(col = "darkblue", lwd = 1, lty = "dashed") + 
  tm_shape(michael.path) +
  tm_lines(col = "black", lwd = 1, lty = "dashed") +
  tm_graticules(n.x = 1, n.y = 3, labels.size = .6, lines = T, alpha = .2) +
  tm_credits("Michael Path", size = .5, col = "black", position = c(.21, .02)) +
  tm_credits("Florence Path", size = .5, col = "darkblue", position = c(.17, .9)) + 
  tm_layout(main.title = "                  b)          Most Frequent Loss Category By County", 
            main.title.size = .7, 
            main.title.position = "left", legend.title.size = .55, legend.text.size = .45,
            legend.position = c(.60, .43), inner.margins = c(0.01, .01, .01, .01)) 
## Join Evacuation and Loss Maps 
survey.cov.maps = tmap_arrange(evac.map, loss.map, nrow = 1, asp = NULL)
tmap_save(survey.cov.maps, here("Figures", "CovariatesMap.jpeg"), height = 4.3, width = 7)

###########################################################
# Fig 5. Average WTP Hurricane Forecast Improvement ------
###########################################################

#restart R
.rs.restartR()

#libraries
library(tidyverse)
library(tidylog) 
library(here) 

##data (manuel type in) 
estimates = tribble(
  ~samp, ~wtp, ~betas, ~se, 
  "Full Sample", "Track", 26.07, 3.57,
  "Full Sample", "Wind", 28.89, 3.62,
  "Full Sample", "Rain", 21.63, 3.00,
  "Florence", "Track", 24.24, 4.26,
  "Florence", "Wind", 30.38, 4.52, 
  "Florence", "Rain", 25.67, 3.84,
  "Michael", "Track", 10.19, 4.36, 
  "Michael", "Wind", 24.11, 5.20, 
  "Michael", "Rain", 10.08, 4.10
)
##plot 
coef.plot = ggplot(estimates, aes(x = factor(wtp, levels = c("Track", "Wind", "Rain")),
                               y = betas)) +
  geom_point() + 
  geom_errorbar(aes(ymin = betas - 1.96*se, ymax = betas + 1.96*se) , 
                width = .2, position = position_dodge(0.05)) + 
  facet_wrap(~ factor(samp, levels = c("Full Sample", "Florence", "Michael"))) + 
  geom_hline(yintercept = 0, lty = 2) +
  ylim(-2, 40) +
  labs(x = NULL, y = "WTP Estimates (US$/household/year)",
       title = NULL) +
  theme_bw() + 
  theme(
    text = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, size = 5, hjust = 1)
  )
ggsave(here("Figures", "Coefficients.eps"), coef.plot, width = 4, height = 3)

#######################################
# Fig 6. Past Hurricane Exposure -----
######################################

#restart R
.rs.restartR()

#libraries 
library(cowplot)
library(here)
library(sf)
library(tidyverse)

##data 
wind = data.table::fread(here("InputData", "WindSwathMaster.csv"),
                         colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""))

county = st_read(here("InputData", "tl_2019_us_county", "tl_2019_us_county.shp"))
county$GEOID = as.character(county$GEOID)

states = st_read(here("InputData", "tl_2019_us_state", "tl_2019_us_state.shp")) %>%
  mutate(NAME = str_to_lower(NAME))

##set up 

##provide categories for windspeeds 
wind.cat = wind %>%
  select(geoid, bt_speed_max) %>%
  mutate(max.wind.cat = ifelse(bt_speed_max >= 50, 1, 
                               ifelse(bt_speed_max >= 40, 2, 
                                      ifelse(bt_speed_max >= 30, 3,
                                             ifelse(bt_speed_max >= 20, 4, 5))))) %>%
  filter(bt_speed_max >= 20) %>%
  mutate(max.wind.cat = as.factor(max.wind.cat))

##get unique counties based on max wind speed experienced 
wind.uniq = wind.cat %>%
  group_by(geoid) %>%
  slice(which.max(bt_speed_max))

##attach county geographies 
wind.geo = inner_join(wind.uniq, 
                      county %>%
                        select(GEOID, geometry),
                      by = c("geoid" = "GEOID")) %>%
  st_as_sf() %>%
  st_transform('ESRI:102003')

##identifying states we need for mapping 
big.state.wind = states %>%
  filter(NAME %in% unique(wind$state)) %>%
  st_transform('ESRI:102003')

##mapping 
key = c(">50 mph", ">40 mph", ">30 mph", ">20 mph")
color.code = c("#D01369","#F60A16","#F6B10A","#F4F60A")
wind.map = ggplot() +
  geom_sf(data = wind.geo, aes(fill = max.wind.cat), size = 0.1) +
  geom_sf(data = big.state.wind, color = "black", fill = "transparent", size = .45) +
  theme_bw() +
  theme(axis.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.width = unit(.55, "line"),
        legend.direction = "horizontal") +
  scale_fill_manual(values = color.code, labels = key) +
  labs(title = NULL, fill = "Maximum Winds")

ggsave(here("Figures", "WindSpeedMap.jpeg"), wind.map, width = 4.5, height = 4)



