######### Hurricane Survey Project -- Data Set Up Script 6/21/20

##libraries
library(tidyverse)
library(tidylog)

# Michael

#Load data
michael <- read.csv("./InputData/HurricaneMichael_FinalData.csv", comment.char="#",
                    stringsAsFactors = FALSE)

# Get rid of extra rows
michael <- michael[-1,]
michael <- michael[-1,]

# Convert time living in house in to net years

michael$Q02_1_TEXT <- as.numeric(michael$Q02_1_TEXT)
michael$Q02_1_TEXT[is.na(michael$Q02_1_TEXT)] <- 0

michael$Q02_2_TEXT <- as.numeric(michael$Q02_2_TEXT)
michael$Q02_2_TEXT[is.na(michael$Q02_2_TEXT)] <- 0


# Process controls

michael <- michael %>% mutate(hurricane = "michael",
                              
                              zip = Q75,
                              
                              age = Q76,
                              
                              female = ifelse(Q77 == "Female", 1, 0),
                              
                              owner = ifelse(Q01 == "Own", 1, 0),
                              
                              tenure = Q02_1_TEXT/12 + Q02_2_TEXT, # how many years have you lived in your house
                              
                              insured = ifelse(Q03a != "", 1, 0), # have insurance
                              
                              short_risk = ifelse(Q05 == "1 in 10,000", 1/10000,
                                                  ifelse(Q05 =="1 in 5,000", 1/5000, 
                                                         ifelse(Q05 == "1 in 1,000", 1/1000,
                                                                ifelse(Q05 == "1 in 500", 1/500,
                                                                       ifelse(Q05 == "1 in 100", 1/100,
                                                                              ifelse(Q05 == "1 in 50", 1/50,
                                                                                     ifelse(Q05 == "1 in 20", 1/20,
                                                                                            ifelse(Q05 == "1 in 10", 1/10,
                                                                                                   ifelse(Q05 == "1 in 5", 1/5,
                                                                                                          ifelse(Q05 == "1 in 1", 1, "")
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                              ),
                              
                              long_risk = ifelse(Q06 == "1 in 10,000", 1/10000,
                                                 ifelse(Q06 =="1 in 5,000", 1/5000, 
                                                        ifelse(Q06 == "1 in 1,000", 1/1000,
                                                               ifelse(Q06 == "1 in 500", 1/500,
                                                                      ifelse(Q06 == "1 in 100", 1/100,
                                                                             ifelse(Q06 == "1 in 50", 1/50,
                                                                                    ifelse(Q06 == "1 in 20", 1/20,
                                                                                           ifelse(Q06 == "1 in 10", 1/10,
                                                                                                  ifelse(Q06 == "1 in 5", 1/5,
                                                                                                         ifelse(Q06 == "1 in 1", 1, "")
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                              ),
                              
                              hurricane_awareness = ifelse(Q07 == "Not informed", 0,
                                                           ifelse(Q07 == "Very little", 1,
                                                                  ifelse(Q07 == "Somewhat informed", 2,
                                                                         ifelse(Q07 == "Moderately informed", 3,
                                                                                ifelse(Q07 == "Very well informed", 4, "")
                                                                         )
                                                                  )
                                                           )
                              ),
                              
                              fema_awareness = ifelse(Q08 == "Not informed", 0,
                                                      ifelse(Q08 == "Very little", 1,
                                                             ifelse(Q08 == "Somewhat informed", 2,
                                                                    ifelse(Q08 == "Moderately informed", 3,
                                                                           ifelse(Q08 == "Very well informed", 4, "")
                                                                    )
                                                             )
                                                      )
                              ),
                              
                              nfip_awareness = ifelse(Q09 == "Not informed", 0,
                                                      ifelse(Q09 == "Very little", 1,
                                                             ifelse(Q09 == "Somewhat informed", 2,
                                                                    ifelse(Q09 == "Moderately informed", 3,
                                                                           ifelse(Q09 == "Very well informed", 4, "")
                                                                    )
                                                             )
                                                      )
                              ),
                              
                              experience = ifelse(Q1 == "Yes", 1, 0),
                              
                              told = ifelse(Q2 == "Yes", 1, 0),
                              
                              evacuate = ifelse(Q3a == "Yes", 1, 0),
                              
                              damage = ifelse(Q4 == "Yes", 1, 0),
                              
                              loss = ifelse(Q5 == "$0 - $999", 1,
                                            ifelse(Q5 == "$1,000 - $4,999", 2,
                                                   ifelse(Q5 == "$5,000 - $9,999", 3,
                                                          ifelse(Q5 == ">$10,000", 4,
                                                                 ifelse(Q5 == "Total Loss", 5, "")
                                                          )
                                                   )
                                            )
                              ),
                              
                              displaced = ifelse(Q6  == "No", 0,
                                                 ifelse(Q6  == "Yes, for less than a week", 1,
                                                        ifelse(Q6  == "Yes, for between a week and a month", 2,
                                                               ifelse(Q6 == "Yes, for between 1 and 3 months", 3,
                                                                      ifelse(Q6 == "Yes, for between 3 and 6 months", 4,
                                                                             ifelse(Q6 == "Yes, for between 6 months and 1 year", 5,
                                                                                    ifelse(Q7 =="Yes, for more than one year", 6, "")
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                              ),
                              
                              influenced = ifelse(Q8 == "Yes", 1, 0),
                              
                              hh_size = as.numeric(Q11a), 
                              
                              hh_dependent = ifelse(is.numeric(Q11b) & !is.na(Q11b) > 0, 1, 0)
                              
                              
)

michael[michael == "I vote Yes"] <- "1"
michael[michael == "I vote No"] <- "0"

michael <- michael %>% 
  mutate(str12 = str_split(Q12_DO, "\\|"), 
         pos1 = unlist(str12)[seq(1, length(unlist(str12)), 4)],
         pos2 = unlist(str12)[seq(2, length(unlist(str12)), 4)],
         pos3 = unlist(str12)[seq(3, length(unlist(str12)), 4)],
         pos4 = unlist(str12)[seq(4, length(unlist(str12)), 4)],
         rain_att = ifelse(pos1 == "Rainfall", Q12_1, 
                           ifelse(pos2 == "Rainfall", Q12_2, 
                                  ifelse(pos3 == "Rainfall", Q12_3, Q12_4))),
         wind_att = ifelse(pos1 == "Wind speed", Q12_1,
                           ifelse(pos2 == "Wind Speed", Q12_2, 
                                  ifelse(pos3 == "Wind speed", Q12_3, Q12_4))), 
         surge_att = ifelse(pos1 == "Storm surge", Q12_1, 
                            ifelse(pos2 == "Storm surge", Q12_2, 
                                   ifelse(pos3 == "Storm surge", Q12_3, Q12_4))),
         track_att = ifelse(pos1 == "Track", Q12_1,
                            ifelse(pos2 == "Track", Q12_2, 
                                   ifelse(pos3 == "Track", Q12_3, Q12_4))))

michael <- michael %>% mutate(first = substr(FL_44_DO, 0, 5),
                              
                              second = substr(FL_44_DO, 7, 11),
                              
                              third = substr(FL_44_DO, 13, 17),
                              
                              track_order = ifelse(first == "FL_50", 1,
                                                   ifelse(second == "FL_50", 2, 3)),
                              
                              wind_order = ifelse(first == "FL_65", 1,
                                                  ifelse(second == "FL_65", 2, 3)),
                              
                              rain_order = ifelse(first == "FL_66", 1,
                                                  ifelse(second == "FL_66", 2, 3)),
                              
                              # Rates
                              
                              track_rate = ifelse(FL_50_DO == "Section3.Trackforecast:-20%", 1,
                                                  ifelse(FL_50_DO == "Section3.Trackforecast:StatusQuo", 2, 3)),
                              
                              wind_rate = ifelse(FL_65_DO == "Section4.Windspeedforecast:-20%", 1,
                                                 ifelse(FL_65_DO == "Section4.Windspeedforecast:StatusQuo", 2, 3)),
                              
                              rain_rate = ifelse(FL_66_DO == "Section5.Rainfallforecast:-20%", 1,
                                                 ifelse(FL_66_DO == "Section5.Rainfallforecast:StatusQuo", 2, 3)),
                              
                              # Bid 1
                              
                              track_bid1 = as.numeric(ifelse(track_rate == 1 , Track.m20,
                                                             ifelse(track_rate == 2 , Track.SQ, Track.p20))),
                              
                              wind_bid1 = as.numeric(ifelse(wind_rate == 1 , Wind.m20,
                                                            ifelse(wind_rate == 2 , Wind.SQ, Wind.p20))),
                              
                              rain_bid1 = as.numeric(ifelse(rain_rate == 1 , Rain.m20,
                                                            ifelse(rain_rate == 2 , Rain.SQ, Rain.SQ))),
                              
                              # Answer 1
                              
                              track_answer1 =ifelse(track_rate == 1, Q13...Track.m20,
                                                    ifelse(track_rate == 2, Q13...Track.SQ, Q13...Track.p20)),
                              
                              wind_answer1 =ifelse(wind_rate == 1, Q15...Wind.m20,
                                                   ifelse(wind_rate == 2, Q15...Wind.SQ, Q15...Wind.p20)),
                              
                              rain_answer1 =ifelse(rain_rate == 1, Q17...Rain.m20,
                                                   ifelse(rain_rate == 2, Q17...Rain.SQ, Q17...Rain.p20)),
                              
                              # Bid 2
                              
                              track_bid2 = ifelse(track_answer1 == 1, track_bid1*1.2,
                                                  ifelse(track_answer1 == 0 , track_bid1*0.8, "")),
                              
                              wind_bid2 = ifelse(wind_answer1 == 1, wind_bid1*1.2,
                                                 ifelse(wind_answer1 == 0, wind_bid1*0.8, "")),
                              
                              rain_bid2 = ifelse(rain_answer1 == 1, rain_bid1*1.2,
                                                 ifelse(rain_answer1 == 0, rain_bid1*0.8, "")),
                              
                              # Answer 2 
                              
                              track_answer2 =ifelse(track_rate == 1 & track_answer1 == 1, Q14a...Track.m20,
                                                    ifelse(track_rate == 1 & track_answer1 == 0, Q14b...Track.m20,
                                                           
                                                           ifelse(track_rate == 2 & track_answer1 == 1, Q14a...Track.SQ,
                                                                  ifelse(track_rate == 2 & track_answer1 == 0, Q14b...Track.SQ,
                                                                         
                                                                         ifelse(track_rate == 3 & track_answer1 == 1, Q14a...Track.p20,
                                                                                ifelse(track_rate == 3 & track_answer1 == 0, Q14b...Track.p20, "")
                                                                         )
                                                                  )
                                                           )
                                                    )
                              ),
                              
                              wind_answer2 =ifelse(wind_rate == 1 & wind_answer1 == 1, Q16a...Wind.m20,
                                                   ifelse(wind_rate == 1 & wind_answer1 == 0, Q16b...Wind.m20,
                                                          
                                                          ifelse(wind_rate == 2 & wind_answer1 == 1, Q16a...Wind.SQ,
                                                                 ifelse(wind_rate == 2 & wind_answer1 == 0, Q16b...Wind.SQ,
                                                                        
                                                                        ifelse(wind_rate == 3 & wind_answer1 == 1, Q16a...Wind.p20,
                                                                               ifelse(wind_rate == 3 & wind_answer1 == 0, Q16b...Wind.p20, "")
                                                                        )
                                                                 )
                                                          )
                                                   )
                              ),
                              
                              
                              rain_answer2 =ifelse(rain_rate == 1 & rain_answer1 == 1, Q18a...Rain.m20,
                                                   ifelse(rain_rate == 1 & rain_answer1 == 0, Q18b...Rain.m20,
                                                          
                                                          ifelse(rain_rate == 2 & rain_answer1 == 1, Q18a...Rain.SQ,
                                                                 ifelse(rain_rate == 2 & rain_answer1 == 0, Q18b...Rain.SQ,
                                                                        
                                                                        ifelse(rain_rate == 3 & rain_answer1 == 1, Q18a...Rain.p20,
                                                                               ifelse(rain_rate == 3 & rain_answer1 == 0, Q18b...Rain.p20, "")
                                                                        )
                                                                 )
                                                          )
                                                   )
                              )
                              
) %>%
  mutate(voice = ifelse(Q20 == "Very likely", 5, 
                        ifelse(Q20 == "Likely", 4,
                               ifelse(Q20 == "Somewhat likely", 3,
                                      ifelse(Q20 == "Moderately likely", 2,
                                             ifelse(Q20 == "Not likely", 1, ""))))),
         action = ifelse(Q21 == "Very likely", 5, 
                         ifelse(Q21 == "Likely", 4, 
                                ifelse(Q21 == "Somewhat likely", 3, 
                                       ifelse(Q21 == "Moderately likely", 2,
                                              ifelse(Q21 == "Not likely", 1, ""))))))

# select relevant variables 
data_michael <- michael %>% select(hurricane, zip, age, female, owner, tenure, insured, short_risk, long_risk,
                                   hurricane_awareness, fema_awareness, nfip_awareness, experience, told,
                                   evacuate, damage, loss, displaced, influenced, hh_size, hh_dependent,
                                   rain_att, wind_att, surge_att, track_att,
                                   track_order, wind_order, rain_order, track_rate, wind_rate, rain_rate,
                                   track_bid1, wind_bid1, rain_bid1, track_answer1, wind_answer1, rain_answer1,
                                   track_bid2, wind_bid2, rain_bid2, track_answer2, wind_answer2, rain_answer2, 
                                   voice, action)

# Florence

#Load data
florence <- read.csv("./InputData/HurricaneFlorence_FinalData.csv", comment.char="#",
                     stringsAsFactors = FALSE)

# Get rid of extra rows
florence <- florence[-1,]
florence <- florence[-1,]

# Convert time living in house in to net years

florence$Q02_1_TEXT <- as.numeric(florence$Q02_1_TEXT)
florence$Q02_1_TEXT[is.na(florence$Q02_1_TEXT)] <- 0

florence$Q02_2_TEXT <- as.numeric(florence$Q02_2_TEXT)
florence$Q02_2_TEXT[is.na(florence$Q02_2_TEXT)] <- 0

# Process controls

florence <- florence %>% mutate(hurricane = "florence",
                                
                                zip = Q75,
                                
                                age = Q76,
                                
                                female = ifelse(Q77 == "Female", 1, 0),
                                
                                owner = ifelse(Q01 == "Own", 1, 0),
                                
                                tenure = Q02_1_TEXT/12 + Q02_2_TEXT, # how many years have you lived in your house
                                
                                insured = ifelse(Q03a != "", 1, 0), # have insurance
                                
                                short_risk = ifelse(Q05 == "1 in 10,000", 1/10000,
                                                    ifelse(Q05 =="1 in 5,000", 1/5000, 
                                                           ifelse(Q05 == "1 in 1,000", 1/1000,
                                                                  ifelse(Q05 == "1 in 500", 1/500,
                                                                         ifelse(Q05 == "1 in 100", 1/100,
                                                                                ifelse(Q05 == "1 in 50", 1/50,
                                                                                       ifelse(Q05 == "1 in 20", 1/20,
                                                                                              ifelse(Q05 == "1 in 10", 1/10,
                                                                                                     ifelse(Q05 == "1 in 5", 1/5,
                                                                                                            ifelse(Q05 == "1 in 1", 1, "")
                                                                                                     )
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                ),
                                
                                long_risk = ifelse(Q06 == "1 in 10,000", 1/10000,
                                                   ifelse(Q06 =="1 in 5,000", 1/5000, 
                                                          ifelse(Q06 == "1 in 1,000", 1/1000,
                                                                 ifelse(Q06 == "1 in 500", 1/500,
                                                                        ifelse(Q06 == "1 in 100", 1/100,
                                                                               ifelse(Q06 == "1 in 50", 1/50,
                                                                                      ifelse(Q06 == "1 in 20", 1/20,
                                                                                             ifelse(Q06 == "1 in 10", 1/10,
                                                                                                    ifelse(Q06 == "1 in 5", 1/5,
                                                                                                           ifelse(Q06 == "1 in 1", 1, "")
                                                                                                    )
                                                                                             )
                                                                                      )
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                ),
                                
                                hurricane_awareness = ifelse(Q07 == "Not informed", 0,
                                                             ifelse(Q07 == "Very little", 1,
                                                                    ifelse(Q07 == "Somewhat informed", 2,
                                                                           ifelse(Q07 == "Moderately informed", 3,
                                                                                  ifelse(Q07 == "Very well informed", 4, "")
                                                                           )
                                                                    )
                                                             )
                                ),
                                
                                fema_awareness = ifelse(Q08 == "Not informed", 0,
                                                        ifelse(Q08 == "Very little", 1,
                                                               ifelse(Q08 == "Somewhat informed", 2,
                                                                      ifelse(Q08 == "Moderately informed", 3,
                                                                             ifelse(Q08 == "Very well informed", 4, "")
                                                                      )
                                                               )
                                                        )
                                ),
                                
                                nfip_awareness = ifelse(Q09 == "Not informed", 0,
                                                        ifelse(Q09 == "Very little", 1,
                                                               ifelse(Q09 == "Somewhat informed", 2,
                                                                      ifelse(Q09 == "Moderately informed", 3,
                                                                             ifelse(Q09 == "Very well informed", 4, "")
                                                                      )
                                                               )
                                                        )
                                ),
                                
                                
                                experience = ifelse(Q1 == "Yes", 1, 0),
                                
                                told = ifelse(Q2 == "Yes", 1, 0),
                                
                                evacuate = ifelse(Q3a == "Yes", 1, 0),
                                
                                damage = ifelse(Q4 == "Yes", 1, 0),
                                
                                loss = ifelse(Q5 == "$0 - $999", 1,
                                              ifelse(Q5 == "$1,000 - $4,999", 2,
                                                     ifelse(Q5 == "$5,000 - $9,999", 3,
                                                            ifelse(Q5 == ">$10,000", 4,
                                                                   ifelse(Q5 == "Total Loss", 5, "")
                                                            )
                                                     )
                                              )
                                ),
                                
                                displaced = ifelse(Q6  == "No", 0,
                                                   ifelse(Q6  == "Yes, for less than a week", 1,
                                                          ifelse(Q6  == "Yes, for between a week and a month", 2,
                                                                 ifelse(Q6 == "Yes, for between 1 and 3 months", 3,
                                                                        ifelse(Q6 == "Yes, for between 3 and 6 months", 4,
                                                                               ifelse(Q6 == "Yes, for between 6 months and 1 year", 5,
                                                                                      ifelse(Q7 =="Yes, for more than one year", 6, "")
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                ),
                                
                                influenced = ifelse(Q8 == "Yes", 1, 0),
                                
                                hh_size = as.numeric(Q11a), 
                                
                                hh_dependent = ifelse(is.numeric(Q11b) & !is.na(Q11b) > 0, 1, 0)
                                
                                
)

florence[florence == "I vote Yes"] <- "1"
florence[florence == "I vote No"] <- "0"

florence = florence %>%
  mutate(str12 = str_split(Q12_DO, "\\|"), 
         pos1 = unlist(str12)[seq(1, length(unlist(str12)), 4)],
         pos2 = unlist(str12)[seq(2, length(unlist(str12)), 4)],
         pos3 = unlist(str12)[seq(3, length(unlist(str12)), 4)],
         pos4 = unlist(str12)[seq(4, length(unlist(str12)), 4)],
         rain_att = ifelse(pos1 == "Rainfall", Q12_1, 
                           ifelse(pos2 == "Rainfall", Q12_2, 
                                  ifelse(pos3 == "Rainfall", Q12_3, Q12_4))),
         wind_att = ifelse(pos1 == "Wind speed", Q12_1,
                           ifelse(pos2 == "Wind Speed", Q12_2, 
                                  ifelse(pos3 == "Wind speed", Q12_3, Q12_4))), 
         surge_att = ifelse(pos1 == "Storm surge", Q12_1, 
                            ifelse(pos2 == "Storm surge", Q12_2, 
                                   ifelse(pos3 == "Storm surge", Q12_3, Q12_4))),
         track_att = ifelse(pos1 == "Track", Q12_1,
                            ifelse(pos2 == "Track", Q12_2, 
                                   ifelse(pos3 == "Track", Q12_3, Q12_4))))

florence <- florence %>% mutate(first = substr(FL_44_DO, 0, 5),
                                
                                second = substr(FL_44_DO, 7, 11),
                                
                                third = substr(FL_44_DO, 13, 17),
                                
                                track_order = ifelse(first == "FL_50", 1,
                                                     ifelse(second == "FL_50", 2, 3)),
                                
                                wind_order = ifelse(first == "FL_65", 1,
                                                    ifelse(second == "FL_65", 2, 3)),
                                
                                rain_order = ifelse(first == "FL_66", 1,
                                                    ifelse(second == "FL_66", 2, 3)),
                                
                                # Rates
                                
                                track_rate = ifelse(FL_50_DO == "Section3.Trackforecast:-20%", 1,
                                                    ifelse(FL_50_DO == "Section3.Trackforecast:StatusQuo", 2, 3)),
                                
                                wind_rate = ifelse(FL_65_DO == "Section4.Windspeedforecast:-20%", 1,
                                                   ifelse(FL_65_DO == "Section4.Windspeedforecast:StatusQuo", 2, 3)),
                                
                                rain_rate = ifelse(FL_66_DO == "Section5.Rainfallforecast:-20%", 1,
                                                   ifelse(FL_66_DO == "Section5.Rainfallforecast:StatusQuo", 2, 3)),
                                
                                # Bid 1
                                
                                track_bid1 = as.numeric(ifelse(track_rate == 1 , Track.m20,
                                                               ifelse(track_rate == 2 , Track.SQ, Track.p20))),
                                
                                wind_bid1 = as.numeric(ifelse(wind_rate == 1 , Wind.m20,
                                                              ifelse(wind_rate == 2 , Wind.SQ, Wind.p20))),
                                
                                rain_bid1 = as.numeric(ifelse(rain_rate == 1 , Rain.m20,
                                                              ifelse(rain_rate == 2 , Rain.SQ, Rain.SQ))),
                                
                                # Answer 1
                                
                                track_answer1 =ifelse(track_rate == 1, Q13...Track.m20,
                                                      ifelse(track_rate == 2, Q13...Track.SQ, Q13...Track.p20)),
                                
                                wind_answer1 =ifelse(wind_rate == 1, Q15...Wind.m20,
                                                     ifelse(wind_rate == 2, Q15...Wind.SQ, Q15...Wind.p20)),
                                
                                rain_answer1 =ifelse(rain_rate == 1, Q17...Rain.m20,
                                                     ifelse(rain_rate == 2, Q17...Rain.SQ, Q17...Rain.p20)),
                                
                                # Bid 2
                                
                                track_bid2 = ifelse(track_answer1 == 1, track_bid1*1.2,
                                                    ifelse(track_answer1 == 0 , track_bid1*0.8, "")),
                                
                                wind_bid2 = ifelse(wind_answer1 == 1, wind_bid1*1.2,
                                                   ifelse(wind_answer1 == 0, wind_bid1*0.8, "")),
                                
                                rain_bid2 = ifelse(rain_answer1 == 1, rain_bid1*1.2,
                                                   ifelse(rain_answer1 == 0, rain_bid1*0.8, "")),
                                
                                # Answer 2 
                                
                                track_answer2 =ifelse(track_rate == 1 & track_answer1 == 1, Q14a...Track.m20,
                                                      ifelse(track_rate == 1 & track_answer1 == 0, Q14b...Track.m20,
                                                             
                                                             ifelse(track_rate == 2 & track_answer1 == 1, Q14a...Track.SQ,
                                                                    ifelse(track_rate == 2 & track_answer1 == 0, Q14b...Track.SQ,
                                                                           
                                                                           ifelse(track_rate == 3 & track_answer1 == 1, Q14a...Track.p20,
                                                                                  ifelse(track_rate == 3 & track_answer1 == 0, Q14b...Track.p20, "")
                                                                           )
                                                                    )
                                                             )
                                                      )
                                ),
                                
                                wind_answer2 =ifelse(wind_rate == 1 & wind_answer1 == 1, Q16a...Wind.m20,
                                                     ifelse(wind_rate == 1 & wind_answer1 == 0, Q16b...Wind.m20,
                                                            
                                                            ifelse(wind_rate == 2 & wind_answer1 == 1, Q16a...Wind.SQ,
                                                                   ifelse(wind_rate == 2 & wind_answer1 == 0, Q16b...Wind.SQ,
                                                                          
                                                                          ifelse(wind_rate == 3 & wind_answer1 == 1, Q16a...Wind.p20,
                                                                                 ifelse(wind_rate == 3 & wind_answer1 == 0, Q16b...Wind.p20, "")
                                                                          )
                                                                   )
                                                            )
                                                     )
                                ),
                                
                                
                                rain_answer2 =ifelse(rain_rate == 1 & rain_answer1 == 1, Q18a...Rain.m20,
                                                     ifelse(rain_rate == 1 & rain_answer1 == 0, Q18b...Rain.m20,
                                                            
                                                            ifelse(rain_rate == 2 & rain_answer1 == 1, Q18a...Rain.SQ,
                                                                   ifelse(rain_rate == 2 & rain_answer1 == 0, Q18b...Rain.SQ,
                                                                          
                                                                          ifelse(rain_rate == 3 & rain_answer1 == 1, Q18a...Rain.p20,
                                                                                 ifelse(rain_rate == 3 & rain_answer1 == 0, Q18b...Rain.p20, "")
                                                                          )
                                                                   )
                                                            )
                                                     )
                                )
                                
) %>%
  mutate(voice = ifelse(Q20 == "Very likely", 5, 
                        ifelse(Q20 == "Likely", 4,
                               ifelse(Q20 == "Somewhat likely", 3,
                                      ifelse(Q20 == "Moderately likely", 2,
                                             ifelse(Q20 == "Not likely", 1, ""))))),
         action = ifelse(Q21 == "Very likely", 5, 
                         ifelse(Q21 == "Likely", 4, 
                                ifelse(Q21 == "Somewhat likely", 3, 
                                       ifelse(Q21 == "Moderately likely", 2,
                                              ifelse(Q21 == "Not likely", 1, ""))))))


# select relevant variables 
data_florence <- florence %>% select(hurricane, zip, age, female, owner, tenure, insured, short_risk, long_risk,
                                     hurricane_awareness, fema_awareness, nfip_awareness, experience, told,
                                     evacuate, damage, loss, displaced, influenced, hh_size, hh_dependent,
                                     rain_att, wind_att, surge_att, track_att,
                                     track_order, wind_order, rain_order, track_rate, wind_rate, rain_rate,
                                     track_bid1, wind_bid1, rain_bid1, track_answer1, wind_answer1, rain_answer1,
                                     track_bid2, wind_bid2, rain_bid2, track_answer2, wind_answer2, rain_answer2,
                                     voice, action)



# Stack the two datasets

data <- rbind(data_florence,data_michael) %>% mutate(florence = ifelse(hurricane == "florence",1,0))
data = data %>%
  mutate(index = row_number())

##matt's write out(i stop here EVERYTIME)
data.table::fwrite(data, "./OutputData/Survey_Data_Cleaned.csv")

#### ZCTA_Control Data Set UP ####

rm(list = ls())


data = data.table::fread("./InputData/ACSDP5Y2018/income_sex_byzip.csv")

data.df1 = data[-1,]

data.df = data.df1 %>%
  select(1, 3, 7, 11, 15, 19, 20) %>%
  rename(geoid = GEO_ID, 
         tot.pop = DP05_0021E,
         male.pop = DP05_0026E,
         fem.pop = DP05_0027E,
         med.inc = DP03_0062E,
         mean.inc = DP03_0063E,
         mean.inc.moe = DP03_0063M) %>%
  mutate(zcta = str_sub(geoid, 10, nchar(geoid)),
         tot.pop = as.numeric(tot.pop),
         male.pop = as.numeric(male.pop),
         fem.pop = as.numeric(fem.pop), 
         med.inc = as.numeric(med.inc),
         mean.inc = as.numeric(mean.inc),
         mean.inc.moe = as.numeric(mean.inc.moe))

data.table::fwrite(data.df, "./OututData/ZCTA_AvgData.csv")

####Spatial Set up (ZCTAs) for Survey and WindSwath Data sets####
rm(list = ls())

library(sf)
library(maps)

##epsg:102003 is USA Contiguous Albers Equal Area Conic

##data 

##state fips codes 
data(state.fips)

##original windswath speeds by county from RENATO
wind.swaths = data.table::fread("./InputData/Windswath_Original.csv") %>%
  mutate(index = row_number())

##original survey data from Hurricanes Florence and Michael form Qualtrics 
survey = data.table::fread("./OutputData/Survey_Data_Cleaned.csv")

##counties shapefile from 2019 county lines
counties = sf::st_read("./InputData/tl_2019_us_county/tl_2019_us_county.shp", 
                       stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  st_transform(102003) %>%
  select(statefp = STATEFP,
         countyfp = COUNTYFP, 
         name = NAMELSAD, 
         geometry) %>%
  mutate(county.name = str_to_lower(name, locale = "en")) %>%
  select(-name)

##zcta tracks from 2019 zcta definitions --> zcta with geometries 
zcta = st_read("./InputData/tl_2019_us_zcta510/tl_2019_us_zcta510.shp", 
               stringsAsFactors = F) %>%
  select(zcta = ZCTA5CE10,
         geometry) %>%
  st_transform(102003)

##zcta control attributes from ACSDP5Y2018 --> income and population 
zcta.atts = data.table::fread("./OutputData/ZCTA_AvgData.csv", 
                              colClasses = c(zcta = "character"))

## Joining WindSwath to StateFIPS for FIPS id and State Abbreviations 
wind.swaths.st.fips = left_join(wind.swaths, 
                                state.fips %>%
                                  select(fips, abb, polyname), 
                                by = c("state" = "polyname")) %>%
  mutate(abb = replace(abb, which(state == "virginia"), "VA"),
         fips = replace(fips, which(state == "virginia"), 51),
         abb = replace(abb, which(state == "massachusetts"), "MA"),
         fips = replace(fips, which(state == "massachusetts"), 25),
         abb = replace(abb, which(state == "north carolina"), "NC"),
         fips = replace(fips, which(state == "north carolina"), 37),
         abb = replace(abb, which(state == "new york"), "NY"),
         fips = replace(fips, which(state == "new york"), 36)) %>%
  mutate(unique.name = paste(state, county, sep = " "))

##Adding State Names to Counties Data Set 
counties.state = inner_join(counties, 
                            wind.swaths.st.fips %>%
                              select(state, fips) %>%
                              distinct(fips, .keep_all = T),
                            by = c("statefp" = "fips")) %>%
  select(statefp, state, countyfp, county.name, geometry)

##Removing "county" and "parish" in county.name (for joining with windswath) 
RemoveWords = function(str, badword){
  x = unlist(strsplit(str, " "))
  paste(x[!x %in% badword], collapse = " ")
}

counties.remv.county = apply(data.frame(counties.state$county.name), 1, RemoveWords, badword = "county")
counties.remv.parish = sapply(counties.remv.county, RemoveWords, badword = "parish")
counties.clean.name = cbind(counties.state, counties.remv.parish) %>%
  select(statefp, state, countyfp, county = counties.remv.parish, geometry) %>%
  mutate(statefp = as.character(statefp),
         county = as.character(county))

##Fix the County Names that don't match with WindSwath Data Set (for joining) 
counties.for.join = counties.clean.name %>%
  mutate(county = replace(county, which(state == "missouri" & county == "ste. genevieve"), "ste genevieve"),
         county = replace(county, which(state == "maryland" & county == "st. mary's"), "st. marys"), 
         county = replace(county, which(state == "maryland" & county == "queen anne's"), "queen annes"),
         county = replace(county, which(state == "maryland" & county == "prince george's"), "prince georges"),
         county = replace(county, which(state == "virginia" & county == "newport news city"), "newport news"), 
         county = replace(county, which((state == "tennessee" | state == "alabama" | state == "georgia") &
                                          county == "dekalb"), "de kalb"),
         county = replace(county, which(state == "virginia" & county == "virginia beach city"), "virginia beach"),
         county = replace(county, which(state == "florida" & county == "desoto"), "de soto"),
         county = replace(county, which(state == "mississippi" & county == "desoto"), "de soto"),
         county = replace(county, which(state == "virginia" & county == "suffolk city"), "suffolk"),
         county = replace(county, which(state == "virginia" & county == "norfolk city"), "norfolk"),
         county = replace(county, which(state == "virginia" & county == "hampton city"), "hampton"),
         county = replace(county, which(state == "district of columbia" & county == "district of columbia"), "washington"),
         county = replace(county, which(state == "texas" & county == "dewitt"), "de witt"),
         county = replace(county, which(state == "louisiana" & county == "lasalle"), "la salle")) %>%
  mutate(unique.name = paste(state, county, sep = " "))

##Convert Survey Zips to ZCTAs: Add ZCTA Geometries and ZCTA Socioeconomic Attributes 

##zips that didn't have 1 to 1 zcta 
bad.zips = setdiff(survey$zip, zcta$zcta) 
##serached on https://www.udsmapper.org/zcta-crosswalk.cfm
rep.zcta = c(28409, 27330, 29501, 27705, 27408, 28504, 28540, 28112, 27546, 27705, 28401, 27403, 
             27707, 28387, 27403, 29577, 29526, 28411, 28374, 27252, 28540, 31701, 32305, 32304,
             39817, 32304, 32401, 32310, 31705, 32405, 31021, 32327, 31015, 32303, 31701)

survey.zcta = survey %>%
  mutate(zcta = zip)
##replace zips with zctas that don't match 1 to 1 
survey.zcta$zcta = rep.zcta[match(survey.zcta$zcta, bad.zips)]

##fill in ZCTA NAs with zips that match 1 to 1 with zctas. 
survey.fixed.zcta = survey.zcta %>%
  mutate(zcta = ifelse(is.na(zcta), zip, zcta),
         zcta = as.character(zcta))

##add zcta geometries to survey data set 
survey.zcta.sf = inner_join(survey.fixed.zcta,
                            zcta, 
                            by = "zcta") %>%
  st_as_sf(crs = 102003)

##attach ZCTA attributes to Survey Data Set 
survey.atts = inner_join(survey.zcta.sf, 
                         zcta.atts, 
                         by = c("zcta"))

##attach state and county names to Survey Data Set (keeps ZCTA geometries) 
survey.full = st_join(survey.atts, 
                      counties.for.join, 
                      left = F, ##drop ZCTAs not within any counties 
                      join = st_within, ##join by ZCTAs within counties 
                      largest = T)


st_write(survey.full, "./OutputData/SurveyData_ZCTAttribs.gpkg")
data.table::fwrite(st_drop_geometry(survey.full), "./OutputData/SurveyData_ZCTAttribs.csv")

##Add County FIP and County Geometry to WindSwaths Datat Set 
wind.sf = inner_join(wind.swaths.st.fips, 
                     counties.for.join %>%
                       select(countyfp, unique.name, geometry), 
                     by = "unique.name") %>%
  select(statefp = fips, countyfp, state, abb, county, unique.name,
         hurricane, bt_speed_max, bt_speed_min, bt_speed_mean, bt_speed_median, geometry) %>%
  st_as_sf(sf_column_name = "geometry")

##write out 
st_write(wind.sf, "./OutputData/Windswath_ST_CNTY.gpkg")


##Create Data Set of WindSwaths with ZCTA number and geometry 
##Break apart by hurricane since, some counties experience multiple hurricanes and don't want those thrown out
michael.wind = wind.sf %>%
  filter(hurricane == "michael")
michael.zcta = st_join(zcta, 
                       michael.wind, 
                       join = st_within, 
                       largest = T,
                       left = F)

florence.wind = wind.sf %>%
  filter(hurricane == "florence")
##this does something bad with zips to counties; puts 36804 in russel,al but it is lee,al
florence.zcta = st_join(zcta, 
                        florence.wind, 
                        join = st_within,
                        largest = T, 
                        left = F) 

irma.wind = wind.sf %>%
  filter(hurricane == "irma") 
irma.zcta = st_join(zcta, 
                    irma.wind, 
                    join = st_within, 
                    largest = T,
                    left = F)

harvey.wind = wind.sf %>%
  filter(hurricane == "harvey")
harvey.zcta = st_join(zcta, 
                      harvey.wind,
                      join = st_within,
                      largest = T,
                      left = F)

ike.wind = wind.sf %>%
  filter(hurricane == "ike")
ike.zcta = st_join(zcta,
                   ike.wind,
                   join = st_within, 
                   largest = T,
                   left = F)

##stack zips by hurricane 
wind.hurricane = rbind(michael.zcta, florence.zcta, irma.zcta, harvey.zcta, ike.zcta)

wind.hurricane = inner_join(wind.hurricane, 
                            zcta.atts, 
                            by = c("zcta" = "zips"))


st_write(wind.hurricane, "./OutputData/Windswath_ByHurricane_ZCTA.gpkg")
data.table::fwrite(st_drop_geometry(wind.hurricane), "./OutputData/Windswath_ByHurricane_ZCTA.csv")

#### Housing Data Set Up####
rm(list = ls())

##data 
survey = data.table::fread("./OutputData/SurveyData_ZCTAttribs.csv", 
                           stringsAsFactors = F,
                           colClasses = c(zcta = "character"))

##windswath data 
wind = data.table::fread("./OutputData/Windswath_ByHurricane_ZCTA.csv", 
                         stringsAsFactors = F,
                         colClasses = c(zcta = "character")) %>%
  mutate(index = row_number()) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""))

##housing data from 2013-2017 5 year ACS 
house = data.table::fread("./InputData/ACS_17_5YR_DP04/ACS_17_5YR_DP04_with_ann.csv",
                          colClasses = c(GEO.id2 = "character"))

##selecting variables of interest from housing data raw 
house.use = house %>%
  select(c(2,4,5,8,9,12,13,184,185,188,189,232,233))
house.use = house.use[-1,]
names(house.use) = c("zcta", "tot.units", "err.tot.units", "occ.units", "err.occ.units",
                     "vac.units", "err.vac.units", "owner.units", "err.owner.units", 
                     "renter.units", "err.renter.units", "no.veh", "err.no.veh")

##zcta attributes from prior ZCTA_Controls script 
zcta.atts = data.table::fread("./InputData/ZCTA_AvgData.csv", 
                              colClasses = c(zcta = "character", med.inc = "integer"))
##zcta geospatial attributes 
zcta = st_read("./InputData/tl_2019_us_zcta510/tl_2019_us_zcta510.shp",
               stringsAsFactors = F) %>%
  st_transform(102003)
##county geospatial attributes 
county = st_read("./InputData/tl_2019_us_county/tl_2019_us_county.shp",
                 stringsAsFactors = F) %>%
  st_transform(102003)

##house estimates by county

##adding zcta spatial reference to house.use
zcta.house = left_join(house.use, 
                       zcta %>%
                         select(ZCTA5CE10, geometry), 
                       by = c("zcta" = "ZCTA5CE10")) %>%
  st_as_sf() %>%
  st_transform(102003)

##adding county specific info to zcta.house data frame for aggregation at county level 
zcta.county = st_join(zcta.house,
                      county %>%
                        select(GEOID, geometry),
                      join = st_within, 
                      largest = T, 
                      left = F)

##coercing occ.units a number
zcta.county$occ.units = as.numeric(zcta.county$occ.units)

##aggregating total occupied units by county 
county.house.total = data.table::setDT(zcta.county)[, county.occ := sum(occ.units), by = GEOID] %>%
  select(zcta, GEOID, county.occ) %>%
  group_by(GEOID) %>%
  slice(1) 

data.table::fwrite(county.house.total, "./OutputData/CountyHousingTotal.csv")  
##population estimates by county 

##adding spatial attributes to zcta 
zcta.atts = left_join(zcta.atts, 
                      zcta %>%
                        select(ZCTA5CE10, geometry), 
                      by = c("zcta" = "ZCTA5CE10")) %>%
  st_as_sf() %>%
  st_transform(102003)

##adding county specific info to zcta.atts data frame for aggregation at county level 
zcta.att.county = st_join(zcta.atts,
                          county %>%
                            select(GEOID, geometry),
                          join = st_within, 
                          largest = T, 
                          left = F)


##aggregating population by county from zcta measures 
zcta.att.total = data.table::setDT(zcta.att.county)[, county.pop := sum(tot.pop), by = GEOID] %>%
  select(zcta, GEOID, county.pop) %>%
  group_by(GEOID) %>%
  slice(1) 

data.table::fwrite(zcta.att.total, "./OutputData/CountyPopTotal.csv")

##house estimates survey 
survey.house = left_join(survey, 
                         house.use,
                         by = "zcta")
data.table::fwrite(survey.house, "./OutputData/SurveyHouse.csv")

##house estimates wind 
wind.house = left_join(wind, 
                       house.use,
                       by = "zcta")
data.table::fwrite(wind.house, "./OutputData/WindswathHouse.csv")

####Sheldus Data Set Up####
rm(list = ls())
##data 
sheldus.a = data.table::fread("./InputData/sheldus_2018/UID3882f_AGG_A.csv") %>%
  mutate(`County FIPS` = str_sub(`County FIPS`, 2, 6))
sheldus.b = data.table::fread("./InputData/sheldus_2018/UID3882f_AGG_B.csv") %>%
  mutate(`County FIPS` = str_sub(`County FIPS`, 2, 6))



survey = data.table::fread("./OutputData/SurveyData_ZCTAttribs.csv",
                           colClasses = c(zcta = "character", countyfp = "character", statefp = "character"),
                           stringsAsFactors = F) %>%
  mutate(fips = paste(statefp, countyfp, sep = ""))

wind = data.table::fread("./OutputData/Windswath_ByHurricane_ZCTA.csv",
                         stringsAsFactors = F,
                         colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(fips = paste(statefp, countyfp, sep = ""),
         index = row_number())


## sheldus data 
michael.sheldus = sheldus.a %>%
  filter(Year == 2018 & Month == 10 & Hazard == "Hurricane/Tropical Storm")
florence.sheldus = sheldus.a %>%
  filter(Year == 2018 & Month == 9 & Hazard == "Hurricane/Tropical Storm")
ike.sheldus = sheldus.a %>%
  filter(Year == 2008 & Month == 9 & Hazard == "Hurricane/Tropical Storm")
ike.sheldusb = sheldus.b %>%
  filter(Year == 2008 & Month == 10 & Hazard == "Hurricane/Tropical Storm")
harvey.sheldus = sheldus.a %>%
  filter(Year == 2017 & Month == 8 & Hazard == "Hurricane/Tropical Storm")
ir.har.sheldus.sep = sheldus.a %>%
  filter(Year == 2017 & Month == 9 & Hazard == "Hurricane/Tropical Storm")

##add sheldus to survey 
survey.michael = left_join(survey %>%
                             filter(hurricane == "michael"),
                           michael.sheldus, 
                           by = c("fips" = "County FIPS"))

survey.florence = left_join(survey %>%
                              filter(hurricane == "florence"),
                            florence.sheldus,
                            by = c("fips" = "County FIPS"))
survey.sheldus = rbind(survey.michael, survey.florence)
data.table::fwrite(survey.sheldus, "./OutputData/SurveySheldus.csv")


##add sheldus to windswaths 
wind.michael = left_join(wind %>%
                           filter(hurricane == "michael"),
                         michael.sheldus,
                         by = c("fips" = "County FIPS"))
wind.florence = left_join(wind %>%
                            filter(hurricane == "florence"),
                          florence.sheldus, 
                          by = c("fips" = "County FIPS"))
wind.ike = left_join(wind %>%
                       filter(hurricane == "ike"),
                     ike.sheldus,
                     by = c("fips" = "County FIPS"))
# wind.ike2 = left_join(wind %>%
#                         filter(hurricane == "ike"),
#                      ike.sheldusb,
#                      by = c("fips" = "County FIPS"))
wind.harvey = inner_join(wind %>%
                           filter(hurricane == "harvey"),
                         harvey.sheldus,
                         by = c("fips" = "County FIPS"))

wind.harvirv = left_join(wind %>%
                           filter(hurricane == "harvey" | hurricane == "irma"),
                         ir.har.sheldus.sep,
                         by = c("fips" = "County FIPS")) %>%
  filter(!index %in% unique(wind.harvey$index))



wind.out = rbind(wind.florence, wind.harvey, wind.harvirv, wind.ike, wind.michael) %>%
  select(-c(`State Name`, `County Name`))

data.table::fwrite(wind.out, "./OutputData/WindSwath_Sheldus.csv")

####Distance to Shore and Hurricane Track####
##data 

##zcta tracks from 2019 zcta definitions --> zcta with geometries 
zcta = st_read("./InputData/tl_2019_us_zcta510/tl_2019_us_zcta510.shp", 
               stringsAsFactors = F) %>%
  select(zcta = ZCTA5CE10,
         geometry) %>%
  st_transform(102003)

##states polygons
states = st_read("./InputData/tl_2019_us_state/tl_2019_us_state.shp") %>%
  mutate(NAME = str_to_lower(NAME)) %>%
  st_transform(102003)

##survey 
survey = data.table::fread("./OutputData/SurveyHouse.csv",
                           colClasses = c(statefp = "character", countyfp = "character", zcta = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""))

##shoreline line feature (turned from polygon feature to lines in arcgis) 
shoreline = st_read("./InputData/us_coastline_linefeat/nam_coast_lines.shp") %>%
  st_transform(102003)

##hurricane tracks (as line segments) 
flo_track = st_read("./InputData/Florence_Hurricane/AL062018_lin.shp") %>%
  st_transform(102003)
mich_track = st_read("./InputData/Michael_TRACK/AL142018_lin.shp") %>%
  st_transform(102003)

##pulling ZCTAs from survey to find distance 
zcta_dist = zcta %>%
  filter(zcta %in% survey$zcta)

##creating centroid point for each ZCTA 
zcta_cent = st_centroid(zcta_dist)

##calculating distance to shoreline 
zcta.shore.dist = apply(st_distance(zcta_cent, shoreline), 1, min)

##attaching distance calculations to ZCTAs
zcta.shore = cbind(zcta_dist, zcta.shore.dist) %>%
  data.frame() %>%
  rename(dist.shore = zcta.shore.dist)


##pulling ZCTAs from survey for Florence 
surv_flo = survey %>%
  filter(hurricane == "florence")
zcta_flo = zcta %>%
  filter(zcta %in% surv_flo$zcta)
zcta_flo_cent = st_centroid(zcta_flo) 

##calculating distance to florence track 
zcta.flo.dist = apply(st_distance(zcta_flo_cent, flo_track), 1, min) 

##attach distance to zcta from Florence 
zcta.florence = cbind(zcta_flo, zcta.flo.dist) %>%
  data.frame()

##add distance from Florence ZCTA to Florence Survey 
survey.florence = inner_join(surv_flo, 
                             zcta.florence, 
                             by = "zcta") %>%
  rename(dist.hurr = zcta.flo.dist) 

##pulling ZCTAs from suvey for Michael 
surv_mich = survey %>%
  filter(hurricane == "michael") 
zcta_mich = zcta %>%
  filter(zcta %in% surv_mich$zcta)
zcta_mich_cent = st_centroid(zcta_mich)

##calculating distance to michael track 
zcta.mich.dist = apply(st_distance(zcta_mich_cent, mich_track), 1, min)

##attach distance to zcta from Michael 
zcta.michael = cbind(zcta_mich, zcta.mich.dist) %>%
  data.frame()

##add distance from Michael ZCTA to Michael Survey 
survey.michael = inner_join(surv_mich, 
                            zcta.michael, 
                            by = "zcta") %>%
  rename(dist.hurr = zcta.mich.dist) 

##stack survey michael and survey florence 
survey.stack = rbind(survey.florence, survey.michael)


##attaching distance calculations to survey based on ZCTA
survey.out = inner_join(survey.stack, 
                        zcta.shore %>%
                          select(zcta, zcta.shore.dist), 
                        by =  "zcta") %>%
  select(-geometry)

data.table::fwrite(survey.out, "./OutputData/SurveyDistances.csv")

####Joining all Data Sets####
##data 
wind.house = data.table::fread("./OutputData/WindswathHouse.csv", 
                               colClasses = c(zcta = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""))


wind.sheldus = data.table::fread("./OutputData/WindSwath_Sheldus.csv",
                                 colClasses = c(zcta = "character")) %>%
  select(c("index", "Hazard", "Year", "Month", "CropDmg", CropDmg18 = "CropDmg(ADJ 2018)",
           CropDmgPerCap18 = "CropDmgPerCapita(ADJ 2018)",
           "PropertyDmg", PropDmg18 = "PropertyDmg(ADJ 2018)", 
           PropDmgPerCap18 = "PropertyDmgPerCapita(ADJ 2018)")) 

survey.house = data.table::fread("./OutputData/SurveyDistances.csv",
                                 colClasses = c(zcta = "character")) 
survey.sheldus = data.table::fread("./OutputData/SurveySheldus.csv",
                                   colClasses = c(zcta = "character")) %>% 
  select(c("index", "Hazard", "Year", "Month", "CropDmg", CropDmg18 = "CropDmg(ADJ 2018)",
           CropDmgPerCap18 = "CropDmgPerCapita(ADJ 2018)",
           "PropertyDmg", PropDmg18 = "PropertyDmg(ADJ 2018)", 
           PropDmgPerCap18 = "PropertyDmgPerCapita(ADJ 2018)")) 

##join survey 
survey.master = inner_join(survey.house, 
                           survey.sheldus, 
                           by = "index") 

survey.out = survey.master %>%
  select(-c(tot.units, err.tot.units, err.occ.units, vac.units, err.vac.units, owner.units, err.owner.units,
            renter.units, err.renter.units, no.veh, err.no.veh)) %>%
  mutate(CropDmg = ifelse(is.na(CropDmg), 0, CropDmg),
         CropDmg18 = ifelse(is.na(CropDmg18), 0, CropDmg18),
         CropDmgPerCap18 = ifelse(is.na(CropDmgPerCap18), 0 , CropDmgPerCap18),
         PropertyDmg = ifelse(is.na(PropertyDmg), 0, PropertyDmg),
         PropDmg18 = ifelse(is.na(PropDmg18), 0, PropDmg18),
         PropDmgPerCap18 = ifelse(is.na(PropDmgPerCap18), 0, PropDmgPerCap18)) %>%
  mutate(Hazard = ifelse(Hazard == "", "Hurricane/Tropical Storm", Hazard),
         Year = 2018,
         Month = ifelse(hurricane == "michael", 10, 9))

##join windswath
wind.master = inner_join(wind.house, 
                         wind.sheldus, 
                         by = "index")

wind.out = wind.master %>%
  # filter(bt_speed_max > 30) %>%
  select(-c(tot.units, err.tot.units, err.occ.units, vac.units, err.vac.units, owner.units, err.owner.units,
            renter.units, err.renter.units, no.veh, err.no.veh)) %>%
  mutate(CropDmg = ifelse(is.na(CropDmg), 0, CropDmg),
         CropDmg18 = ifelse(is.na(CropDmg18), 0, CropDmg18),
         CropDmgPerCap18 = ifelse(is.na(CropDmgPerCap18), 0 , CropDmgPerCap18),
         PropertyDmg = ifelse(is.na(PropertyDmg), 0, PropertyDmg),
         PropDmg18 = ifelse(is.na(PropDmg18), 0, PropDmg18),
         PropDmgPerCap18 = ifelse(is.na(PropDmgPerCap18), 0, PropDmgPerCap18)) %>%
  mutate(Hazard = ifelse(Hazard == "", "Hurricane/Tropical Storm", Hazard)) 

wind.out.michael = wind.out %>%
  filter(hurricane == "michael") %>%
  mutate(Year = 2018,
         Month = 10)
wind.out.florence = wind.out %>%
  filter(hurricane == "florence") %>%
  mutate(Year = 2018,
         Month = 9)
wind.out.irma = wind.out %>%
  filter(hurricane == "irma") %>%
  mutate(Year = 2017, 
         Month = 9)
wind.out.harvey = wind.out %>%
  filter(hurricane == "harvey") %>%
  mutate(Year = 2017,
         Month = 8)
wind.out.ike = wind.out %>%
  filter(hurricane == "ike") %>%
  mutate(Year = 2008, 
         Month = 9)

wind.write = rbind(wind.out.florence, wind.out.harvey, wind.out.ike, wind.out.irma, wind.out.michael)

data.table::fwrite(wind.write, "./OutputData/WindSwathMaster.csv")

##joining census averages
wind = wind.write %>%
  filter(bt_speed_max >= 30)
us.data = data = readxl::read_excel("./InputData/representation_table.xlsx", skip = 2,
                                    col_names = c("state", "tot.pop",
                                                  "pop18_24", "var_pop18_24", "pop25_34", "var_pop25_34", "pop35_44", "var_pop35_44",
                                                  "pop45_54", "var_pop45_54", "pop55_64", "var_pop55_64", "pop65_74", "var_pop65_74",
                                                  "pop75up", "var_pop75up", "male18up", "var_male18up", "female18up", "var_female18up",
                                                  "avg_housesize", "var_avg_housesize", "houses", "var_houses",
                                                  "ownr_occ_per", "var_ownr_occ_per", 
                                                  "rntr_occ_per", "var_rntr_occ_per", "median.age")) %>%
  mutate(state = str_to_lower(state)) %>%
  filter(state %in% wind$state) %>%
  select(male18up, female18up, avg_housesize, ownr_occ_per, rntr_occ_per, median.age)
zcta_atts = data.table::fread("./InputData/ZCTA_AvgData.csv", 
                              colClasses = c(zcta = "character")) %>%
  select(zcta, mean.inc) %>%
  filter(zcta %in% wind$zcta)

survey.out = survey %>%
  mutate(US_males = mean(us.data$male18up, na.rm = T), 
         US_hh = mean(us.data$avg_housesize, na.rm = T),
         US_ownr = mean(us.data$ownr_occ_per, na.rm = T), 
         US_med.age = mean(us.data$median.age, na.rm = T),
         US_inc = mean(zcta_atts$mean.inc, na.rm = T)) 



data.table::fwrite(survey.out, "./OutputData/SurveyMaster.csv")  


