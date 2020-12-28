######### Hurricane Survey Project --


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(here)
library(sf) 
library(maps)

# (1) Raw Survey Setup --------------------------------------------------------

# Michael
#Load data
michael <- read.csv(here("InputData", "HurricaneMichael_FinalData.csv"), 
                    comment.char="#",
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
                                              ifelse(Q21 == "Not likely", 1, "")))))) %>%
  separate(Q19, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), sep = ",") %>%
  mutate(A = ifelse(a == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(a == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(a == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(a == "I would like to see these changes implemented", 4, 
                                         ifelse(a == " but I cannot afford to pay much for it", 4, 
                                                ifelse(a == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(a == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(a == "I am not worried about hurricanes", 7, 
                                                                     ifelse(a == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(a == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(a == "Other", 10,
                                                                                          ifelse(a == " please specify", 10, 11)))))))))))),
         B = ifelse(b == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(b == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(b == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(b == "I would like to see these changes implemented", 4, 
                                         ifelse(b == " but I cannot afford to pay much for it", 4, 
                                                ifelse(b == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(b == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(b == "I am not worried about hurricanes", 7, 
                                                                     ifelse(b == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(b == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(b == "Other", 10,
                                                                                          ifelse(b == " please specify", 10, 11)))))))))))),
         C = ifelse(c == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(c == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(c == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(c == "I would like to see these changes implemented", 4, 
                                         ifelse(c == " but I cannot afford to pay much for it", 4, 
                                                ifelse(c == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(c == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(c == "I am not worried about hurricanes", 7, 
                                                                     ifelse(c == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(c == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(c == "Other", 10,
                                                                                          ifelse(c == " please specify", 10, 11)))))))))))),
         D = ifelse(d == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(d == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(d == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(d == "I would like to see these changes implemented", 4, 
                                         ifelse(d == " but I cannot afford to pay much for it", 4, 
                                                ifelse(d == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(d == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(d == "I am not worried about hurricanes", 7, 
                                                                     ifelse(d == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(d == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(d == "Other", 10,
                                                                                          ifelse(d == " please specify", 10, 11)))))))))))),
         E = ifelse(e == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(e == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(e == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(e == "I would like to see these changes implemented", 4, 
                                         ifelse(e == " but I cannot afford to pay much for it", 4, 
                                                ifelse(e == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(e == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(e == "I am not worried about hurricanes", 7, 
                                                                     ifelse(e == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(e == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(e == "Other", 10,
                                                                                          ifelse(e == " please specify", 10, 11)))))))))))),
         Ff = ifelse(f == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                     ifelse(f == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                            ifelse(f == "I believe that funding this project is well worth it to me", 3, 
                                   ifelse(f == "I would like to see these changes implemented", 4, 
                                          ifelse(f == " but I cannot afford to pay much for it", 4, 
                                                 ifelse(f == "It was difficult for me to decide which option to choose", 5, 
                                                        ifelse(f == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                               ifelse(f == "I am not worried about hurricanes", 7, 
                                                                      ifelse(f == "I did not read the information on the proposal carefully", 8, 
                                                                             ifelse(f == "I do not trust the federal government to solve this serious problem", 9,
                                                                                    ifelse(f == "Other", 10,
                                                                                           ifelse(f == " please specify", 10, 11)))))))))))),
         G = ifelse(g == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(g== "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(g == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(g == "I would like to see these changes implemented", 4, 
                                         ifelse(g == " but I cannot afford to pay much for it", 4, 
                                                ifelse(g == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(g == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(g == "I am not worried about hurricanes", 7, 
                                                                     ifelse(g == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(g == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(g == "Other", 10,
                                                                                          ifelse(g == " please specify", 10, 11)))))))))))),
         H = ifelse(h == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(h == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(h == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(h == "I would like to see these changes implemented", 4, 
                                         ifelse(h == " but I cannot afford to pay much for it", 4, 
                                                ifelse(h == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(h == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(h == "I am not worried about hurricanes", 7, 
                                                                     ifelse(h == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(h == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(h == "Other", 10,
                                                                                          ifelse(h == " please specify", 10, 11)))))))))))),
         I = ifelse(i == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(i == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(i == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(i == "I would like to see these changes implemented", 4, 
                                         ifelse(i == " but I cannot afford to pay much for it", 4, 
                                                ifelse(i == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(i == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(i == "I am not worried about hurricanes", 7, 
                                                                     ifelse(i == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(i == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(i == "Other", 10,
                                                                                          ifelse(i == " please specify", 10, 11)))))))))))),
         J = ifelse(j == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(j == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(j == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(j == "I would like to see these changes implemented", 4, 
                                         ifelse(j == " but I cannot afford to pay much for it", 4, 
                                                ifelse(j == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(j == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(j == "I am not worried about hurricanes", 7, 
                                                                     ifelse(j == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(j == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(j == "Other", 10,
                                                                                          ifelse(j == " please specify", 10, 11)))))))))))),
         K = ifelse(k == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(k == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(k == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(k == "I would like to see these changes implemented", 4, 
                                         ifelse(k == " but I cannot afford to pay much for it", 4, 
                                                ifelse(k == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(k == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(k == "I am not worried about hurricanes", 7, 
                                                                     ifelse(k == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(k == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(k == "Other", 10,
                                                                                          ifelse(k == " please specify", 10, 11)))))))))))),
         L = ifelse(l == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(l == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(l == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(l == "I would like to see these changes implemented", 4, 
                                         ifelse(l == " but I cannot afford to pay much for it", 4, 
                                                ifelse(l == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(l == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(l == "I am not worried about hurricanes", 7, 
                                                                     ifelse(l == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(l == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(l == "Other", 10,
                                                                                          ifelse(l == " please specify", 10, 11)))))))))))),
         tax = ifelse(A == 1 | B == 1 | C == 1 | D == 1 | E == 1 | Ff == 1 | G == 1 | H == 1 | I == 1 | J == 1 | K == 1 | L == 1, 1, 0),
         personal = ifelse(A == 2 | B == 2 | C == 2 | D == 2| E == 2| Ff == 2 | G == 2| H == 2| I == 2| J == 2| K == 2| L == 2, 1, 0), 
         worth = ifelse(A == 3 | B == 3| C == 3 | D == 3 | E == 3 | Ff == 3 | G == 3| H == 3 | I == 3 | J == 3 | K == 3| L == 3, 1, 0), 
         costly = ifelse(A == 4| B == 4| C == 4 | D == 4| E == 4| Ff == 4| G == 4| H == 4| I == 4 | J == 4| K == 4| L == 4, 1, 0), 
         conflicted = ifelse(A == 5 | B == 5| C == 5| D ==5 | E ==5 | Ff == 5| G == 5| H == 5| I == 5| J == 5 | K ==5 | L == 5, 1, 0), 
         incomplete = ifelse(A == 6 | B ==6 | C == 6| D ==6 | E == 6| Ff ==6 | G == 6| H == 6| I == 6| J == 6| K == 6| L == 6, 1, 0), 
         delusional = ifelse(A == 7| B == 7 | C == 7| D == 7| E == 7| Ff ==7 | G ==7 | H == 7| I == 7| J == 7 | K == 7| L ==7, 1, 0), 
         skimmed = ifelse(A == 8 | B == 8 | C ==8 | D == 8| E == 8 | Ff == 8 | G == 8 | H == 8| I == 8| J == 8 | K == 8| L ==8, 1, 0), 
         trust = ifelse(A == 9| B == 9 | C == 9| D == 9| E ==9 | Ff ==9 | G == 9 | H ==9| I ==9| J == 9 | K == 9| L == 9, 1, 0), 
         other = ifelse(A == 10 | B == 10| C == 10| D == 10 | E == 10 | Ff == 10 | G == 10| H == 10 | I == 10 | J == 10| K == 10| L == 10, 1, 0), 
         bad = ifelse(A == 11 | B == 11| C == 11| D == 11| E == 11| Ff == 11| G ==11 | H == 11| I == 11| J ==11 | K == 11| L == 11, 1, 0))

# select relevant variables 
data_michael <- michael %>% select(Duration..in.seconds., 
                                   hurricane, zip, age, female, owner, tenure, insured, short_risk, long_risk,
                                   hurricane_awareness, fema_awareness, nfip_awareness, experience, told,
                                   evacuate, damage, loss, displaced, influenced, hh_size, hh_dependent,
                                   rain_att, wind_att, surge_att, track_att,
                                   track_order, wind_order, rain_order, track_rate, wind_rate, rain_rate,
                                   track_bid1, wind_bid1, rain_bid1, track_answer1, wind_answer1, rain_answer1,
                                   track_bid2, wind_bid2, rain_bid2, track_answer2, wind_answer2, rain_answer2, 
                                   voice, action, tax, personal, worth, costly, conflicted, incomplete, 
                                   delusional, skimmed, trust, other) %>%
  mutate(tax = ifelse(is.na(tax), 0, tax), 
         personal = ifelse(is.na(personal), 0, personal), 
         worth = ifelse(is.na(worth), 0, worth), 
         costly = ifelse(is.na(costly), 0, costly), 
         conflicted = ifelse(is.na(conflicted), 0, costly), 
         incomplete = ifelse(is.na(incomplete), 0, incomplete), 
         delusional = ifelse(is.na(delusional), 0, delusional), 
         skimmed = ifelse(is.na(skimmed), 0, skimmed), 
         trust = ifelse(is.na(trust), 0, trust), 
         other = ifelse(is.na(other), 0, other))

# Florence

#Load data
florence <- read.csv(here("InputData", "HurricaneFlorence_FinalData.csv"), 
                     comment.char="#",
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
                                              ifelse(Q21 == "Not likely", 1, "")))))) %>%
  separate(Q19, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), sep = ",") %>%
  mutate(A = ifelse(a == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(a == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(a == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(a == "I would like to see these changes implemented", 4, 
                                         ifelse(a == " but I cannot afford to pay much for it", 4, 
                                                ifelse(a == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(a == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(a == "I am not worried about hurricanes", 7, 
                                                                     ifelse(a == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(a == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(a == "Other", 10,
                                                                                          ifelse(a == " please specify", 10, 11)))))))))))),
         B = ifelse(b == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(b == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(b == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(b == "I would like to see these changes implemented", 4, 
                                         ifelse(b == " but I cannot afford to pay much for it", 4, 
                                                ifelse(b == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(b == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(b == "I am not worried about hurricanes", 7, 
                                                                     ifelse(b == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(b == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(b == "Other", 10,
                                                                                          ifelse(b == " please specify", 10, 11)))))))))))),
         C = ifelse(c == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(c == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(c == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(c == "I would like to see these changes implemented", 4, 
                                         ifelse(c == " but I cannot afford to pay much for it", 4, 
                                                ifelse(c == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(c == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(c == "I am not worried about hurricanes", 7, 
                                                                     ifelse(c == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(c == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(c == "Other", 10,
                                                                                          ifelse(c == " please specify", 10, 11)))))))))))),
         D = ifelse(d == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(d == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(d == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(d == "I would like to see these changes implemented", 4, 
                                         ifelse(d == " but I cannot afford to pay much for it", 4, 
                                                ifelse(d == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(d == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(d == "I am not worried about hurricanes", 7, 
                                                                     ifelse(d == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(d == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(d == "Other", 10,
                                                                                          ifelse(d == " please specify", 10, 11)))))))))))),
         E = ifelse(e == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(e == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(e == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(e == "I would like to see these changes implemented", 4, 
                                         ifelse(e == " but I cannot afford to pay much for it", 4, 
                                                ifelse(e == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(e == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(e == "I am not worried about hurricanes", 7, 
                                                                     ifelse(e == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(e == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(e == "Other", 10,
                                                                                          ifelse(e == " please specify", 10, 11)))))))))))),
         Ff = ifelse(f == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                     ifelse(f == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                            ifelse(f == "I believe that funding this project is well worth it to me", 3, 
                                   ifelse(f == "I would like to see these changes implemented", 4, 
                                          ifelse(f == " but I cannot afford to pay much for it", 4, 
                                                 ifelse(f == "It was difficult for me to decide which option to choose", 5, 
                                                        ifelse(f == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                               ifelse(f == "I am not worried about hurricanes", 7, 
                                                                      ifelse(f == "I did not read the information on the proposal carefully", 8, 
                                                                             ifelse(f == "I do not trust the federal government to solve this serious problem", 9,
                                                                                    ifelse(f == "Other", 10,
                                                                                           ifelse(f == " please specify", 10, 11)))))))))))),
         G = ifelse(g == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(g== "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(g == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(g == "I would like to see these changes implemented", 4, 
                                         ifelse(g == " but I cannot afford to pay much for it", 4, 
                                                ifelse(g == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(g == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(g == "I am not worried about hurricanes", 7, 
                                                                     ifelse(g == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(g == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(g == "Other", 10,
                                                                                          ifelse(g == " please specify", 10, 11)))))))))))),
         H = ifelse(h == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(h == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(h == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(h == "I would like to see these changes implemented", 4, 
                                         ifelse(h == " but I cannot afford to pay much for it", 4, 
                                                ifelse(h == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(h == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(h == "I am not worried about hurricanes", 7, 
                                                                     ifelse(h == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(h == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(h == "Other", 10,
                                                                                          ifelse(h == " please specify", 10, 11)))))))))))),
         I = ifelse(i == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(i == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(i == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(i == "I would like to see these changes implemented", 4, 
                                         ifelse(i == " but I cannot afford to pay much for it", 4, 
                                                ifelse(i == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(i == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(i == "I am not worried about hurricanes", 7, 
                                                                     ifelse(i == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(i == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(i == "Other", 10,
                                                                                          ifelse(i == " please specify", 10, 11)))))))))))),
         J = ifelse(j == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(j == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(j == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(j == "I would like to see these changes implemented", 4, 
                                         ifelse(j == " but I cannot afford to pay much for it", 4, 
                                                ifelse(j == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(j == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(j == "I am not worried about hurricanes", 7, 
                                                                     ifelse(j == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(j == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(j == "Other", 10,
                                                                                          ifelse(j == " please specify", 10, 11)))))))))))),
         K = ifelse(k == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(k == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(k == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(k == "I would like to see these changes implemented", 4, 
                                         ifelse(k == " but I cannot afford to pay much for it", 4, 
                                                ifelse(k == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(k == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(k == "I am not worried about hurricanes", 7, 
                                                                     ifelse(k == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(k == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(k == "Other", 10,
                                                                                          ifelse(k == " please specify", 10, 11)))))))))))),
         L = ifelse(l == "I believe that my taxes are too high already and am against any initiative that will increase them", 1 ,
                    ifelse(l == "I feel that homes and businesses in areas at risk should cover their own losses", 2, 
                           ifelse(l == "I believe that funding this project is well worth it to me", 3, 
                                  ifelse(l == "I would like to see these changes implemented", 4, 
                                         ifelse(l == " but I cannot afford to pay much for it", 4, 
                                                ifelse(l == "It was difficult for me to decide which option to choose", 5, 
                                                       ifelse(l == "I do not have enough information on this issue to make a comfortable decision", 6, 
                                                              ifelse(l == "I am not worried about hurricanes", 7, 
                                                                     ifelse(l == "I did not read the information on the proposal carefully", 8, 
                                                                            ifelse(l == "I do not trust the federal government to solve this serious problem", 9,
                                                                                   ifelse(l == "Other", 10,
                                                                                          ifelse(l == " please specify", 10, 11)))))))))))),
         tax = ifelse(A == 1 | B == 1 | C == 1 | D == 1 | E == 1 | Ff == 1 | G == 1 | H == 1 | I == 1 | J == 1 | K == 1 | L == 1, 1, 0),
         personal = ifelse(A == 2 | B == 2 | C == 2 | D == 2| E == 2| Ff == 2 | G == 2| H == 2| I == 2| J == 2| K == 2| L == 2, 1, 0), 
         worth = ifelse(A == 3 | B == 3| C == 3 | D == 3 | E == 3 | Ff == 3 | G == 3| H == 3 | I == 3 | J == 3 | K == 3| L == 3, 1, 0), 
         costly = ifelse(A == 4| B == 4| C == 4 | D == 4| E == 4| Ff == 4| G == 4| H == 4| I == 4 | J == 4| K == 4| L == 4, 1, 0), 
         conflicted = ifelse(A == 5 | B == 5| C == 5| D ==5 | E ==5 | Ff == 5| G == 5| H == 5| I == 5| J == 5 | K ==5 | L == 5, 1, 0), 
         incomplete = ifelse(A == 6 | B ==6 | C == 6| D ==6 | E == 6| Ff ==6 | G == 6| H == 6| I == 6| J == 6| K == 6| L == 6, 1, 0), 
         delusional = ifelse(A == 7| B == 7 | C == 7| D == 7| E == 7| Ff ==7 | G ==7 | H == 7| I == 7| J == 7 | K == 7| L ==7, 1, 0), 
         skimmed = ifelse(A == 8 | B == 8 | C ==8 | D == 8| E == 8 | Ff == 8 | G == 8 | H == 8| I == 8| J == 8 | K == 8| L ==8, 1, 0), 
         trust = ifelse(A == 9| B == 9 | C == 9| D == 9| E ==9 | Ff ==9 | G == 9 | H ==9| I ==9| J == 9 | K == 9| L == 9, 1, 0), 
         other = ifelse(A == 10 | B == 10| C == 10| D == 10 | E == 10 | Ff == 10 | G == 10| H == 10 | I == 10 | J == 10| K == 10| L == 10, 1, 0), 
         bad = ifelse(A == 11 | B == 11| C == 11| D == 11| E == 11| Ff == 11| G ==11 | H == 11| I == 11| J ==11 | K == 11| L == 11, 1, 0))

# select relevant variables 
data_florence <- florence %>% select(Duration..in.seconds., 
                                     hurricane, zip, age, female, owner, tenure, insured, short_risk, long_risk,
                                     hurricane_awareness, fema_awareness, nfip_awareness, experience, told,
                                     evacuate, damage, loss, displaced, influenced, hh_size, hh_dependent,
                                     rain_att, wind_att, surge_att, track_att,
                                     track_order, wind_order, rain_order, track_rate, wind_rate, rain_rate,
                                     track_bid1, wind_bid1, rain_bid1, track_answer1, wind_answer1, rain_answer1,
                                     track_bid2, wind_bid2, rain_bid2, track_answer2, wind_answer2, rain_answer2, 
                                     voice, action, tax, personal, worth, costly, conflicted, incomplete, 
                                     delusional, skimmed, trust, other) %>%
  mutate(tax = ifelse(is.na(tax), 0, tax), 
         personal = ifelse(is.na(personal), 0, personal), 
         worth = ifelse(is.na(worth), 0, worth), 
         costly = ifelse(is.na(costly), 0, costly), 
         conflicted = ifelse(is.na(conflicted), 0, costly), 
         incomplete = ifelse(is.na(incomplete), 0, incomplete), 
         delusional = ifelse(is.na(delusional), 0, delusional), 
         skimmed = ifelse(is.na(skimmed), 0, skimmed), 
         trust = ifelse(is.na(trust), 0, trust), 
         other = ifelse(is.na(other), 0, other))



# Stack the two datasets

data <- rbind(data_florence,data_michael) %>% mutate(florence = ifelse(hurricane == "florence",1,0))
survey1 = data %>%
  mutate(index = row_number())


# ZCTA Controls 2018 5YR --------------------------------------------------

##New Data 
  ##ACS 2018 5YR 
ACS2018 = data.table::fread(here("InputData", "ACSDP5Y2018", "income_sex_byzip.csv"))

ACS2018.df = ACS2018[-1,] %>%
  select(1, 3, 7, 11, 15, 19) %>%
  rename(geoid = GEO_ID, 
         tot.pop18 = DP05_0021E,
         male.pop18 = DP05_0026E,
         fem.pop18 = DP05_0027E,
         med.inc18 = DP03_0062E,
         mean.inc18 = DP03_0063E) %>%
  mutate(zcta = str_sub(geoid, 10, nchar(geoid)),
         tot.pop18 = as.numeric(tot.pop18),
         male.pop18 = as.numeric(male.pop18),
         fem.pop18 = as.numeric(fem.pop18), 
         med.inc18 = as.numeric(med.inc18),
         mean.inc18 = as.numeric(mean.inc18)) 


# (2) Spatial Set Up for Survey Data ------------------------------------------

##epsg:102003 is USA Contiguous Albers Equal Area Conic

##Old Data: 
  ##survey - Raw Survey Data 
  ##ACS2018.df - ZCTA Demographic Attributes 

##New Data
  ##state fips codes 
data(state.fips)

##Data Set of Counties Experiencing Hurricane Wind Speeds 
wind.original = data.table::fread(here("InputData", "Windswath_Original.csv")) %>%
  mutate(index = row_number())

county = sf::st_read(here("InputData", "tl_2019_us_county", "tl_2019_us_county.shp"), 
                       stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  st_transform('ESRI:102003') %>%
  select(statefp = STATEFP,
         countyfp = COUNTYFP, 
         name = NAMELSAD, 
         GEOID, 
         geometry) %>%
  mutate(county.name = str_to_lower(name, locale = "en")) %>%
  select(-name)

  ##ZCTA Tracks (2019) 
zcta = st_read(here("InputData", "tl_2019_us_zcta510", "tl_2019_us_zcta510.shp")) %>%
  select(zcta = ZCTA5CE10,
         geometry) %>%
  st_transform('ESRI:102003')

##County Name Set Up 
## Joining WindSwath to StateFIPS for FIPS ID and State Abb
wind.swaths.st.fips = left_join(wind.original, 
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
counties.state = inner_join(county, 
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
##ID Zips not 1 to 1 ZCTA
bad.zips = setdiff(survey1$zip, zcta$zcta) 
##serached on https://www.udsmapper.org/zcta-crosswalk.cfm
rep.zcta = c(28409, 27330, 29501, 27705, 27408, 28504, 28540, 28112, 27546, 27705, 28401, 27403, 
             27707, 28387, 27403, 29577, 29526, 28411, 28374, 27252, 28540, 31701, 32305, 32304,
             39817, 32304, 32401, 32310, 31705, 32405, 31021, 32327, 31015, 32303, 31701)
##rename
survey.zcta = survey1 %>%
  mutate(zcta = zip)
##For Bad ZIPS --> ZCTA, we replace with searched ZCTA
survey.zcta$zcta = rep.zcta[match(survey.zcta$zcta, bad.zips)]

##Fill ZCTA NAs with  1 to 1 match ZIPs with ZCTAs. 
survey.fixed.zcta = survey.zcta %>%
  mutate(zcta = ifelse(is.na(zcta), zip, zcta),
         zcta = as.character(zcta))

##ZCTA Geometries for Survey 
survey.zcta.sf = inner_join(survey.fixed.zcta,
                            zcta, 
                            by = "zcta") %>%
  st_as_sf(crs = 'ESRI:102003')

##ACS2018
survey.atts = inner_join(survey.zcta.sf, 
                         ACS2018.df, 
                         by = "zcta")

##State, County and Unique Names(keeps ZCTA geometries) 
survey.county = st_join(survey.atts, 
                      counties.for.join, 
                      left = F, ##drop ZCTAs not within any counties 
                      join = st_within, ##join by ZCTAs within counties 
                      largest = T)

##Rename
survey2 = survey.county 

# Aggregate ZCTA Housing Data ACS(2017) and ZCTA Population Data ACS(2018) -------------------------------------------

##Old Data: 
  ##zcta = ID and Geom for all ZCTA tracks 
  ##county = US County Names and Geoms 
  ##ACS2018.df = ZCTA demographic attributes 
  

##New Data:
  ##Housing Data from 2013-2017 5 year ACS (ZCTA) 
house = data.table::fread(here("InputData", "ACS_17_5YR_DP04", "ACS_17_5YR_DP04_with_ann.csv"),
                          colClasses = c(GEO.id2 = "character")) %>%
select(c(2,8)) %>%
  slice(-1) %>%
  rename(zcta = GEO.id2, 
         occ.units = HC01_VC04) %>% 
  mutate(occ.units = as.numeric(occ.units)) 


##ZCTA Geometries for Housing Data 
zcta.house = left_join(house, 
                       zcta,
                       by = "zcta") %>%
  st_as_sf() %>%
  st_transform('ESRI:102003')
##Housing at ZCTA Level with Assigned Counties for each ZCTA 
county.house = st_join(zcta.house,
                      county %>%
                        select(GEOID, geometry),
                      join = st_within, 
                      largest = T, 
                      left = F)
##Aggregate Housing by County (GEOID) 
##Provides Data Set for Estimating WTP (Occupied Housing at County Level)
agg.county.house = data.table::setDT(county.house)[, county.occ := sum(occ.units), by = GEOID] %>%
  select(GEOID, county.occ) %>%
  group_by(GEOID) %>%
  slice(1)  ##Only need row for each County (GEOID)

##Write Out 
# data.table::fwrite(agg.county.house, here("OutputData", "CountyHousingTotal.csv"))

##Population Estimates 
##adding spatial attributes to zcta populations 
zcta.atts.geo = left_join(ACS2018.df, 
                      zcta, 
                      by = "zcta") %>%
  st_as_sf() %>%
  st_transform('ESRI:102003')

##Population at ZCTA Level with Assigned Counties for each ZCTA 
county.pop = st_join(zcta.atts.geo,
                          county %>%
                            select(GEOID, geometry),
                          join = st_within, 
                          largest = T, 
                          left = F)

##Aggregate Population by County 
##Provides Total Population at County Level for WTP per Person Estimates 
agg.county.pop = data.table::setDT(county.pop)[, county.pop := sum(tot.pop18), by = GEOID] %>%
  select(GEOID, county.pop) %>%
  group_by(GEOID) %>%
  slice(1) 
##Write Out
# data.table::fwrite(agg.county.pop, here("OutputData", "CountyPopTotal.csv"))


# (3) ZCTA Distance to Shoreline and Hurricane Track --------------------------

##Old Data:
  ##zcta - ZCTA IDs and Geoms 
  ##survey2 = Raw Survey + ACS2018 Demographics + State, County Info + Geometry

##New Data:
  ##State Polygons 
states = st_read(here("InputData", "tl_2019_us_state", "tl_2019_us_state.shp")) %>%
  mutate(NAME = str_to_lower(NAME)) %>%
  st_transform('ESRI:102003')

  ##Shoreline Line Feature (converted from polygon in ArcPro)  
shoreline = st_read(here("InputData", "us_coastline_linefeat", "nam_coast_lines.shp")) %>%
  st_transform('ESRI:102003')

  ##Florence Hurricane Track Line Feature 
flo.track = st_read(here("InputData", "Florence_TRACK", "AL062018_lin.shp")) %>%
  st_transform('ESRI:102003')
  ##Michael Hurricane Track Line Feature 
mich.track = st_read(here("InputData", "Michael_TRACK", "AL142018_lin.shp")) %>%
  st_transform('ESRI:102003')

##Survey ZCTAs with Geoms 
zcta.dist = zcta %>%
  filter(zcta %in% survey2$zcta)

##Centroid of ZCTA Polygon 
zcta.centroid = st_centroid(zcta.dist)

##ZCTA Centroid to Nearest Shoreline (meters) 
zcta.shore.dist = apply(st_distance(zcta.centroid, shoreline), 1, min)

##Data Frame of ZCTA and Distance from Centroid 
zcta.shore = cbind(zcta.dist, zcta.shore.dist) %>%
  data.frame() %>%
  rename(dist.shore = zcta.shore.dist)

##Survey Florence 
surv.flo = survey2 %>%
  filter(hurricane == "florence")

##Florence ZCTAs 
zcta.flo = zcta %>%
  filter(zcta %in% surv.flo$zcta)

##Florence ZCTAs Centroid 
zcta.flo.cent = st_centroid(zcta.flo) 

##Florence ZCTA nearest distance (meters) Florence Track 
zcta.flo.dist = apply(st_distance(zcta.flo.cent, flo.track), 1, min) 

##Data Frame of ZCTA and Distance to Florence Track 
zcta.florence.track = cbind(zcta.flo %>%
                              st_drop_geometry(),
                            zcta.flo.dist) %>%
  data.frame()

##ZCTA Centroid Nearest Distance to Florence Track (meters) Data Frame 
survey.florence = inner_join(surv.flo, 
                             zcta.florence.track, 
                             by = "zcta") %>%
  rename(dist.hurr = zcta.flo.dist) 

##Survey Michael 
surv.mich = survey2 %>%
  filter(hurricane == "michael") 

##Michael ZCTAs 
zcta.mich = zcta %>%
  filter(zcta %in% surv.mich$zcta)

##Michael ZCTA Centroids 
zcta.mich.cent = st_centroid(zcta.mich)

##Michael ZCTA nearest distance (meters) Michael Track 
zcta.mich.dist = apply(st_distance(zcta.mich.cent, mich.track), 1, min)

##Data Frame of ZCTA and Distance to Michael Track 
zcta.michael = cbind(zcta.mich %>% 
                       st_drop_geometry(),
                     zcta.mich.dist) %>%
  data.frame()

##add distance from Michael ZCTA to Michael Survey 
survey.michael = inner_join(surv.mich, 
                            zcta.michael, 
                            by = "zcta") %>%
  rename(dist.hurr = zcta.mich.dist) 

##Stack Florence and Michael Survey 
survey.stack = rbind(survey.florence, survey.michael)


##attaching distance calculations to survey based on ZCTA
survey.distance = inner_join(survey.stack, 
                        zcta.shore %>%
                          select(zcta, dist.shore), 
                        by =  "zcta")

##Rename Survey 
survey3 = survey.distance

# (4) Impacted State Average Demographic Calculations  ----------------------------------------------------

##Old Data: 
  ## survey3 = Raw Survey + ACS2018 Demographics + State, County Info + Geometry
  ##          Distance to Shore + Distance to Hurricane Track
  ##ACS2018.df = ACS 5YR 2018 demographic data 



##New Data: 

##Data Set of States Experiencing Hurricane Winds in US 2003-2016
windswath = data.table::fread(here("InputData", "WindSwathMaster.csv"), 
                              colClasses = c(statefp = "character", countyfp = "character")) %>%
  mutate(statefp = str_pad(statefp, 2, side = "left", pad = "0"),
         countyfp = str_pad(countyfp, 3, side = "left", pad = "0"),
         geoid = paste(statefp, countyfp, sep = ""),
         ownrfrac = owner.units/tot.units,
         femfrac = fem.pop/tot.pop)

##Data Set of All Areas Impacted by Hurricane Force Winds >=30 mph 2003-2016
wind30 = windswath %>%
  filter(bt_speed_max >= 30)

##Creating ACS2018 5YR Data Set for Income by ZCTA for Impaced ZCTAs by Hurricane Winds >= 30 mph 
ACS2018.impacted = ACS2018.df %>%
  select(zcta, mean.inc18) %>%
  filter(zcta %in% wind30$zcta)

##ACS 1YR 2018 Data for each State Identified as Impacted by >=30 mph Hurricane Winds 2003-2016
us.data = data = readxl::read_excel(here("InputData", "ACSSPP1Y2018", 
                                         "representation_table.xlsx"), skip = 2,
                                    col_names = c("state", "tot.pop",
                                                  "pop18_24", "var_pop18_24", "pop25_34", "var_pop25_34", "pop35_44", "var_pop35_44",
                                                  "pop45_54", "var_pop45_54", "pop55_64", "var_pop55_64", "pop65_74", "var_pop65_74",
                                                  "pop75up", "var_pop75up", "male18up", "var_male18up", "female18up", "var_female18up",
                                                  "avg_housesize", "var_avg_housesize", "houses", "var_houses",
                                                  "ownr_occ_per", "var_ownr_occ_per", 
                                                  "rntr_occ_per", "var_rntr_occ_per", "median.age")) %>%
  mutate(state = str_to_lower(state)) %>%
  filter(state %in% wind30$state) %>% ##States Experience >=30 MPH Winds 
  select(state, male18up, female18up, avg_housesize, ownr_occ_per, rntr_occ_per, median.age)

##Including US Averages (based on impacted states) for Relevant Covariates 
survey.USavg = survey3 %>%
  mutate(US_males = mean(us.data$male18up, na.rm = T), 
         US_hh = mean(us.data$avg_housesize, na.rm = T),
         US_ownr = mean(us.data$ownr_occ_per, na.rm = T), 
         US_med.age = mean(us.data$median.age, na.rm = T),
         US_inc18 = mean(ACS2018.impacted$mean.inc18, na.rm = T)) 


##Rename
survey4 = survey.USavg 

# (5) Adding 2008 Income Averages  ----------------------------------------


##Old Data: 
## survey4= Raw Survey + ACS2018 Demographics + State, County Info + Geometry
##          Distance to Shore + Distance to Hurricane Track + US Impacted State Demo Avg 


##New Data: 
##ACS 5YR 2011 (2007-2011) Income Statistics by ZCTA 
inc2011 = data.table::fread(here("InputData", "ACSDP5Y2011", "ACS2011-Inc.csv")) %>%
  select(geoid = GEO_ID, 
         med.inc = DP03_0062E, 
         mean.inc = DP03_0063E) %>%
  mutate(med.inc = as.numeric(med.inc),
         mean.inc = as.numeric(mean.inc),
         ##converting to 2018 Dollars for Comparison with 2018 Incomes 
         med.inc11 = med.inc*1.13, ##https://data.bls.gov/cgi-bin/cpicalc.pl convert 
         mean.inc11 = mean.inc*1.13, 
         zcta = str_sub(geoid, 10, nchar(geoid)))  ##from Jan 2011 to Jan 2018

##Add Income ACS 5YR 2011 Data to Survey 
survey.08inc = left_join(survey4, 
                       inc2011 %>% 
                         select(zcta, mean.inc11, med.inc11), 
                       by = "zcta")

##Rename 
survey5 = survey.08inc

# (6) Hurricane Forecast Errors  ------------------------------------------

##Old Datat:
## survey5 = Raw Survey + ACS2018 Demographics + State, County Info + Geometry
##          Distance to Shore + Distance to Hurricane Track + US Impacted State Demo Avg
##          ZCTA 2011 Incomes  

##New Data:
  ##Hurricane Forecast Errors 
fore.err = data.table::fread(here("InputData", "ALL_swath_wind_differences_in_time.csv")) %>%
  mutate(unique.name = paste(state, county, sep = " "))

##Join Forecast Error for Michael and Florence (Survey Hurricanes) 
survey.err = left_join(survey5, 
                       fore.err %>%
                         select(unique.name, hurricane, dif_max, dif_mean, dif_min, dif_median, dif_sd, 
                                lag), 
                       by = c("unique.name" = "unique.name", "hurricane" = "hurricane")) %>%
  filter(lag == -3) ##72 Hour Forecast Error 

##Rename 
survey6 = survey.err 

# (7) Averages Demographic Statistics by Maxiumum Experienced Wind --------

##Old Data: 
## survey5 = Raw Survey + ACS2018 Demographics + State, County Info + Geometry
##          Distance to Shore + Distance to Hurricane Track + US Impacted State Demo Avg
##          ZCTA 2011 Incomes + 72 Hr Forecast Error 


##Setting Up Maximum Speed Groups for Impacted Area Cutoffs 
speed = c(20, 30, 40, 50) 

##Empty Data Frame for Loop
new.avg = data.frame()
##Loop Average Demographic based on Maximum Windspeed Experienced 
for(i in 1:length(speed)){
  avg = windswath %>%
    filter(bt_speed_max <= speed[i]) %>%
    group_by(geoid) %>%
    slice(which.max(bt_speed_max)) %>%
    ungroup() %>%
    summarize(avg.fem = mean(femfrac, na.rm = T), 
              avg.meaninc = mean(mean.inc, na.rm = T), 
              avg.medinc = mean(med.inc, na.rm = T), 
              avg.owner = mean(ownrfrac, na.rm = T))
  new.avg = rbind(new.avg, avg)
}

##Average Demographic Calculations Table 
attch.avg = cbind(speed, new.avg)


survey.impacted = survey6 %>%
  mutate(fem_20ws = attch.avg[attch.avg$speed == 20, ]$avg.fem, 
         fem_30ws = attch.avg[attch.avg$speed == 30, ]$avg.fem, 
         fem_40ws = attch.avg[attch.avg$speed == 40, ]$avg.fem,
         fem_50ws = attch.avg[attch.avg$speed == 50, ]$avg.fem, 
         meaninc_20ws = attch.avg[attch.avg$speed == 20, ]$avg.meaninc,
         meaninc_30ws = attch.avg[attch.avg$speed == 30, ]$avg.meaninc,
         meaninc_40ws = attch.avg[attch.avg$speed == 40, ]$avg.meaninc,
         meaninc_50ws = attch.avg[attch.avg$speed == 50, ]$avg.meaninc,
         ownr_20ws = attch.avg[attch.avg$speed == 20, ]$avg.owner,
         ownr_30ws = attch.avg[attch.avg$speed == 30, ]$avg.owner,
         ownr_40ws = attch.avg[attch.avg$speed == 40, ]$avg.owner, 
         ownr_50ws = attch.avg[attch.avg$speed == 50, ]$avg.owner) 

##Write Out Data Set 
survey.out = survey.impacted %>%
  select(index, 
        ##General Location 
        statefp, state, countyfp, county, unique.name, hurricane, zcta,
        ##Response Variables, 
        time.seconds = Duration..in.seconds., track_order, wind_order, rain_order, 
        track_order, wind_order, rain_order, track_rate, wind_rate, rain_rate, track_bid1, 
        wind_bid1, rain_bid1, track_answer1, wind_answer1, rain_answer1, track_bid2, 
        wind_bid2, rain_bid2, track_answer2, wind_answer2, rain_answer2, 
        ##Control Set 1
        mean.inc18, female, experience, evacuate, voice, action, long_risk,
        ##Control Set 2
         age, owner, tenure, short_risk, hurricane_awareness, fema_awareness, nfip_awareness,
         damage, hh_size, dist.shore, 
        ##Exposure Specific Demo Averages
        fem_20ws, fem_30ws, fem_40ws, fem_50ws, meaninc_20ws, meaninc_30ws, meaninc_40ws, 
        meaninc_50ws, ownr_20ws, ownr_30ws, ownr_40ws, ownr_50ws, 
        ##RR New Covariates 
        tax, personal, worth, costly, trust, 
        mean.inc11, ##pre HFIP funding (2008) 
        US_inc18, ##exposed population
        dif_mean, lag ##mean forecast error 
        )
data.table::fwrite(survey.out %>%
                     st_drop_geometry(), here("OutputData", "Survey_Master.csv"))

