######### Hurricane Survey Project --


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(here)
library(sf) 
library(maps)

# Raw Survey Setup --------------------------------------------------------

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
survey = data %>%
  mutate(index = row_number())



# ZCTA Controls 2018 5YR --------------------------------------------------
ACS2018 = data.table::fread(here("InputData", "ACSDP5Y2018", "income_sex_byzip.csv"))

ACS2018.df = ACS2018[-1,] %>%
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


# Spatial Set Up for Survey Data ------------------------------------------

##epsg:102003 is USA Contiguous Albers Equal Area Conic

##Data Sets We Have:: 
  ##survey - Survey Data 
  ##ACS2018.df - ZCTA controls

##New Data
##state fips codes 
data(state.fips)

##ZCTA Tracks (2019) 
zcta = st_read(here("InputData", "tl_2019_us_zcta510", "tl_2019_us_zcta510.shp")) %>%
  select(zcta = ZCTA5CE10,
         geometry) %>%
  st_transform('ESRI:102003')

##Convert Survey Zips to ZCTAs: Add ZCTA Geometries and ZCTA Socioeconomic Attributes 
##ID Zips not 1 to 1 ZCTA
bad.zips = setdiff(survey$zip, zcta$zcta) 
##serached on https://www.udsmapper.org/zcta-crosswalk.cfm
rep.zcta = c(28409, 27330, 29501, 27705, 27408, 28504, 28540, 28112, 27546, 27705, 28401, 27403, 
             27707, 28387, 27403, 29577, 29526, 28411, 28374, 27252, 28540, 31701, 32305, 32304,
             39817, 32304, 32401, 32310, 31705, 32405, 31021, 32327, 31015, 32303, 31701)
##rename
survey.zcta = survey %>%
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


# ZCTA Housing Data ACS(2017) with Survey -------------------------------------------

##Data Sets We Need:: 
    ## survey.atts = Survey Raw + ZCTA Demo Attribures and Geom 

##housing data from 2013-2017 5 year ACS (by zcta) 
house = data.table::fread(here("InputData", "ACS_17_5YR_DP04", "ACS_17_5YR_DP04_with_ann.csv"),
                          colClasses = c(GEO.id2 = "character")) %>%
  select(c(2,4,5,8,9,12,13,184,185,188,189,232,233)) %>%
  slice(-1) 
names(house.use) = c("zcta", "tot.units", "err.tot.units", "occ.units", "err.occ.units",
                     "vac.units", "err.vac.units", "owner.units", "err.owner.units", 
                     "renter.units", "err.renter.units", "no.veh", "err.no.veh")

##zcta attributes from prior ZCTA_Controls script 
zcta.atts = data.table::fread("./InputData/ZCTA_AvgData.csv", 
                              colClasses = c(zcta = "character", med.inc = "integer"))
##ZCTA Geospatial Add
zcta = st_read("./InputData/tl_2019_us_zcta510/tl_2019_us_zcta510.shp",
               stringsAsFactors = F) %>%
  st_transform('ESRI:102003')

##adding zcta spatial reference to house.use
zcta.house = left_join(house.use, 
                       zcta %>%
                         select(ZCTA5CE10, geometry), 
                       by = c("zcta" = "ZCTA5CE10")) %>%
  st_as_sf() %>%
  st_transform('ESRI:102003')


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


