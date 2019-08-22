library(readr)
library(dplyr)

import_data <- read_csv("C:/Users/ss12293/Box Sync/Hearing Depression Project/hd_98_14.csv")
hearing_data <- import_data

hearing_data <- hearing_data %>% rename(hearing_aid_14 = OC102, hearing_14 = OC103, 
                                        hearing_aid_12 = NC102, hearing_12 = NC103, 
                                        hearing_aid_10 = MC102, hearing_10 = MC103,
                                        hearing_aid_08 = LC102, hearing_08 = LC103,
                                        hearing_aid_06 = KC102, hearing_06 = KC103,
                                        hearing_aid_04 = JC102, hearing_04 = JC103,
                                        hearing_aid_02 = HC102, hearing_02 = HC103,
                                        hearing_aid_00 = G1368, hearing_00 = G1369,
                                        hearing_aid_PL_02 = HZ090,
                                        hearing_aid_PL_04 = JZ090, hearing_aid_PL_06 = KZ090, 
                                        hearing_aid_PL_08 = LZ090, hearing_aid_PL_10 = MZ090,
                                        hearing_aid_PL_12 = NZ090, hearing_aid_PL_14 = OZ090)

hearing_data <- hearing_data %>% 
  mutate(hearing_prob_00 = if_else((hearing_00 == 4 | hearing_00 == 5), 1, 0)) %>%
  mutate(hearing_prob_02 = if_else((hearing_02 == 4 | hearing_02 == 5), 1, 0)) %>%
  mutate(hearing_prob_04 = if_else((hearing_04 == 4 | hearing_04 == 5), 1, 0)) %>%
  mutate(hearing_prob_06 = if_else((hearing_06 == 4 | hearing_06 == 5), 1, 0)) %>%
  mutate(hearing_prob_08 = if_else((hearing_08 == 4 | hearing_08 == 5), 1, 0)) %>%
  mutate(hearing_prob_10 = if_else((hearing_10 == 4 | hearing_10 == 5), 1, 0)) %>%
  mutate(hearing_prob_12 = if_else((hearing_12 == 4 | hearing_12 == 5), 1, 0)) %>%
  mutate(hearing_prob_14 = if_else((hearing_14 == 4 | hearing_14 == 5), 1, 0)) 


# Depression (8 Items)

hearing_data <- hearing_data %>% rename(felt_depressed_00 = G1669,
                                        felt_depressed_02 = HD110, felt_depressed_04 = JD110,
                                        felt_depressed_06 = KD110, felt_depressed_08 = LD110,
                                        felt_depressed_10 = MD110, felt_depressed_12 = ND110,
                                        felt_depressed_14 = OD110)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("felt_depressed")), function(x) ifelse(x == 1, 1, 0))


hearing_data <- hearing_data %>% rename(effort_00 = G1670,
                                        effort_02 = HD111, effort_04 = JD111,
                                        effort_06 = KD111, effort_08 = LD111,
                                        effort_10 = MD111, effort_12 = ND111,
                                        effort_14 = OD111)


hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("effort")), function(x) ifelse(x == 1, 1, 0))


hearing_data <- hearing_data %>% rename(sleep_restless_00 = G1671,
                                        sleep_restless_02 = HD112, sleep_restless_04 = JD112,
                                        sleep_restless_06 = KD112, sleep_restless_08 = LD112,
                                        sleep_restless_10 = MD112, sleep_restless_12 = ND112,
                                        sleep_restless_14 = OD112)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("sleep_restless")), function(x) ifelse(x == 1, 1, 0))


hearing_data <- hearing_data %>% rename(happy_00 = G1672,
                                        happy_02 = HD113, happy_04 = JD113,
                                        happy_06 = KD113, happy_08 = LD113,
                                        happy_10 = MD113, happy_12 = ND113,
                                        happy_14 = OD113)


hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("happy")), function(x) ifelse(x == 5, 1, 0))

hearing_data <- hearing_data %>% rename(lonely_00 = G1673,
                                        lonely_02 = HD114, lonely_04 = JD114,
                                        lonely_06 = KD114, lonely_08 = LD114,
                                        lonely_10 = MD114, lonely_12 = ND114,
                                        lonely_14 = OD114)
hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("lonely")), function(x) ifelse(x == 1, 1, 0))


hearing_data <- hearing_data %>% rename(enjoyed_life_00 = G1674,
                                        enjoyed_life_02 = HD115, enjoyed_life_04 = JD115,
                                        enjoyed_life_06 = KD115, enjoyed_life_08 = LD115,
                                        enjoyed_life_10 = MD115, enjoyed_life_12 = ND115,
                                        enjoyed_life_14 = OD115)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("enjoyed_life")), function(x) ifelse(x == 5, 1, 0))


hearing_data <- hearing_data %>% rename(felt_sad_00 = G1675,
                                        felt_sad_02 = HD116, felt_sad_04 = JD116,
                                        felt_sad_06 = KD116, felt_sad_08 = LD116,
                                        felt_sad_10 = MD116, felt_sad_12 = ND116,
                                        felt_sad_14 = OD116)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("felt_sad")), function(x) ifelse(x == 1, 1, 0))

hearing_data <- hearing_data %>% rename(not_going_00 = G1676,
                                        not_going_02 = HD117, not_going_04 = JD117,
                                        not_going_06 = KD117, not_going_08 = LD117,
                                        not_going_10 = MD117, not_going_12 = ND117,
                                        not_going_14 = OD117)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("not_going")), function(x) ifelse(x == 1, 1, 0))


# Age

hearing_data <- hearing_data %>% rename(age_00 = G1101, 
                                        age_02 = HA019,
                                        age_04 = JA019,
                                        age_06 = KA019,
                                        age_08 = LA019,
                                        age_10 = MA019,
                                        age_12 = NA019,
                                        age_14 = OA019)



# Gender

hearing_data <- hearing_data %>% rename(gender_00 = G490,
                                        gender_02 = HX060_R,
                                        gender_04 = JX060_R,
                                        gender_06 = KX060_R,
                                        gender_08 = LX060_R,
                                        gender_10 = MX060_R,
                                        gender_12 = NX060_R,
                                        gender_14 = OX060_R)
#New, time-invariant variable (gender):
hearing_data$gender <- NA

hearing_data$gender[hearing_data$gender_00 == 1 |
                    hearing_data$gender_02 == 1 |
                    hearing_data$gender_04 == 1 |
                    hearing_data$gender_06 == 1 |
                    hearing_data$gender_08 == 1 |
                    hearing_data$gender_10 == 1 |
                    hearing_data$gender_12 == 1 |
                    hearing_data$gender_14 == 1] <- 1 #Male
                      
hearing_data$gender[hearing_data$gender_00 == 2 |
                      hearing_data$gender_02 == 2 |
                      hearing_data$gender_04 == 2 |
                      hearing_data$gender_06 == 2 |
                      hearing_data$gender_08 == 2 |
                      hearing_data$gender_10 == 2 |
                      hearing_data$gender_12 == 2 |
                      hearing_data$gender_14 == 2] <- 0 #Female


# Race

hearing_data <- hearing_data %>% rename(race_00 = G1092A, race_02 = HB031A,
                                        race_04 = JB031A, race_06 = KB089M1M,
                                        race_08 = LB089M1M, race_10 = MB089M1M,
                                        race_12 = NB089M1M, race_14 = OB089M1M)


#New, time-invariant variable (race):
hearing_data$race <- NA

hearing_data$race[hearing_data$race_00 == 1 |
                      hearing_data$race_02 == 1 |
                      hearing_data$race_04 == 1|
                      hearing_data$race_06 == 1 |
                      hearing_data$race_08 == 1 |
                      hearing_data$race_10 == 1 |
                      hearing_data$race_12 == 1 |
                      hearing_data$race_14 == 1] <- "White"


hearing_data$race[hearing_data$race_00 == 2   |
                    hearing_data$race_02 == 2 |
                    hearing_data$race_04 == 2 |
                    hearing_data$race_06 == 2 |
                    hearing_data$race_08 == 2 |
                    hearing_data$race_10 == 2 |
                    hearing_data$race_12 == 2 |
                    hearing_data$race_14 == 2] <- "Black"


hearing_data$race[hearing_data$race_00 == 7 |
                    hearing_data$race_02 == 7 |
                    hearing_data$race_04 == 7 |
                    hearing_data$race_06 == 7 |
                    hearing_data$race_08 == 7 |
                    hearing_data$race_10 == 7 |
                    hearing_data$race_12 == 7 |
                    hearing_data$race_14 == 7] <- "Other"


# Hispanic

hearing_data <- hearing_data %>% rename(hispanic_00 = G1089A, 
                                        hispanic_02 = HB028A, hispanic_04 = JB028A, 
                                        hispanic_06 = KB028, hispanic_08 = LB028,
                                        hispanic_10 = MB028, hispanic_12 = NB028, 
                                        hispanic_14 = OB028)

#New, time-invariant variable (hispanic):
hearing_data$hispanic <- NA

hearing_data$hispanic[hearing_data$hispanic_00 == 1 |
                      hearing_data$hispanic_02 == 1 |
                      hearing_data$hispanic_04 == 1 |
                      hearing_data$hispanic_06 == 1 |
                      hearing_data$hispanic_08 == 1 |
                      hearing_data$hispanic_10 == 1 |
                      hearing_data$hispanic_12 == 1 |
                      hearing_data$hispanic_14 == 1] <- 1

hearing_data$hispanic[hearing_data$hispanic_00 == 5 |
                        hearing_data$hispanic_02 == 5 |
                        hearing_data$hispanic_04 == 5 |
                        hearing_data$hispanic_06 == 5 |
                        hearing_data$hispanic_08 == 5 |
                        hearing_data$hispanic_10 == 5 |
                        hearing_data$hispanic_12 == 5 |
                        hearing_data$hispanic_14 == 5] <- 0



# Cognitive Impairment


hearing_data <- hearing_data %>% rename(cog_function_98 = cogfunction1998,
                                        cog_function_00 = cogfunction2000,
                                        cog_function_02 = cogfunction2002,
                                        cog_function_04 = cogfunction2004,
                                        cog_function_06 = cogfunction2006,
                                        cog_function_08 = cogfunction2008,
                                        cog_function_10 = cogfunction2010,
                                        cog_function_12 = cogfunction2012,
                                        cog_function_14 = cogfunction2014,
                                        cogtot_98 = cogtot27_imp1998,
                                        cogtot_00 = cogtot27_imp2000,
                                        cogtot_02 = cogtot27_imp2002,
                                        cogtot_04 = cogtot27_imp2004,
                                        cogtot_06 = cogtot27_imp2006,
                                        cogtot_08 = cogtot27_imp2008,
                                        cogtot_10 = cogtot27_imp2010,
                                        cogtot_12 = cogtot27_imp2012,
                                        cogtot_14 = cogtot27_imp2014)


hearing_data$cog_function_04 <- ifelse(is.na(hearing_data$cog_function_04), hearing_data$cog_function_02, hearing_data$cog_function_04)
hearing_data$cog_function_04 <- ifelse(is.na(hearing_data$cog_function_04), hearing_data$cog_function_06, hearing_data$cog_function_04)



# Smoking Status


hearing_data <- hearing_data %>% rename(smoker_00 = G1400,
                                        smoker_02 = HC117, smoker_04 = JC117,
                                        smoker_06 = KC117, smoker_08 = LC117,
                                        smoker_10 = MC117, smoker_12 = NC117,
                                        smoker_14 = OC117,
                                        ever_smoke_04 = JZ205,
                                        ever_smoke_06 = KZ205,
                                        ever_smoke_08 = LZ205,
                                        ever_smoke_10 = MZ205,
                                        ever_smoke_12 = NZ205,
                                        ever_smoke_14 = OZ205)


#New variable (smooking status 2004):
hearing_data$smoke_04 <- NA

hearing_data$smoke_04[hearing_data$ever_smoke_04 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_04 == 1 & (hearing_data$smoker_04 == 5 | is.na(hearing_data$smoker_04))] <- 0
hearing_data$smoke_04[hearing_data$smoker_04 == 1] <- 1
hearing_data$smoke_04[hearing_data$smoker_04 == 5] <- 0
hearing_data$smoke_04[hearing_data$smoker_02 == 5] <- 0
hearing_data$smoke_04[hearing_data$smoker_00 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_06 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_08 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_10 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_12 == 5] <- 0
hearing_data$smoke_04[hearing_data$ever_smoke_14 == 5] <- 0


#New variable (smoking status 2006):
hearing_data$smoke_06 <- NA

hearing_data$smoke_06[hearing_data$ever_smoke_06 == 5] <- 0
hearing_data$smoke_06[hearing_data$ever_smoke_06 == 1 & (hearing_data$smoker_06 == 5 | is.na(hearing_data$smoker_06))] <- 0
hearing_data$smoke_06[hearing_data$smoker_06 == 1] <- 1
hearing_data$smoke_06[hearing_data$smoker_06 == 5] <- 0


#New variable (smoking status 2008)
hearing_data$smoke_08 <- NA

hearing_data$smoke_08[hearing_data$ever_smoke_08 == 5] <- 0
hearing_data$smoke_08[hearing_data$ever_smoke_08 == 1 & (hearing_data$smoker_08 == 5 | is.na(hearing_data$smoker_08))] <- 0
hearing_data$smoke_08[hearing_data$smoker_08 == 1] <- 1
hearing_data$smoke_08[hearing_data$smoker_08 == 5] <- 0


# Education

hearing_data <- hearing_data %>% rename(highest_education_00 =G1074A,
                                        highest_education_02 = HB014A,
                                        highest_education_04 = JB014A,
                                        highest_education_preload_06 = KZ216,
                                        highest_education_06 = KB014,
                                        highest_education_preload_08 = LZ216,
                                        highest_education_08 = LB014,
                                        highest_education_preload_10 = MZ216,
                                        highest_education_10 = MB014,
                                        highest_education_preload_12 = NZ216,
                                        highest_education_12 = NB014,
                                        highest_education_preload_14 = OZ216,
                                        highest_education_14 = OB014)

hearing_data <- hearing_data %>% 
  mutate_at(vars(starts_with("highest_education")), function(x) {
    case_when(
      x == 0 ~ "No Formal Education",
      between(x, 1, 11) ~ "Grade School",
      x == 12 ~ "High School",
      between(x,13,15) ~ "Some College",
      x == 16 ~ "College",
      x == 17 ~ "Post College"
    )
  })


# New, time-invariant variable (education):

hearing_data$education <- NA

hearing_data$education[hearing_data$highest_education_00 == "No Formal Education" | hearing_data$highest_education_02 == "No Formal Education" |
                         hearing_data$highest_education_04 == "No Formal Education" | hearing_data$highest_education_preload_06 == "No Formal Education" | 
                         hearing_data$highest_education_06 == "No Formal Education" |
                         hearing_data$highest_education_preload_08 == "No Formal Education" | hearing_data$highest_education_08 == "No Formal Education" |
                         hearing_data$highest_education_preload_10 == "No Formal Education" | hearing_data$highest_education_10 == "No Formal Education" |
                         hearing_data$highest_education_preload_12 == "No Formal Education" |
                         hearing_data$highest_education_12 == "No Formal Education" | hearing_data$highest_education_preload_14 == "No Formal Education"| 
                         hearing_data$highest_education_14 == "No Formal Education"] <- "No Formal Education"


hearing_data$education[hearing_data$highest_education_00 == "Grade School" | hearing_data$highest_education_02 == "Grade School" |
                         hearing_data$highest_education_04 == "Grade School" | hearing_data$highest_education_preload_06 == "Grade School" | 
                         hearing_data$highest_education_06 == "Grade School" |
                         hearing_data$highest_education_preload_08 == "Grade School" | hearing_data$highest_education_08 == "Grade School" |
                         hearing_data$highest_education_preload_10 == "Grade School" | hearing_data$highest_education_10 == "Grade School" |
                         hearing_data$highest_education_preload_12 == "Grade School" |
                         hearing_data$highest_education_12 == "Grade School" | hearing_data$highest_education_preload_14 == "Grade School"| 
                         hearing_data$highest_education_14 == "Grade School"] <- "Grade School"


hearing_data$education[hearing_data$highest_education_00 == "High School" | hearing_data$highest_education_02 == "High School" |
                         hearing_data$highest_education_04 == "High School" | hearing_data$highest_education_preload_06 == "High School" | 
                         hearing_data$highest_education_06 == "High School" |
                         hearing_data$highest_education_preload_08 == "High School" | hearing_data$highest_education_08 == "High School" |
                         hearing_data$highest_education_preload_10 == "High School" | hearing_data$highest_education_10 == "High School" |
                         hearing_data$highest_education_preload_12 == "High School" |
                         hearing_data$highest_education_12 == "High School" | hearing_data$highest_education_preload_14 == "High School"| 
                         hearing_data$highest_education_14 == "High School"] <- "High School"



hearing_data$education[hearing_data$highest_education_00 == "Some College" | hearing_data$highest_education_02 == "Some College" |
                         hearing_data$highest_education_04 == "Some College" | hearing_data$highest_education_preload_06 == "Some College" | 
                         hearing_data$highest_education_06 == "Some College" |
                         hearing_data$highest_education_preload_08 == "Some College" | hearing_data$highest_education_08 == "Some College" |
                         hearing_data$highest_education_preload_10 == "Some College" | hearing_data$highest_education_10 == "Some College" |
                         hearing_data$highest_education_preload_12 == "Some College" |
                         hearing_data$highest_education_12 == "Some College" | hearing_data$highest_education_preload_14 == "Some College"| 
                         hearing_data$highest_education_14 == "Some College"] <- "Some College"



hearing_data$education[hearing_data$highest_education_00 == "College" | hearing_data$highest_education_02 == "College" |
                         hearing_data$highest_education_04 == "College" | hearing_data$highest_education_preload_06 == "College" | 
                         hearing_data$highest_education_06 == "College" |
                         hearing_data$highest_education_preload_08 == "College" | hearing_data$highest_education_08 == "College" |
                         hearing_data$highest_education_preload_10 == "College" | hearing_data$highest_education_10 == "College" |
                         hearing_data$highest_education_preload_12 == "College" |
                         hearing_data$highest_education_12 == "College" | hearing_data$highest_education_preload_14 == "College"| 
                         hearing_data$highest_education_14 == "College"] <- "College"


hearing_data$education[hearing_data$highest_education_00 == "Post College" | hearing_data$highest_education_02 == "Post College" |
                         hearing_data$highest_education_04 == "Post College" | hearing_data$highest_education_preload_06 == "Post College" | 
                         hearing_data$highest_education_06 == "Post College" |
                         hearing_data$highest_education_preload_08 == "Post College" | hearing_data$highest_education_08 == "Post College" |
                         hearing_data$highest_education_preload_10 == "Post College" | hearing_data$highest_education_10 == "Post College" |
                         hearing_data$highest_education_preload_12 == "Post College" |
                         hearing_data$highest_education_12 == "Post College" | hearing_data$highest_education_preload_14 == "Post College"| 
                         hearing_data$highest_education_14 == "Post College"] <- "Post College"

###########################################################################################################

hearing_data <- hearing_data %>%
  rename(sr_heart_condition_14 = OC036, heart_med_14 = OC037, heart_doctor_14 = OC038,
         heart_failure_14 = OC263, angina_14 = OC260, heart_attack_14 = OC257, arrythmia_14 = OC266,
         heart_surgery_14 = OC052, heart_treatment_14 = OC051, heart_attack_sr_htn_14 = OC005, htn_med_14 = OC006,
         lung_disease_14 = OC030, lung_med_14 = OC032, 
         diabetes_14 = OC010, diabetes_oral_14 = OC011, diabetes_insulin_14 = OC012,
         cancer_14 = OC018, cancer_treat_14 = OC020, cancer_treat1_14 = OC021M1, 
         cancer_treat2_14 = OC021M2, cancer_treat3_14 = OC021M3, 
         cancer_treat4_14 = OC021M4, cancer_treat5_14 = OC021M5, cancer_treat6_14 = OC021M6, 
         cancer_treat7_14 = OC021M7, stroke_14 = OC053, stroke_prob_14 = OC055,
         stroke_med_14 = OC060, syst_1_14 = OI859, syst_2_14 = OI864, syst_03_14 = OI869,
         diast_1_14 = OI860, diast_02_14 = OI865, diast_03_14 = OI870, dementia_14 = OC273,
         alzheimers_14 = OC272, memory_drug_14 = OC210, cancer_year_14 = OC028, arthritis_14 = OC070) %>%
  
  rename(sr_heart_condition_12 = NC036, heart_med_12 = NC037, heart_doctor_12 = NC038,
         heart_failure_12 = NC263, angina_12 = NC260, heart_attack_12 = NC257, 
         heart_surgery_12 = NC052, arrythmia_12 = NC266, heart_treatment_12 = NC051,
         sr_htn_12 = NC005, htn_med_12 = NC006,
         lung_disease_12 = NC030, lung_med_12 = NC032, 
         diabetes_12 = NC010, diabetes_oral_12 = NC011, diabetes_insulin_12 = NC012,
         cancer_12 = NC018, cancer_treat_12 = NC020, cancer_treat1_12 = NC021M1, 
         cancer_treat2_12 = NC021M2, cancer_treat3_12 = NC021M3, 
         cancer_treat4_12 = NC021M4, cancer_treat5_12 = NC021M5, cancer_treat6_12 = NC021M6, 
         stroke_12 = NC053, stroke_prob_12 = NC055,
         stroke_med_12 = NC060, syst_1_12 = NI859, syst_2_12 = NI864, syst_03_12 = NI869,
         diast_1_12 = NI860, diast_02_12 = NI865, diast_03_12 = NI870, 
         cancer_year_12 = NC028, dementia_12 = NC273, alzheimers_12 = NC272, 
         memory_drug_12 = NC210, arthritis_12 = NC070, arthritis_trt_12 = NC074) %>%
  
  rename(sr_heart_condition_10 = MC036, heart_med_10 = MC037, heart_doctor_10 = MC038,
         heart_failure_10 = MC263, angina_10 = MC260, heart_attack_10 = MC257,
         heart_surgery_10 = MC052, arrythmia_10 = MC266, heart_treatment_10 = MC051,
         sr_htn_10 = MC005, htn_med_10 = MC006,
         lung_disease_10 = MC030, lung_med_10 = MC032, 
         diabetes_10 = MC010, diabetes_oral_10 = MC011, diabetes_insulin_10 = MC012,
         cancer_10 = MC018, cancer_treat_10 = MC020, cancer_treat1_10 = MC021M1, 
         cancer_treat2_10 = MC021M2, cancer_treat3_10 = MC021M3, 
         cancer_treat4_10 = MC021M4, cancer_treat5_10 = MC021M5, cancer_treat6_10 = MC021M6, 
         stroke_10 = MC053, stroke_prob_10 = MC055,
         stroke_med_10 = MC060, syst_1_10 = MI859, syst_2_10 = MI864, syst_03_10 = MI869,
         diast_1_10 = MI860, diast_02_10 = MI865, diast_03_10 = MI870, 
         cancer_year_10 = MC028, dementia_10 = MC273, alzheimers_10 = MC272, memory_drug_10 = MC210,
         arthritis_10 = MC070, arthritis_trt_10 = MC074) %>%
  
  rename(sr_heart_condition_08 = LC036, heart_med_08 = LC037, heart_doctor_08 = LC038,
         heart_failure_08 = LC048, angina_08 = LC045, heart_attack_08 = LC040,
         heart_surgery_08 = LC052, heart_treatment_08 = LC051, sr_htn_08 = LC005, htn_med_08 = LC006,
         lung_disease_08 = LC030, lung_med_08 = LC032, 
         diabetes_08 = LC010, diabetes_oral_08 = LC011, diabetes_insulin_08 = LC012,
         cancer_08 = LC018, cancer_treat_08 = LC020, cancer_treat1_08 = LC021M1, 
         cancer_treat2_08 = LC021M2, cancer_treat3_08 = LC021M3, 
         cancer_treat4_08 = LC021M4, stroke_08 = LC053, stroke_prob_08 = LC055,
         stroke_med_08 = LC060, syst_1_08 = LI859, syst_2_08 = LI864, syst_03_08 = LI869,
         diast_1_08 = LI860, diast_02_08 = LI865, diast_03_08 = LI870, 
         cancer_year_08 = LC028, dementia_08 = LC069, memory_drug_08 = LC210,
         arthritis_08 = LC070, arthritis_trt_08 = LC074) %>%
  
  rename(sr_heart_condition_06 = KC036, heart_med_06 = KC037, heart_doctor_06 = KC038,
         heart_failure_06 = KC048, angina_06 = KC045, heart_attack_06 = KC040,
         heart_surgery_06 = KC052, heart_treatment_06 = KC051, sr_htn_06 = KC005, htn_med_06 = KC006,
         lung_disease_06 = KC030, lung_med_06 = KC032, 
         diabetes_06 = KC010, diabetes_oral_06 = KC011, diabetes_insulin_06 = KC012,
         cancer_06 = KC018, cancer_treat_06 = KC020, cancer_treat1_06 = KC021M1, 
         cancer_treat2_06 = KC021M2, cancer_treat3_06 = KC021M3, 
         cancer_treat4_06 = KC021M4, cancer_treat5_06 = KC021M5, cancer_treat6_06 = KC021M6, 
         cancer_year_06 = KC028, stroke_06 = KC053, stroke_prob_06 = KC055,
         stroke_med_06 = KC060, syst_1_06 = KI859, syst_2_06 = KI864, syst_03_06 = KI869,
         diast_1_06 = KI860, diast_02_06 = KI865, diast_03_06 = KI870, dementia_06 = KC069,
         arthritis_06 = KC070, arthritis_trt_06 = KC074) %>%
  
  rename(sr_heart_condition_04 = JC036, heart_med_04 = JC037, heart_doctor_04 = JC038,
         heart_failure_04 = JC048, angina_04 = JC045, heart_attack_04 = JC040,
         heart_surgery_04 = JC052, heart_treatment_04 = JC051, sr_htn_04 = JC005, htn_med_04 = JC006,
         lung_disease_04 = JC030, lung_med_04 = JC032, 
         diabetes_04 = JC010, diabetes_oral_04 = JC011, diabetes_insulin_04 = JC012,
         cancer_04 = JC018, cancer_treat_04 = JC020, cancer_treat1_04 = JC021M1, 
         cancer_treat2_04 = JC021M2, cancer_treat3_04 = JC021M3, 
         cancer_treat4_04 = JC021M4, cancer_year_04 = JC028, stroke_04 = JC053, stroke_prob_04 = JC055,
         stroke_med_04 = JC060, dementia_04 = JC069, arthritis_04 = JC070,
         arthritis_trt_04 = JC074) %>%
  
  rename(sr_heart_condition_02 = HC036, heart_med_02 = HC037, heart_doctor_02 = HC038,
         heart_failure_02 = HC048, angina_02 = HC045, heart_attack_02 = HC040,
         heart_surgery_02 = HC052, heart_treatment_02 = HC051, sr_htn_02 = HC005, htn_med_02 = HC006,
         lung_disease_02 = HC030, lung_med_02 = HC032, 
         diabetes_02 = HC010, diabetes_oral_02 = HC011, diabetes_insulin_02 = HC012,
         cancer_02 = HC018, cancer_treat_02 = HC020, cancer_treat1_02 = HC021M1, 
         cancer_treat2_02 = HC021M2, cancer_treat3_02 = HC021M3, 
         cancer_treat4_02 = HC021M4, cancer_treat5_02 = HC021M5, 
         cancer_year_02 = HC028, stroke_02 = HC053, stroke_prob_02 = HC055,
         stroke_med_02 = HC060, dementia_02 = HC069, arthritis_02 = HC070,
         arthritis_trt_02 = HC074) %>%
  
  rename(sr_heart_condition_00 = G1289, heart_med_00 = G1290, heart_doctor_00 = G1291,
         heart_failure_00 = G1304, angina_00 = G1301, heart_treatment_00 = G1307,
         heart_attack_00 = G1295,
         heart_surgery_00 = G1308, sr_htn_00 = G1238, htn_med_00 = G1239,
         lung_disease_00 = G1279, lung_med_00 = G1284, 
         diabetes_00 = G1245, diabetes_oral_00 = G1248, diabetes_insulin_00 = G1249,
         cancer_00 = G1262, cancer_treat_00 = G1264, cancer_treat1_00 = G1265M1,
         cancer_treat2_00 = G1265M2, cancer_treat3_00 = G1265M3, 
         cancer_treat_4_00 = G1265M4, cancer_year_00 = G1274, stroke_00 = G1309, stroke_prob_00 = G1312,
         stroke_med_00 = G1317, dementia_00 = G1326, arthritis_00 = G1327,
         arthritis_trt_00 = G1331) 

hearing_data_subset <- hearing_data %>% dplyr::select(HHID, PN, ends_with("_98"), ends_with("_00"),
                                        ends_with("_02"), ends_with("_04"), ends_with("_06"), ends_with("_08"), ends_with("_10"),
                                        ends_with("_12"), ends_with("_14"), gender, race, hispanic, education)

hearing_data_subset <- hearing_data_subset %>% dplyr::select(-starts_with("NB"), -starts_with("OB"), -starts_with("MB"), -starts_with("LB"))

##ADJ
