#Required Packages
library(imputeTS)
library(ggpubr)
library(dplyr)
library(car)
library(kableExtra)
library(knitr)
library(apaTables)
library(car)

load("C:/Users/ss12293/Box Sync/Hearing Depression Project/hearing_data_subset.Rdata")

#Exclude anyone who wears a hearing aid.
Q1_data <- hearing_data_subset %>% filter(hearing_aid_04 == 5 & hearing_aid_PL_06 == 5 & hearing_04 != 8 & hearing_04 != 9)

#Mean-Item Imputation (any NAs are replaced with the mean)
Q1_data$felt_depressed_04 <- na_mean(Q1_data$felt_depressed_04)
Q1_data$effort_04 <- na_mean(Q1_data$effort_04)
Q1_data$sleep_restless_04 <- na_mean(Q1_data$sleep_restless_04)
Q1_data$happy_04 <- na_mean(Q1_data$happy_04)
Q1_data$lonely_04 <- na_mean(Q1_data$lonely_04)
Q1_data$enjoyed_life_04 <- na_mean(Q1_data$enjoyed_life_04)
Q1_data$felt_sad_04 <- na_mean(Q1_data$felt_sad_04)
Q1_data$not_going_04 <- na_mean(Q1_data$not_going_04)


#CES-D score is the sum of these 8 items. 
Q1_data <- Q1_data %>% rowwise() %>% mutate(CESD_04 = sum(felt_depressed_04,effort_04, sleep_restless_04,
                                                                            happy_04, lonely_04, enjoyed_life_04,
                                                                            felt_sad_04, not_going_04))

#Self-reported hearing rating is a factor variable.
Q1_data$hearing_04 <- as.factor(Q1_data$hearing_04)
levels(Q1_data$hearing_04) <- c("1" = "Excellent", "2" = "Very Good", "3" = "Good", "4" = "Fair", "5" = "Poor")
levels(Q1_data$hearing_04)


#Race
Q1_data$race <- as.factor(Q1_data$race)

#Cognitive Impairment (Yes/No)
Q1_data$cogImp_04 <- NA
Q1_data$cogImp_04[Q1_data$cog_function_04 == 1] <- 0 #No 
Q1_data$cogImp_04[Q1_data$cog_function_04 == 2 | Q1_data$cog_function_04 == 3] <- 1 #Yes 



#######################Summary Statistics


#Mean CESD per hearing level
hearing_level_CESD_table <- group_by(Q1_data, hearing_04) %>%
  summarise(
    count = n(),
    mean = mean(CESD_04),
    sd = sd(CESD_04)
  )

hearing_level_CESD_table %>% kable(col.names = c("", "count", "mean", "sd"), caption = "Rate Hearing 2004") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)


leveneTest(CESD_04 ~ hearing_04, data = Q1_data) #variances not equal, therefore cannot use ANOVA

#Use Kruskal Test instead of ANOVA
kruskal.test(CESD_04 ~ hearing_04, data = Q1_data)

#Pairwise Wilcoxon to determine which pairs are significant 
pairwise.wilcox.test(Q1_data$CESD_04, Q1_data$hearing_04, p.adjust.method = "BH")


ggboxplot(Q1_data, x = "hearing_04", y = "CESD_04", 
          color = "hearing_04")


summary(Q1_data$CESD_04)


#Mean CESD per hearing prob or not 
hearing_prob_CESD_table <- group_by(Q1_data, hearing_prob_04) %>%
  summarise(
    count = n(),
    mean = mean(CESD_04),
    sd = sd(CESD_04)
  )



hearing_prob_CESD_table %>% kable(col.names = c("", "count", "mean", "sd"),
                                  caption = "Hearing Impairment (2004)") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)



ggboxplot(Q1_data, x = "hearing_prob_04", y = "CESD_04", 
          color = "hearing_prob_04", palette = c("#00AFBB", "#E7B800"),
          ylab = "CESD Score", xlab = "Hearing Impairment")


t.test(CESD_04 ~ hearing_prob_04, data = Q1_data, var.equal = FALSE, alternative = "less")



#Covariates: age, sex, race, ethnicity, cognitive impairment, stroke, smoking status, hypertension, diabetes

Q1_data_subset <- Q1_data %>% dplyr::select(HHID, PN, hearing_04, age_04, gender, race, hispanic,
                                cogImp_04, STROKE_04, smoke_04, HTN_04, DIABETES_04, CESD_04)

Q1_complete_cases <- Q1_data_subset[complete.cases(Q1_data_subset), ]

model_q1 <- lm(CESD_04 ~ hearing_04 + age_04 + gender + race + hispanic + cogImp_04 +
             smoke_04, data = Q1_complete_cases)
apa.reg.table(model_q1, filename = "q1_model.doc")
