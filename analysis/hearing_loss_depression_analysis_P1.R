# Required Packages:
library(imputeTS)
library(ggpubr)
library(dplyr)
library(car)
library(kableExtra)
library(knitr)
library(apaTables)
library(car)

load("C:/Users/ss12293/Box Sync/Hearing Depression Project/hearing_data_subset.Rdata") #change to working directory

# The 2004 wave of HRS data will be used, as the sample size for this year is largest. 

# Exclude anyone who wears a hearing aid in 2004.
Q1_data <- hearing_data_subset %>% filter(hearing_aid_04 == 5 & hearing_aid_PL_06 == 5 & hearing_04 != 8 & hearing_04 != 9)

# Mean-Item Imputation (any NAs are replaced with the mean of that column)
Q1_data$felt_depressed_04 <- na_mean(Q1_data$felt_depressed_04)
Q1_data$effort_04 <- na_mean(Q1_data$effort_04)
Q1_data$sleep_restless_04 <- na_mean(Q1_data$sleep_restless_04)
Q1_data$happy_04 <- na_mean(Q1_data$happy_04)
Q1_data$lonely_04 <- na_mean(Q1_data$lonely_04)
Q1_data$enjoyed_life_04 <- na_mean(Q1_data$enjoyed_life_04)
Q1_data$felt_sad_04 <- na_mean(Q1_data$felt_sad_04)
Q1_data$not_going_04 <- na_mean(Q1_data$not_going_04)


# CES-D score is the sum of the 8 items from above. 
Q1_data <- Q1_data %>% rowwise() %>% mutate(CESD_04 = sum(felt_depressed_04,effort_04, sleep_restless_04,
                                                                            happy_04, lonely_04, enjoyed_life_04,
                                                                            felt_sad_04, not_going_04))

# Self-reported hearing rating is changed to a categorical variable. 
Q1_data$hearing_04 <- as.factor(Q1_data$hearing_04)
levels(Q1_data$hearing_04) <- c("1" = "Excellent", "2" = "Very Good", "3" = "Good", "4" = "Fair", "5" = "Poor")


#Race (a covariate) is changed to a categorical variable. 
Q1_data$race <- as.factor(Q1_data$race)

#Cognitive Impairment (another covariate) (Yes/No)
Q1_data$cogImp_04 <- NA
Q1_data$cogImp_04[Q1_data$cog_function_04 == 1] <- 0 #No 
Q1_data$cogImp_04[Q1_data$cog_function_04 == 2 | Q1_data$cog_function_04 == 3] <- 1 #Yes 



######################################### Summary Statistics

# CES-D Summary Statistics per hearing level
hearing_level_CESD_table <- group_by(Q1_data, hearing_04) %>%
  summarise(
    count = n(),
    mean = mean(CESD_04),
    sd = sd(CESD_04)
  )

hearing_level_CESD_table %>% kable(col.names = c("", "count", "CESD:mean", "CESD:sd"), caption = "Hearing Distribution (2004)") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Boxplot to show distribution of CES-D scores for each hearing rating
ggplot(Q1_data, aes(x=as.factor(hearing_04), y=CESD_04, fill = as.factor(hearing_04))) + 
  geom_boxplot(notch = F) + theme(legend.position="none")  +
  labs(y = "CES-D Score", x = "Self-Reported Hearing Rating") + 
scale_x_discrete(limits = c("Excellent","Very Good","Good","Fair","Poor"), 
                 labels = c("Excellent (1)", "Very Good (2)", "Good (3)", "Fair (4)", "Poor (5)"))

# Use a Levene test to check for equality of variances:
leveneTest(CESD_04 ~ hearing_04, data = Q1_data) #variances not equal, therefore cannot use ANOVA

# Use Kruskal Test instead of ANOVA:
kruskal.test(CESD_04 ~ hearing_04, data = Q1_data)

# Use Pairwise Wilcoxon Rank Sum test to determine which pairs are significant:
pairwise.wilcox.test(Q1_data$CESD_04, Q1_data$hearing_04, p.adjust.method = "BH")

##############################################################

# A score of 4 or 5 implies hearing impairment. 

hearing_prob_CESD_table <- group_by(Q1_data, hearing_prob_04) %>%
  summarise(
    count = n(),
    mean = mean(CESD_04),
    sd = sd(CESD_04)
  )

#CES-D Scores per hearing impairment or not
hearing_prob_CESD_table %>% kable(col.names = c("", "count", "mean", "sd"),
                                  caption = "Hearing Impairment (2004)") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)


# Boxplot to show distribution of 2004 CES-D scores for hearing impairment or not:

ggplot(Q1_data, aes(x=as.factor(hearing_prob_04), y=CESD_04, fill = as.factor(hearing_prob_04))) + 
  geom_boxplot() + theme(legend.position="none")  + scale_x_discrete(limits=c("0","1"), labels = c("No", "Yes"))+
  labs(y = "CES-D Score", x = "Hearing Impairment") + scale_fill_manual(values=c("#00AFBB", "#E7B800"))

# T-test to test for the hypothesis that the mean CES-D score among those who are not hearing impaired is lower than the mean CES-D score among those who are hearing impaired.

t.test(CESD_04 ~ hearing_prob_04, data = Q1_data, var.equal = FALSE, alternative = "less")

######################################### Addition of Covariates

#Select covariates: age, sex, race, ethnicity, cognitive impairment, stroke, smoking status, hypertension, diabetes

Q1_data_subset <- Q1_data %>% dplyr::select(HHID, PN, hearing_04, age_04, gender, race, hispanic,
                                cogImp_04, STROKE_04, smoke_04, HTN_04, DIABETES_04, CESD_04)

# Only include rows in which no missing values for any of the columns: 

Q1_complete_cases <- Q1_data_subset[complete.cases(Q1_data_subset), ]

# Linear regression to check for relationship:

model_q1 <- lm(CESD_04 ~ hearing_04 + age_04 + gender + race + hispanic + cogImp_04 +
             smoke_04, data = Q1_complete_cases)
apa.reg.table(model_q1, filename = "q1_model.doc")
