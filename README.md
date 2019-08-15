# hearing-loss-depression-data


A medical student from NYU was interested in determining the effects of quality of hearing on depression levels among the geriatric population, as well as the long-term benefits of hearing aids on depression levels among those experiencing hearing impairment. I was given the task of doing the statistical analyses required to answer this question. 
All of the data used for this analysis originates from the Health & Retirement Study (HRS), which can be downloaded via https://ssl.isr.umich.edu/hrs/files2.php (registration is required). 

The Health and Retirement Study is a longitudinal project sponsored by the National Institute on Aging which, every two years, surveys a representative sample of approximately 20,000 people of age 50 and over in the US. Questions asked provide information on characteristics such as Demographics, Physical Health, Cognition, and Income & Assets.

The final dataset required to run the code is an R data file, titled 'hearing_data_subset', which can be found under ‘data’. 

Hearing level is quantified in HRS by asking respondents to self-report their quality of hearing on a five-point scale: Excellent (1), Very Good (2), Good (3), Fair (4), and Poor (5), where a rating of 4 or 5 constitutes hearing impairment. HRS also asks respondents if they ever wear a hearing aid.  
Depression scores are quantified using the Center for Epidemiologic Studies Depression (CES-D) scale, which is computed using the answers to eight questions from the Cognition section of HRS. Mean-item imputation was used to replace any missing values from each of the 8 items. 

The first part of the analysis focused on the association between depression and hearing quality of any individual not suffering from hearing loss, while the second part of the analysis focused on the effects, if any, of wearing hearing aids on depression levels.

Due to the longitudinal nature of HRS data, a cohort difference-in-differences design was used to answer the second part; the treatment group comprised of individuals who did not wear a hearing aid in the first wave, began wearing a hearing aid in the second wave, and continued to do so for the third wave, while the control group was made up of individuals who suffered from hearing loss but did not wear a hearing aid in any of the three waves. Since this was a cohort study, it was important that individuals were survey respondents in ALL three waves (some participants were lost to follow up throughout the years). 

The code required to run the analysis for the first part is 'hearing_depression_P1.R', and the code required to run the analysis for the second part is 'hearing_depression_P2.R', both of which can be found in 'analysis'. 
