
#apron exploratory

# apron_questions <-
#   
# SURVEY %>% 
#   select ( starts_with ("LEAD_APRONS")) %>% 
#   
#   mutate_at( vars(1), funs(case_when( is.na(.) ~ NA_real_,
#                                       . == 'Y' ~ 2,
#                                       . == 'N' ~ 1,
#                                       TRUE ~ 0))) %>% 
#              
#   mutate_at( vars(-1), funs(case_when( is.na(.) ~ NA_real_,
#                                       . == 'Y' ~ 1,
#                                       TRUE ~ 0))) 
# 
# alpha( apron_questions[, c(6, 8:9)], check.keys = T)





median(SURVEY$HEIGHT, na.rm=T)

histogram( SURVEY$HEIGHT2)


library(psych) 
library(GPArotation)



FACTOR_BASE_RIGOROUS <-
  
  SURVEY %>%
  mutate(
          BIRTHSEX = relevel(BIRTHSEX, ref='M'),
          
          HTCAT = recode_factor(HEIGHT2, "< 5'"='0', 
                                "5-5'3" = '0',
                                "5'4-5'6" = '0',
                                "5'7-5'9" = '1',
                                "5'10-6'" = '1',
                                "6'1-6'4" = '1',
                                "> 6'4" = '1'),
          
          HTCAT= relevel(HTCAT, ref='1'),
          
       HTCAT2 = as.numeric(HEIGHT2)) %>% 

      group_by( BIRTHSEX ) %>%
    
        mutate( MEDIAN_HT_SXADJ = median( HTCAT2, na.rm = T),
                HTCATSEXADJ =  case_when ( is.na(HTCAT2) ~ NA_real_,
                                           HTCAT2 < MEDIAN_HT_SXADJ ~ 1,
                                           HTCAT2 == MEDIAN_HT_SXADJ ~ 2,
                                           TRUE ~ 3),
                HTCATSEXADJ = factor(HTCATSEXADJ),
                HTCATSEXADJ = relevel(recode_factor(HTCATSEXADJ, '1'='SHORT', '2'='AVG', '3'='TALL'), ref='TALL')) %>% 
  
        ungroup() %>%
  
  
  mutate(   TEACHERSF = case_when (is.na(FEMALE_TRAINERS) ~ NA_real_,
                                          FEMALE_TRAINERS == '1-2' ~ 1,
                                          FEMALE_TRAINERS == '3-5' ~ 2,
                                          FEMALE_TRAINERS == '6-10' ~ 3,
                                          FEMALE_TRAINERS == '> 10' ~ 4,
                                          TRUE ~ 0 ),
              
              TEACHERSM = case_when (is.na(MALE_TRAINERS) ~ NA_real_,
                                          MALE_TRAINERS == '1-2' ~ 1,
                                        MALE_TRAINERS == '3-5' ~ 2,
                                          MALE_TRAINERS == '6-10' ~ 3,
                                          MALE_TRAINERS == '> 10' ~ 4,
                                          TRUE ~ 0 ),
            
              FMTEACH_RATIO = case_when ( is.na(TEACHERSF) | is.na(TEACHERSM)   ~ NA_real_,
                                          TRUE ~ round(TEACHERSF / TEACHERSM, 2))) %>% 

  
  
  
          
 mutate(    TRAINING_LEVEL = factor(as.numeric(TRAINING_LEVEL)),
            TRAINING_LEVEL = relevel(recode_factor(TRAINING_LEVEL, '1' = 'FIRSTYR', '2'= 'SECONDYR', .default= 'ADVANCED'),ref= 'ADVANCED'),

  #  HEIGHTBAND = as.numeric(HTCAT),
    
    PAIN_INJURY =  case_when(  is.na(EVER_INJURED) ~ NA_real_,
                               EVER_INJURED == "Y" ~ 1,
                               TRUE ~ 0),
    
    PAIN_HAND =  case_when(  is.na(EXPERIENCED_TRANSIENT_PAIN_HAND) ~ NA_real_,
                             EXPERIENCED_TRANSIENT_PAIN_HAND == "Y" ~ 1,
                             TRUE ~ 0),        
    PAIN_NECKSH =  case_when(  is.na(EXPERIENCED_TRANSIENT_PAIN_NECK_SHOULDER) ~ NA_real_,
                               EXPERIENCED_TRANSIENT_PAIN_NECK_SHOULDER == "Y" ~ 1,
                               TRUE ~ 0),        
    PAIN_BACK =  case_when(  is.na(EXPERIENCED_TRANSIENT_PAIN_BACK) ~ NA_real_,
                             EXPERIENCED_TRANSIENT_PAIN_BACK == "Y" ~ 1,
                             TRUE ~ 0),        
    PAIN_LEG =  case_when(  is.na(EXPERIENCED_TRANSIENT_PAIN_LEG) ~ NA_real_,
                            EXPERIENCED_TRANSIENT_PAIN_LEG == "Y" ~ 1,
                            TRUE ~ 0),        
    PAIN_FOOT = case_when(  is.na(EXPERIENCED_TRANSIENT_PAIN_FOOT) ~ NA_real_,
                            EXPERIENCED_TRANSIENT_PAIN_FOOT == "Y" ~ 1,
                            TRUE ~ 0),
    
    
   # DEMO_HOURS = case_when (is.na(PERFORMANCE_HOURS) ~ NA_real_,
   #                          PERFORMANCE_HOURS == '< 10' ~ 1,
   #                          PERFORMANCE_HOURS == '10-20' ~ 2,
   #                          PERFORMANCE_HOURS == '21-30' ~ 3,
   #                          PERFORMANCE_HOURS == '31-40' ~ 4,
   #                          TRUE ~ 5 ),



    # DEMO_SGPREF = case_when (is.na(TEACHER_GENDER_PREFERENCE) ~ NA_real_,
    #                         TEACHER_GENDER_PREFERENCE == 'Yes' ~ 1,
    #                         TRUE ~ 0),

    
    
    
    
    
    #Higher numbers equal higher levels of training/attention
    
    TRNG_FORMAL = case_when (is.na(FELLOWSHIP_FORMAL_ERGO_TRAINING) ~ NA_real_,
                             FELLOWSHIP_FORMAL_ERGO_TRAINING == 'Y' ~ 1,
                             TRUE ~ 0 ),
    
    
    TRNG_INFORMAL = case_when (is.na(INFORMAL_TRAINING) ~ NA_real_,
                               INFORMAL_TRAINING == 'Y' ~ 1,
                               TRUE ~ 0 ),
    
    
    TRNG_POSTURAL = case_when (is.na(TRAINING_TECHNIQUES_POSTURAL) ~ NA_real_,
                               TRAINING_TECHNIQUES_POSTURAL == "Y" ~ 1,
                               TRUE ~ 0),
    
    TRNG_BEDHT = case_when (is.na(TRAINING_TECHNIQUES_BEDHEIGHT) ~ NA_real_,
                            TRAINING_TECHNIQUES_BEDHEIGHT == "Y" ~ 1,
                            TRUE ~ 0),
    
    TRNG_BEDANG = case_when (is.na(TRAINING_TECHNIQUES_BEDANGLE) ~ NA_real_,
                             TRAINING_TECHNIQUES_BEDANGLE == "Y" ~ 1,
                             TRUE ~ 0),
    
    TRNG_MONITOR = case_when (is.na(TRAINING_TECHNIQUES_MONITORHEIGHT) ~ NA_real_,
                              TRAINING_TECHNIQUES_MONITORHEIGHT == "Y" ~ 1,
                              TRUE ~ 0),
    
    TRNG_MANEUVERS = case_when (is.na(TRAINING_TECHNIQUES_MUSCULOSKELETAL) ~ NA_real_,
                                TRAINING_TECHNIQUES_MUSCULOSKELETAL == "Y" ~ 1,
                                TRUE ~ 0) ,
    
    TRNG_EXERCISE = case_when (is.na(TRAINING_TECHNIQUES_EXERCISE_STRETCHING) ~ NA_real_,
                               TRAINING_TECHNIQUES_EXERCISE_STRETCHING == "Y" ~ 1,
                               TRUE ~ 0),
    
    TRNG_DIALEXT= case_when (is.na(TRAINING_TECHNIQUES_DIAL_EXTENDERS) ~ NA_real_,
                             TRAINING_TECHNIQUES_DIAL_EXTENDERS == "Y" ~ 1,
                             TRUE ~ 0),
    
    TRNG_PEDISCOPE= case_when (is.na(TRAINING_TECHNIQUES_PEDIATRIC_COLONOSCOPE) ~ NA_real_,
                               TRAINING_TECHNIQUES_PEDIATRIC_COLONOSCOPE == "Y" ~ 1,
                               TRUE ~ 0),
    
    TRNG_FEEDBACK= case_when (is.na(ERGO_FEEDBACK) ~ NA_real_,
                              ERGO_FEEDBACK == "Often" ~ 3,
                              ERGO_FEEDBACK == "Sometimes" ~ 2,
                              ERGO_FEEDBACK == "Rarely" ~ 1,
                              TRUE ~ 0),
  
   TRNG_FEEDBACKSX = case_when( is.na(ERGO_FEEDBACK_BY_WHOM) ~ NA_real_,
                                ERGO_FEEDBACK_BY_WHOM == 'Both equally' ~ 2,
                                ERGO_FEEDBACK_BY_WHOM %in% c('Mostly female teachers','Mostly male teachers') ~ 1,
                                TRUE ~ 0 ),
                                                 
    TRNG_OPTIM= case_when (is.na(ERGO_OPTIMIZATION) ~ NA_real_,
                           ERGO_OPTIMIZATION == "Y" ~ 2,
                           ERGO_OPTIMIZATION == "N" ~ 1,
                           TRUE ~ 0),
   
    
    TRNG_BUDGET= case_when (is.na(ERGO_TRAINING_BUDGET) ~ NA_real_,
                            ERGO_TRAINING_BUDGET == "Y" ~ 2,
                            ERGO_TRAINING_BUDGET == "N" ~ 1,
                           TRUE ~ 0),
    
    
    #Higher numbers equal heightened equipment availability
    
    EQUIP_GLOVE_AVAIL = case_when (is.na(GLOVE_SIZE_AVAILABLE) ~ NA_real_,
                                   GLOVE_SIZE_AVAILABLE == "Y" ~ 1,
                                   TRUE ~ 0),
    
    EQUIP_DIALEXT_AVAIL= case_when (is.na(DIAL_EXTENDERS_AVAILABLE) ~ NA_real_,
                                    DIAL_EXTENDERS_AVAILABLE == "Y" ~ 1,
                                    TRUE ~ 0),
    
    
    EQUIP_DIALEXT_ENC= case_when (is.na(DIAL_EXTENDERS_ENCOURAGED) ~ NA_real_,
                                  DIAL_EXTENDERS_ENCOURAGED == "Y" ~ 2,
                                  DIAL_EXTENDERS_ENCOURAGED == "N" ~ 1,
                                  TRUE ~ 0),
  
  
  # EQUIP_DIALEXT_USEF= case_when (is.na(DIAL_EXTENDERS_FEMALEATT) ~ NA_real_,
  #                               DIAL_EXTENDERS_FEMALEATT == "Very likely" ~ 2,
  #                               DIAL_EXTENDERS_FEMALEATT == "Somewhat likely" ~ 1,
  #                               DIAL_EXTENDERS_FEMALEATT == "Not likely" ~ 1,
  #                               TRUE ~ 0),
  # 
  #   EQUIP_DIALEXT_USEM= case_when (is.na(DIAL_EXTENDERS_MALEATT) ~ NA_real_,
  #                                  DIAL_EXTENDERS_MALEATT == "Very likely" ~ 2,
  #                                  DIAL_EXTENDERS_MALEATT == "Somewhat likely" ~ 1,
  #                                  DIAL_EXTENDERS_MALEATT == "Not likely" ~ 1,
  #                                  TRUE ~ 0),
    
    EQUIP_PEDISCOPE_AVAIL = case_when (is.na(PEDI_COLONOSCOPES_AVAILABLE) ~ NA_real_,
                                      PEDI_COLONOSCOPES_AVAILABLE == "Y" ~ 1,
                                      TRUE ~ 0),
    
    EQUIP_LAPRON_AVAIL = case_when (is.na(LEAD_APRONS_AVAILABLE) ~ NA_real_,
                                    LEAD_APRONS_AVAILABLE == "Y" ~ 2,
                                    LEAD_APRONS_AVAILABLE == "N" ~ 1,
                                    TRUE ~ 0),
    
   
   
   EQUIP_LAPRON_LW1P = case_when (is.na(LEAD_APRONS_LW_ONEPIECE) ~ NA_real_,
                                  LEAD_APRONS_LW_ONEPIECE == "Y" ~ 1,
                                   TRUE ~ 0),
   EQUIP_LAPRON_LW2P = case_when (is.na(LEAD_APRONS_LW_TWOPIECE) ~ NA_real_,
                                  LEAD_APRONS_LW_TWOPIECE == "Y" ~ 1,
                                  TRUE ~ 0),
   EQUIP_LAPRON_STD1P = case_when (is.na(LEAD_APRONS_STANDARD_ONEPIECE) ~ NA_real_,
                                   LEAD_APRONS_STANDARD_ONEPIECE == "Y" ~ 1,
                                  TRUE ~ 0),
   EQUIP_LAPRON_STD2P = case_when (is.na(LEAD_APRONS_STANDARD_TWOPIECE) ~ NA_real_,
                                   LEAD_APRONS_STANDARD_TWOPIECE == "Y" ~ 1,
                                   TRUE ~ 0),
   EQUIP_LAPRON_DOUBLE = case_when (is.na(LEAD_APRONS_DOUBLE) ~ NA_real_,
                                    LEAD_APRONS_DOUBLE == "Y" ~ 1,
                                   TRUE ~ 0),
   EQUIP_LAPRON_THYROID = case_when (is.na(LEAD_APRONS_THYROID) ~ NA_real_,
                                     LEAD_APRONS_THYROID == "Y" ~ 1,
                                    TRUE ~ 0),
   EQUIP_LAPRON_MATDOS = case_when (is.na(LEAD_APRONS_MATERNALDOS) ~ NA_real_,
                                      LEAD_APRONS_MATERNALDOS == "Y" ~ 1,
                                     TRUE ~ 0),
   EQUIP_LAPRON_FETALDOS = case_when (is.na(LEAD_APRONS_FETALDOS) ~ NA_real_,
                                      LEAD_APRONS_FETALDOS == "Y" ~ 1,
                                      TRUE ~ 0),
    
    EQUIP_TO_FORMAL= case_when (is.na(ERGO_FORMAL_TIMEOUT_PRIOR) ~ NA_real_,
                               ERGO_FORMAL_TIMEOUT_PRIOR == "Y" ~ 1,
                               TRUE ~ 0),
    
    EQUIP_TO_INFORMAL= case_when (is.na(ERGO_INFORMAL_TIMEOUT_PRIOR) ~ NA_real_,
                                  ERGO_INFORMAL_TIMEOUT_PRIOR == "Y" ~ 1,
                                  TRUE ~ 0),
    
    EQUIP_MONITORS_EASYADJ = case_when (is.na(MONITORS_ADJUSTABLE) ~ NA_real_,
                                        MONITORS_ADJUSTABLE == "Y" ~ 1,
                                        TRUE ~ 0),
    
    
    TRNG_TACTILE_MALES = case_when (is.na(TACTILE_INSTRUCTION_FROM_MALES) ~ NA_real_,
                                    TACTILE_INSTRUCTION_FROM_MALES == "Often" ~ 2,
                                    TACTILE_INSTRUCTION_FROM_MALES == "Rarely" ~ 1,
                                    TRUE ~ 0),
    
    TRNG_TACTILE_FEMALES = case_when (is.na(TACTILE_INSTRUCTION_FROM_FEMALES) ~ NA_real_,
                                      TACTILE_INSTRUCTION_FROM_FEMALES == "Often" ~ 2,
                                      TACTILE_INSTRUCTION_FROM_FEMALES == "Rarely" ~ 1,
                                      TRUE ~ 0),
    
    
    
    # Higher numbers indicated heightened levels of respect
    
    CONFIDENT_NURSES = case_when (is.na(COMFORTABLE_ASKING_NURSES) ~ NA_real_,
                                COMFORTABLE_ASKING_NURSES == 'Y' ~ 1,
                                TRUE ~ 0 ),
    
    CONFIDENT_TECHS = case_when (is.na(COMFORTABLE_ASKING_TECHS) ~ NA_real_,
                               COMFORTABLE_ASKING_TECHS == "Y" ~ 1,
                               TRUE ~ 0),
    
    RESPECT_NFREQ_TRAINEE = case_when (is.na(NURSES_ASKING) ~ NA_real_,
                               NURSES_ASKING == "Once" ~ 2,
                               NURSES_ASKING == "Twice" ~ 1,
                               TRUE ~ 0),

    RESPECT_NFREQ_MATT = case_when (is.na(MALE_ATTENDINGS_ASKING) ~ NA_real_,
                                    MALE_ATTENDINGS_ASKING == "Once" ~ 2,
                                    MALE_ATTENDINGS_ASKING == "Twice" ~ 1,
                               TRUE ~ 0),

    RESPECT_NFREQ_FATT = case_when (is.na(FEMALE_ATTENDINGS_ASKING) ~ NA_real_,
                                    FEMALE_ATTENDINGS_ASKING == "Once" ~ 2,
                                    FEMALE_ATTENDINGS_ASKING == "Twice" ~ 1,
                               TRUE ~ 0),
    
    RESPECT_ESTAFF = case_when (is.na(RECOGNIZED_RESPECTED_ES_STAFF) ~ NA_real_,
                                RECOGNIZED_RESPECTED_ES_STAFF == "Y" ~ 1,
                                TRUE ~ 0),
    
    RESPECT_ANES = case_when (is.na(RECOGNIZED_RESPECTED_ANESTHETISTS) ~ NA_real_,
                              RECOGNIZED_RESPECTED_ANESTHETISTS == "Y" ~ 1,
                              TRUE ~ 0),
    
    RESPECT_GIATT = case_when (is.na(RECOGNIZED_RESPECTED_GASTRO_ATTENDING) ~ NA_real_,
                               RECOGNIZED_RESPECTED_GASTRO_ATTENDING == "Y" ~ 1,
                               TRUE ~ 0),
    
    RESPECT_GPAINS = case_when (is.na(GROWING_PAINS) ~ NA_real_,
                                GROWING_PAINS  == "Y" ~ 0,
                                TRUE ~ 1),
    
    TRNG_SENSITIVITY = case_when (is.na(TEACHER_SENSITIVITY_STATURE_HANDSIZE) ~ NA_real_,
                                  TEACHER_SENSITIVITY_STATURE_HANDSIZE  == "Y" ~ 1,
                                  TRUE ~ 0),
   
   TRNG_SENSITIVITYSX = case_when( is.na(TEACHER_SENSITIVITY_BY_GENDER) ~ NA_real_,
                                   TEACHER_SENSITIVITY_BY_GENDER == 'Both Equally' ~ 2,
                                   TEACHER_SENSITIVITY_BY_GENDER %in% c('Male','Female') ~ 1,
                                    TRUE ~ 0 ),
    
    RESPECT_FNAME =  case_when (is.na(FIRST_NAME_NO_PERMISSION) ~ NA_real_,
                                FIRST_NAME_NO_PERMISSION == "Y" ~ 0,
                                TRUE ~ 1))  %>% 
  
  
  # Median Substitute missing values FOR EACH SEX

   group_by( BIRTHSEX ) %>%
   mutate_at(vars( FMTEACH_RATIO, PAIN_INJURY, PAIN_HAND:RESPECT_FNAME), ~replace_na(., median(., na.rm = TRUE))) %>% ungroup() %>%
  
  
  select( RECORD_ID, BIRTHSEX, RACE2, AGE2, TRAINING_LEVEL, HEIGHT2, HEIGHTBAND=HTCAT, HTCATSEXADJ, GLOVE, FMTEACH_RATIO,
          starts_with('DEMO_'), starts_with('TRNG_'),  starts_with('EQUIP_'), starts_with('RESPECT_'), starts_with('PAIN_'), starts_with('CONFIDENT_')) 

  
    # left_join(CORRECT[, c("RECORD_ID", "COUNT_CORRECT")], by= 'RECORD_ID')  %>% rename( ERGO_QUIZ = COUNT_CORRECT) %>% 
    # group_by( BIRTHSEX ) %>%
    # mutate_at(vars( ERGO_QUIZ), ~replace_na(., median(., na.rm = TRUE))) %>% ungroup() 

   # left_join(APRON_SCORE[, c("RECORD_ID", "APRON_SCORE")], by= 'RECORD_ID')  %>% rename( EQUIP_APRONS_CT = APRON_SCORE)





# Observations with unacceptable SKEWNESS

REMOVESKEW <-

FACTOR_BASE_RIGOROUS %>% select( -c(1:9)  ) %>%
    summarize_all( ., funs(skew(.))) %>% 
    pivot_longer(., cols= everything(),
                    names_to = "QUESTION",
                    values_to = "SKEWNESS") %>% 
    filter( abs(SKEWNESS) > 2) %>% 
  mutate( VAR = QUESTION) %>% column_to_rownames(var = "VAR") 


# Observations with unacceptable KURTOSIS

REMOVEKURT <-
  
FACTOR_BASE_RIGOROUS %>% select( -c(1:9)  ) %>%
  summarize_all( ., funs(skew(.))) %>%
  pivot_longer(., cols= everything(),
               names_to = "QUESTION",
               values_to = "KURTOSIS") %>%
  filter( abs(KURTOSIS) > 1) %>% 
  mutate( VAR = QUESTION) %>% column_to_rownames(var = "VAR") 



# Observations with identical responses above 90% threshold

REMOVE90 <-
  
FACTOR_BASE_RIGOROUS %>% #select( !starts_with("RESPECT") ) %>% #
    pivot_longer(., cols=  TRNG_FORMAL: CONFIDENT_TECHS,
                    names_to = "QUESTION",
                    values_to = "SCORE") %>% select( QUESTION, RECORD_ID, SCORE) %>% arrange( QUESTION, desc(SCORE), RECORD_ID) %>%
    group_by( QUESTION, SCORE ) %>%
    summarize( N = n() ) %>%  ungroup() %>% group_by (QUESTION) %>%
    mutate( SCORE_PCT = N / sum(N, na.rm=T)) %>% arrange( QUESTION, desc(SCORE_PCT)) %>%
    group_by( QUESTION ) %>%  summarize (MAX_SCORE_PCT = max(SCORE_PCT)) %>% filter (MAX_SCORE_PCT > 0.90 ) %>% ungroup() %>% 
    mutate( VAR = QUESTION) %>% column_to_rownames(var = "VAR") 


 print(REMOVESKEW[-1])
 print(REMOVEKURT[-1])
 print(REMOVE90[-1])
 
 
 
 
REMOVEPROBLEMATIC <-
  
  sqldf( "select QUESTION from REMOVESKEW
                intersect 
          select QUESTION from REMOVEKURT
                intersect 
          select QUESTION from REMOVE90 ")

print(REMOVEPROBLEMATIC)


 
 # Choose one removal method, not both (for now)
 
# FACTOR_BASE_RIGOROUS %<>% select ( -rownames(REMOVE90))
 
# FACTOR_BASE_RIGOROUS %<>% select ( -rownames(REMOVESKEW))

# FACTOR_BASE_RIGOROUS %<>% select ( -rownames(REMOVESKEW))
 
 
 

describe(FACTOR_BASE_RIGOROUS[, -c(1:10)])
sum(complete.cases(FACTOR_BASE_RIGOROUS[, -c(1:10)]))

describe(FACTOR_BASE_RIGOROUS[, c(1:10)])
sum(complete.cases(FACTOR_BASE_RIGOROUS[, c(1:10)]))


#X <- na.omit(FACTOR_BASE_RIGOROUS[, -c(1:10)])


 X <- (FACTOR_BASE_RIGOROUS[, -c(1:10)])


# Bartlett's Test Sphericity - If you can't reject this null hypothesis, then there is essentially nothing to factor, as all variables are essentially different. 
# In other words, you ant your data matrix to have a p value greater than 0.05

psych::cortest.bartlett( X )


# X<- scale(center(X))



# KAISER-MEYER=OLKIN FACTOR ADEQUACY TESET

# Variables < 0.50 should be eliminated from analysis
# Score north of 0.70 is ideal; above 0.60 is acceptable


KMO( X)

KREMOVE<- colnames(X[ , KMO(X)$MSAi<0.49]) %>% tbl_df %>% pivot_wider(., names_from= value)
print(KREMOVE)


        X %<>% select ( -colnames(KREMOVE))

KMO(X)
max(KMO( X)$MSAi)
min(KMO( X)$MSAi)






# ANY VARIABLES YOU WANT TO CONSIDER MANUALLY REMOVING

      X %<>% select( -c(EQUIP_LAPRON_DOUBLE, EQUIP_LAPRON_FETALDOS, EQUIP_LAPRON_MATDOS, TRNG_BUDGET, TRNG_EXERCISE ))
      
  #    X %<>% select( -c( EQUIP_TO_FORMAL, TRNG_BEDHT, TRNG_BUDGET, TRNG_EXERCISE))
        KMO(X)
        max(KMO( X)$MSAi)
        min(KMO( X)$MSAi)




###################################
        
        
fa.parallel(X)

scree(X)




lenX <-length(X)

ev <- eigen(cor(X))
ev$values
nfactors <- rep(1: as.numeric(lenX))

Eigen_Values <- ev$values
Scree <- data.frame(nfactors, Eigen_Values)

ggplot(data = Scree, mapping = aes(x = nfactors, y = Eigen_Values)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(name = "Eigen Values", limits = c(0,6)) +
  scale_x_continuous(name= "Number of Factors", breaks = scales::breaks_width(1))+
  geom_vline(xintercept= 5, linetype='solid', col = 'red')+
  geom_vline(xintercept= 9, linetype='solid', col = 'red')+
  geom_vline(xintercept= which(ev$values < 1)[1], linetype='solid', col = 'darkred', size=1)+
  geom_hline(yintercept= 1, linetype='dashed', col = 'darkgrey', size=1)+
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "darkgrey")) +
  ggtitle("Scree Plot")



Xcorr <- cor(X)

#?fa


nfactors(X, rotate='promax', fm='mle')


#psych package fa solution

fapsych.out <- fa( X, nfactors = 12,
              fm="mle",
              max.iter = 1000000,
              scores= "regression",
              rotate = "promax")


# ?fa.diagram

fa.diagram(fapsych.out, sort=T, cut= 0.25, rsize=.35, main="Principal Axis Factor Analysis - 9-Factor Solution - Oblique (Promax) Rotation")


# 
# test <- as.data.frame(cbind(FACTOR_BASE_RIGOROUS$BIRTHSEX, (fapsych.out$scores[, "ML10"])))
# colnames(test) <- c('SEX', 'ML10')
# eov.ttest(test, ML10, SEX )





fa.sort(fapsych.out,polar=F)
fa.structure <- structure.diagram( fapsych.out ,errors=TRUE)
print(fa.structure$lavaan)
print(fapsych.out$loadings, digits=2, cutoff=0.30, sort=T)


sink("loadings.log", type= "output")
  print(fapsych.out$loadings, digits=2, cutoff=0.30, sort=T)
sink()



                      #Prepare for a SEM run

                      #   
                      # #if mle method
                      # mle.fa.model <- structure.diagram( fapsych.out ,errors=TRUE)
                      # mle.fa.model
                      # 
                      # ?structure.diagram
                      # #if pa method
                      # pa.fa.model <- structure.diagram( fapsych.out ,errors=TRUE)
                      # pa.fa.model
                      # 
                      # library(sem)
                      # 
                      # ?summary
                      
                        XMAT<- t(X)%*%as.matrix(X)
                        XMATcorr<- t(Xcorr)%*%as.matrix(Xcorr)

Model.mle9.promax <-"
ML6=~TRNG_INFORMAL+TRNG_POSTURAL+TRNG_BEDANG+TRNG_MONITOR+TRNG_MANEUVERS+TRNG_FEEDBACK+TRNG_FEEDBACKSX+TRNG_SENSITIVITY+TRNG_SENSITIVITYSX+EQUIP_TO_INFORMAL
ML3=~RESPECT_NFREQ_TRAINEE+RESPECT_NFREQ_MATT+RESPECT_NFREQ_FATT
ML1=~RESPECT_ESTAFF+RESPECT_ANES
ML2=~TRNG_TACTILE_MALES+TRNG_TACTILE_FEMALES
ML4=~EQUIP_LAPRON_LW1P+EQUIP_LAPRON_LW2P
ML8=~EQUIP_LAPRON_STD1P+EQUIP_LAPRON_STD2P+EQUIP_LAPRON_THYROID
ML7=~TRNG_DIALEXT+EQUIP_DIALEXT_AVAIL+EQUIP_DIALEXT_ENC
ML5=~CONFIDENT_NURSES+CONFIDENT_TECHS
ML9=~PAIN_NECKSH+PAIN_BACK+PAIN_LEG+PAIN_FOOT
"

Model.pa8.promax<-"
PA1=~TRNG_INFORMAL + TRNG_POSTURAL + TRNG_BEDANG + TRNG_MONITOR + TRNG_MANEUVERS + TRNG_FEEDBACK + TRNG_FEEDBACKSX + TRNG_SENSITIVITY + TRNG_SENSITIVITYSX + EQUIP_TO_INFORMAL
PA6=~RESPECT_ESTAFF + RESPECT_ANES + RESPECT_FNAME + CONFIDENT_NURSES + CONFIDENT_TECHS                                                                                       
PA2=~RESPECT_NFREQ_TRAINEE + RESPECT_NFREQ_MATT + RESPECT_NFREQ_FATT                                                                                                          
PA4=~TRNG_TACTILE_MALES + TRNG_TACTILE_FEMALES                                                                                                                                
PA7=~EQUIP_LAPRON_LW1P + EQUIP_LAPRON_LW2P                                                                                                                                    
PA3=~EQUIP_LAPRON_STD1P + EQUIP_LAPRON_STD2P + EQUIP_LAPRON_THYROID                                                                                                           
PA5=~TRNG_DIALEXT + EQUIP_DIALEXT_AVAIL + EQUIP_DIALEXT_ENC                                                                                                                   
PA8=~PAIN_NECKSH + PAIN_BACK + PAIN_LEG + PAIN_FOOT
"
                        
                      lavaan.mle.fa.model <- lavaan::cfa(Model.mle9.promax, data=X, std.lv=T)
                      summary(lavaan.mle.fa.model, fit.measures=T)

                      
                      lavaan.pa.fa.model <- lavaan::sem(Model.pa8.promax, data=X, std.lv=T,)
                      summary(lavaan.pa.fa.model, fit.measures=T)
                      
                  
                      ?sem
                      
                      
                      
                      
                      

fapsych.out$e.values
fapsych.out$values
fapsych.out$uniquenesses
fapsych.out$communality

fapsych.out$communality
fapsych.out$loadings
fapsych.out$TLI
fapsych.out$RMSEA



fapsych.out$uniquenesses[fapsych.out$uniqueness>0.85]
fapsych.out$communality[fapsych.out$communality<0.15]


?omega

alpha(X, check.keys=T)$total$std.alpha
psych::glb.fa( X)$glb
psych::omega(X, nfactors=12, rotate="promax", fm='ml' )$omega.tot
psych::omegaSem(X, nfactors=9, rotate="promax", fm='ml' )



alpha( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT", "TRNG_OPTIM")], check.keys=T)

alpha( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")], check.keys=T)
psych::glb.fa( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$glb
psych::omega( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$omega.tot

alpha( X[, c("TRNG_MONITOR", "EQUIP_MONITORS_EASYADJ")], check.keys=T)
psych::glb.fa( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")] )$glb
psych::omega( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")] )$omega.tot
alpha( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")], check.keys=T)

alpha( X[, c("TRNG_OPTIM", "TRNG_SENSITIVITY", "EQUIP_GLOVE_AVAIL", "EQUIP_LAPRON_AVAIL")], check.keys=T)
alpha( X[, c("EQUIP_GLOVE_AVAIL", "RESPECT_GPAINS")], check.keys=T)


#F1 of 9 - TRNG SENSITIVITY & FEEDBACK
alpha(X[ , c("TRNG_FEEDBACK","TRNG_FEEDBACKSX", "TRNG_POSTURAL",   "TRNG_MANEUVERS", 'TRNG_SENSITIVITYSX',"TRNG_INFORMAL", 'TRNG_SENSITIVITY',
             "TRNG_BEDANG", "EQUIP_TO_INFORMAL", "TRNG_FORMAL", "TRNG_MONITOR", "TRNG_BEDHT", 'TRNG_PEDISCOPE' ) ], check.keys=T)

psych::glb.fa( X[ , c("TRNG_FEEDBACK","TRNG_FEEDBACKSX", "TRNG_POSTURAL",   "TRNG_MANEUVERS", 'TRNG_SENSITIVITYSX',"TRNG_INFORMAL", 'TRNG_SENSITIVITY',
                      "TRNG_BEDANG", "EQUIP_TO_INFORMAL", "TRNG_FORMAL", "TRNG_MONITOR", "TRNG_BEDHT", 'TRNG_PEDISCOPE' ) ])$glb

psych::omega( X[ , c("TRNG_FEEDBACK","TRNG_FEEDBACKSX", "TRNG_POSTURAL",   "TRNG_MANEUVERS", 'TRNG_SENSITIVITYSX',"TRNG_INFORMAL", 'TRNG_SENSITIVITY',
                     "TRNG_BEDANG", "EQUIP_TO_INFORMAL", "TRNG_FORMAL", "TRNG_MONITOR", "TRNG_BEDHT", 'TRNG_PEDISCOPE' ) ], rotate='promax', fm='ml')$omega.tot


#F2 of 9 - NURSE COOP
alpha( X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")], check.keys=T)
psych::glb.fa( X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")])$glb
psych::omega(X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")])$omega.tot


#F3 of 9 - RESPECT (NO CONFID)
alpha (X[ , c( "RESPECT_ESTAFF","RESPECT_ANES", "RESPECT_GIATT", "RESPECT_FNAME" )], check.keys=T)$total$std.alpha
alpha (X[ , c( "RESPECT_ESTAFF","RESPECT_ANES" )], check.keys=T)$total$std.alpha
psych::glb.fa( X[ , c( "RESPECT_ESTAFF","RESPECT_ANES","RESPECT_GIATT", "RESPECT_FNAME" )])$glb
psych::omega( X[ , c( "RESPECT_ESTAFF","RESPECT_ANES","RESPECT_GIATT", "RESPECT_FNAME" )])$omega.tot



#F4 of 9 - TACTILE
alpha( X[, c( "TRNG_TACTILE_MALES", "TRNG_TACTILE_FEMALES")], check.keys=T)
psych::glb.fa( X[, c( "TRNG_TACTILE_MALES","TRNG_TACTILE_FEMALES", "TRNG_EXERCISE")])$glb
psych::omega( X[ , c( "TRNG_TACTILE_MALES","TRNG_TACTILE_FEMALES", "TRNG_EXERCISE" )])$omega.tot


#F5 of 9 - APRONS STD
alpha( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID", "EQUIP_LAPRON_AVAIL")], check.keys=T)
psych::glb.fa( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID")])$glb
psych::omega( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID")])$omega.tot



#F6 of 9 - APRONS LW
alpha( X[, c("EQUIP_LAPRON_LW1P", "EQUIP_LAPRON_LW2P")], check.keys=T)
psych::glb.fa( X[, c( "EQUIP_LAPRON_LW1P","EQUIP_LAPRON_LW2P")])$glb
psych::omega(  X[, c( "EQUIP_LAPRON_LW1P","EQUIP_LAPRON_LW2P")])$omega.tot


#F7 of 9 - DIAL EXTENDERS
alpha( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")], check.keys=T)
psych::glb.fa( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")])$glb
psych::omega( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")])$omega.tot



cor.test( X$EQUIP_LAPRON_FETALDOS, X$EQUIP_LAPRON_MATDOS, method='p')


#F8 of 9 - TRAINEE CONFIDENCE
alpha( X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")], check.keys=T)
psych::glb.fa( X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")])$glb
psych::omega(X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")])$omega.tot



#F9 of 9 - PAIN 
alpha( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")], check.keys=T)$total$std.alpha
psych::glb.fa( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")])$glb
psych::omega( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")])$omega.tot






# alpha(X[ , c("TRNG_TEACHERSF", "TRNG_TEACHERSM")], check.keys = T)$total$std.alpha
# psych::glb.fa(X[ , c("TRNG_TEACHERSF", "TRNG_TEACHERSM")], check.keys = T)$glb
# psych::omega( X[, c("TRNG_EXERCISE", "TRNG_DIALEXT", "TRNG_MONITOR", "TRNG_PEDISCOPE", "TRNG_FORMAL")])$omega.tot


alpha(X[ , c("TRNG_EXERCISE", "TRNG_DIALEXT", "TRNG_MONITOR", "TRNG_PEDISCOPE")], check.keys = T)$total$std.alpha
psych::glb.fa( X[, c("TRNG_EXERCISE", "TRNG_DIALEXT", "TRNG_MONITOR", "TRNG_PEDISCOPE")])$glb
psych::omega( X[, c("TRNG_EXERCISE", "TRNG_DIALEXT", "TRNG_MONITOR", "TRNG_PEDISCOPE")])$omega.tot


alpha(X[ , c("EQUIP_DIALEXT_AVAIL", "TRNG_DIALEXT")], check.keys = T)$total$std.alpha

alpha(X[ , c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")], check.keys = T)


?omega
?glb.fa

  psych::omega( X, nfactors=12, fm="mle", rotate="promax")
 
  
                
                alpha(X, check.keys=T)$total$std.alpha
                psych::glb.fa( X)$glb
                psych::omega(X, nfactors=12, rotate="promax", fm='ml' )$omega.tot
                
                
                
                alpha( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT", "TRNG_OPTIM")], check.keys=T)
                
                alpha( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")], check.keys=T)
                psych::glb.fa( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$glb
                psych::omega( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$omega.tot
                
                alpha( X[, c("TRNG_MONITOR", "EQUIP_MONITORS_EASYADJ")], check.keys=T)
                psych::glb.fa( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")] )$glb
                psych::omega( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")] )$omega.tot
                alpha( X[, c("TRNG_INFORMAL", "TRNG_BEDHT", "TRNG_MONITOR", "TRNG_POSTURAL")], check.keys=T)
                
                alpha( X[, c("TRNG_OPTIM", "TRNG_SENSITIVITY", "EQUIP_GLOVE_AVAIL", "EQUIP_LAPRON_AVAIL")], check.keys=T)
                alpha( X[, c("EQUIP_GLOVE_AVAIL", "RESPECT_GPAINS")], check.keys=T)
                
                
                #F1 of 12 - NURSE COOP
                alpha( X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")], check.keys=T)
                psych::glb.fa( X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")])$glb
                psych::omega(X[, c( "RESPECT_NFREQ_TRAINEE","RESPECT_NFREQ_MATT", "RESPECT_NFREQ_FATT")])$omega.tot
                
                
                
                #F2 of 9 - TACTILE
                alpha( X[, c( "TRNG_TACTILE_MALES", "TRNG_TACTILE_FEMALES")], check.keys=T)
                psych::glb.fa( X[, c( "TRNG_TACTILE_MALES","TRNG_TACTILE_FEMALES", "TRNG_EXERCISE")])$glb
                psych::omega( X[ , c( "TRNG_TACTILE_MALES","TRNG_TACTILE_FEMALES", "TRNG_EXERCISE" )])$omega.tot
                
                
                
                #F3 of 12- TRAINING WITH SENSITIVITY
                alpha(X[ , c("TRNG_SENSITIVITY", 'TRNG_SENSITIVITYSX', "EQUIP_TO_INFORMAL", "TRNG_MANEUVERS" ) ], check.keys=T)
                psych::glb.fa( X[ ,c("TRNG_SENSITIVITY", 'TRNG_SENSITIVITYSX', "EQUIP_TO_INFORMAL", "TRNG_MANEUVERS" ) ])$glb
                psych::omega( X[ , c("TRNG_SENSITIVITY", 'TRNG_SENSITIVITYSX', "EQUIP_TO_INFORMAL", "TRNG_MANEUVERS" ) ], rotate='promax', fm='ml')$omega.tot
                
                
                
                #F4 of 12 - RESPECT (NO CONFID)
                alpha (X[ , c( "RESPECT_ESTAFF","RESPECT_ANES", "RESPECT_GIATT", "RESPECT_FNAME" )], check.keys=T)$total$std.alpha
                psych::glb.fa( X[ , c( "RESPECT_ESTAFF","RESPECT_ANES","RESPECT_GIATT", "RESPECT_FNAME" )])$glb
                psych::omega( X[ , c( "RESPECT_ESTAFF","RESPECT_ANES","RESPECT_GIATT", "RESPECT_FNAME" )])$omega.tot
                
                
                #F5 of 12 - APRONS LW
                alpha( X[, c("EQUIP_LAPRON_LW1P", "EQUIP_LAPRON_LW2P")], check.keys=T)
                psych::glb.fa( X[, c( "EQUIP_LAPRON_LW1P","EQUIP_LAPRON_LW2P")])$glb
                psych::omega(  X[, c( "EQUIP_LAPRON_LW1P","EQUIP_LAPRON_LW2P")])$omega.tot
                
                #F6 of 12 - APRONS STD
                alpha( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID")], check.keys=T)
                psych::glb.fa( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID")])$glb
                psych::omega( X[, c("EQUIP_LAPRON_STD2P", "EQUIP_LAPRON_STD1P", "EQUIP_LAPRON_THYROID")])$omega.tot
                
                
                #F7 of 12 - TRAINEE CONFIDENCE
                alpha( X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")], check.keys=T)
                psych::glb.fa( X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")])$glb
                psych::omega(X[, c( "CONFIDENT_NURSES", "CONFIDENT_TECHS")])$omega.tot
                
                #F8 of 12- DIAL EXTENDERS
                alpha( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")], check.keys=T)
                psych::glb.fa( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")])$glb
                psych::omega( X[, c("TRNG_DIALEXT", "EQUIP_DIALEXT_AVAIL", "EQUIP_DIALEXT_ENC")])$omega.tot
                
                
                #F9 of 12- DIAL EXTENDERS
                alpha( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")], check.keys=T)
                psych::glb.fa( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$glb
                psych::omega( X[, c("TRNG_INFORMAL", "TRNG_POSTURAL", "TRNG_BEDHT", "TRNG_MONITOR")])$omega.tot
                
                
                
                #F11 of 12- FEEDBACK
                alpha( X[, c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")], check.keys=T)
                psych::glb.fa( X[,  c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")])$glb
                psych::omega( X[,  c("PAIN_NECKSH", "PAIN_BACK", "PAIN_LEG", "PAIN_FOOT")])$omega.tot
                
                
                #F11 of 12- FEEDBACK
                alpha( X[, c("TRNG_FEEDBACK", "TRNG_FEEDBACKSX")], check.keys=T)
                psych::glb.fa( X[,  c("TRNG_FEEDBACK", "TRNG_FEEDBACKSX")])$glb
                psych::omega( X[,  c("TRNG_FEEDBACK", "TRNG_FEEDBACKSX")])$omega.tot
                
                
                
                #F12 of 12- MONITORS
                alpha( X[, c("TRNG_MONITOR", "EQUIP_MONITORS_EASYADJ")], check.keys=T)
                psych::glb.fa( X[,  c("TRNG_MONITOR", "EQUIP_MONITORS_EASYADJ")])$glb
                psych::omega( X[,  c("TRNG_MONITOR", "EQUIP_MONITORS_EASYADJ")])$omega.tot
                

                
                
# fa( X, nfactors=3, residuals = FALSE, fm="ml", rotate = "varimax", scores="regression", digits = 2, max.iter = 50)



 x<- as.data.frame(cbind( X$PAIN_BACK, X$PAIN_FOOT, X$PAIN_HAND, X$PAIN_LEG, X$PAIN_NECKSH, SURVEY$TRAINING_LEVEL))
 colnames(x) <- c('PBACK','PFOOT','PHAND','PLEG','PNECK','TLEVEL')
  
  
  
#  ?sapply 
#   
#   x <- sapply(x[,1:5], `*`, x$TLEVEL)
#   
#   
# alpha( x, check.keys=T)$total[2]
# psych::glb.fa(x)$glb
  
  
  
#factanal solution

# testf <-factanal( X, factors=6, scores = c("regression"), rotation = "promax", cutoff=0.4)
# fa.diagram(testf$loadings, sort=T)
# 
# print(testf$loadings,sort=TRUE, cutoff = .35)
# 
# 
# 
# FACTANAL.RIGOROUS <- cbind( FACTOR_BASE_RIGOROUS, testf$scores ) %>% 
#   rename( F1.TRNG.FEEDBACK.SENSITIVITY = Factor1,
#           F2.NURSES = Factor2,
#           F3.CONFIDENCE = Factor3,
#           F4.PAIN = Factor4,
#           F5.RESPECT = Factor5,
#           F7.TRNG.ERGO = Factor6)
# YR <-
#   FACTANAL.RIGOROUS %>% 
#   pivot_longer( cols= contains("."),
#                 names_to = "FACTOR",
#                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:HTCATSEXADJ, FACTOR, FACTOR.SCORES)





# IF USING A MAXIMUM LIKELIHOOD SOLUTION (SAS-LIKE)

 #promax
 FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
   rename( F1.TRNG.FEEDBACK.SENSITIVITY = ML5,
           F2.NURSE.COOP = ML3,
           F3.RESPECT.ESTAFF = ML1,
           F4.TRNG.TACTILE = ML2,
           F5.APRONS.LW = ML4,
           F6.APRONS.STD = ML7,
           F7.CONFIDENCE.TRAINEE= ML6,
           F8.PAIN = ML8,
           F9.TRNG.EQUIP = ML9)
 YR.fa <-
   FACTANAL.RIGOROUS.fa %>% 
   pivot_longer( cols= contains("."),
                 names_to = "FACTOR",
                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:FMTEACH_RATIO, FACTOR, FACTOR.SCORES)


 
 
 #promax dial ext
 FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
   rename( F1.TRNG.FEEDBACK.SENSITIVITY = ML6,
           F2.NURSE.COOP = ML3,
           F3.RESPECT.ESTAFF = ML1,
           F4.TRNG.TACTILE = ML2,
           F5.APRONS.LW = ML4,
           F6.APRONS.STD = ML8,
           F7.DIALEXT = ML7,
           F8.CONFIDENCE.TRAINEE = ML5,
           F9.PAIN = ML9)
 YR.fa <-
   FACTANAL.RIGOROUS.fa %>% 
   pivot_longer( cols= contains("."),
                 names_to = "FACTOR",
                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:FMTEACH_RATIO, FACTOR, FACTOR.SCORES)
 
 
 
 
 
 #promax 12 factor
 FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
   rename( F01.NURSE.COOP = ML2,
           F02.TRNG.SENSITIVITY = ML12,
           F03.APRONS.LW = ML4,
           F04.CONFIDENCE.TRAINEE = ML3,
           F05.TRNG.POSTURAL = ML11,
           F06.TRNG.FEEDBACK = ML5,
           F07.TRNG.TACTILE = ML1,
           F08.RESPECT.ESTAFF = ML8,
           F09.APRONS.STD = ML7,
           F10.DIALEXT = ML6,
           F11.PAIN = ML9,
           F12.MONITORS = ML10) %>% 
   select( c(1:60),  F01.NURSE.COOP,
                     F02.TRNG.SENSITIVITY,
                     F03.APRONS.LW,
                     F04.CONFIDENCE.TRAINEE,
                     F05.TRNG.POSTURAL,
                     F06.TRNG.FEEDBACK,
                     F07.TRNG.TACTILE,
                     F08.RESPECT.ESTAFF,
                     F09.APRONS.STD,
                     F10.DIALEXT,
                     F11.PAIN,
                     F12.MONITORS      )
 
 YR.fa <-
   FACTANAL.RIGOROUS.fa %>% 
   pivot_longer( cols= contains("."),
                 names_to = "FACTOR",
                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:FMTEACH_RATIO, FACTOR, FACTOR.SCORES) %>% 
   arrange( RECORD_ID, FACTOR)
 
 
 
 
 
 
 

grouped_ggbetweenstats(
  data = YR.fa,
  x = BIRTHSEX,
  y = FACTOR.SCORES,
  type="parametric",
  bf.message=F,
  caption = 'factanal mle - PROMAX ROTATION - 9-FACTOR SOLUTION',
  grouping.var = FACTOR )

pairs.panels( FACTANAL.RIGOROUS.fa[, c(61:72)] , pch=21, stars=T) 





#DIAL EXTENDER VERSION

tab_model(         
  lm( F9.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F6.APRONS.STD , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F8.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F5.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F5.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY + F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ BIRTHSEX + F6.APRONS.STD + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY+ F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Female Trainees<br>.")


tab_model(          
  lm( F9.PAIN ~ HEIGHTBAND , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F6.APRONS.STD , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F8.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F5.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F5.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY + F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F6.APRONS.STD + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY+ F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by SHORTER Trainees<br>.")


  summary(lm( F9.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa))

# Create the model


fit <- lm( F9.PAIN ~ BIRTHSEX + F6.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa)



# Create the base plot
ggplot(mtcars, aes(mpg, disp))+
  geom_point() +
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_line(data = fortify(fit), aes(x = mpg, y = .fitted))






ggplot( FACTANAL.RIGOROUS.fa, aes( F8.CONFIDENCE.TRAINEE, F9.PAIN, col=BIRTHSEX, fill=BIRTHSEX))+
  geom_point( shape=21, col="black") +
 # geom_line(data = fortify(fit), aes(x = F8.CONFIDENCE.TRAINEE, y = .fitted))
  geom_smooth(method='lm', formula= y ~ x, aes( color=BIRTHSEX))










tab_model(         
  lm( F9.PAIN ~ HEIGHTBAND , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F5.APRONS.STD , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F6.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F8.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F6.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F6.APRONS.LW + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY + F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  lm( F9.PAIN ~ HEIGHTBAND + F5.APRONS.STD + F8.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY+ F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Female Trainees<br>.")



corr.test( as.numeric(FACTANAL.RIGOROUS.fa$BIRTHSEX), as.numeric(FACTANAL.RIGOROUS.fa$HEIGHTBAND), method = 'pearson')

cor.test( FACTANAL.RIGOROUS.fa$PAIN_HAND, FACTANAL.RIGOROUS.fa$EQUIP_GLOVE_AVAIL, method='p')



  Anova( lm (F9.PAIN ~ HTCATSEXADJ, data= FACTANAL.RIGOROUS.fa), type=3)

  summary(lm( F8.PAIN ~ BIRTHSEX + FM, data = FACTANAL.RIGOROUS.fa))
  summary(lm( F8.PAIN ~ BIRTHSEX + F6.APRONS.STD, data = FACTANAL.RIGOROUS.fa))
  summary(lm( F8.PAIN ~ BIRTHSEX + F5.APRONS.LW, data = FACTANAL.RIGOROUS.fa))
  summary(lm( F8.PAIN ~ BIRTHSEX + F4.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa))
  
  
  summary( lm( F3.RESPECT.ESTAFF ~ BIRTHSEX, data= FACTANAL.RIGOROUS.fa))
  summary( lm( F3.RESPECT.ESTAFF ~ HEIGHTBAND, data= FACTANAL.RIGOROUS.fa))
  summary( lm( F3.RESPECT.ESTAFF ~ HEIGHTBAND + F1.TRNG.FEEDBACK.SENSITIVITY, data= FACTANAL.RIGOROUS.fa))
  summary( lm( F3.RESPECT.ESTAFF ~ F8.CONFIDENCE.TRAINEE + BIRTHSEX, data= FACTANAL.RIGOROUS.fa))

  
  
tab_model(         
  lm( F8.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F7.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ BIRTHSEX + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Female Trainees<br>.")


tab_model(         
  lm( F8.PAIN ~ TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F7.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ TRAINING_LEVEL + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Training Levels<br>.")

Anova(lm(F8.PAIN ~ TRAINING_LEVEL + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa), type=3)
Anova(  lm( F8.PAIN ~ TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa), type=3)

tab_model(         
  lm( F8.PAIN ~ HEIGHTBAND, data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F7.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHTBAND + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Height Band<br>.")





FACTANAL.RIGOROUS.fa$HEIGHT2 <- relevel( FACTANAL.RIGOROUS.fa$HEIGHT2, ref= "5'7-5'9")

tab_model(         
  lm( F8.PAIN ~ HEIGHT2, data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F5.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F7.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F3.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F2.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F1.TRNG.FEEDBACK.SENSITIVITY  , data = FACTANAL.RIGOROUS.fa),
  lm( F8.PAIN ~ HEIGHT2 + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa),
  show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-9F-PROMAX: Models Showing Impact on Pain Experienced by Height Bands 2<br>.")


summary(lm( F8.PAIN ~ HEIGHT2, data = FACTANAL.RIGOROUS.fa))
anova(lm( F8.PAIN ~ HEIGHT2, data = FACTANAL.RIGOROUS.fa))
Anova(lm( F8.PAIN ~ HEIGHT2, data = FACTANAL.RIGOROUS.fa), type=3)

summary(lm( F8.PAIN ~ HEIGHT2 + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa))
anova(  lm( F8.PAIN ~ HEIGHT2 + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa))
anova(  lm( F8.PAIN ~ F1.TRNG.FEEDBACK.SENSITIVITY + F2.NURSE.COOP + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF +  HEIGHT2 , data = FACTANAL.RIGOROUS.fa))



Anova( lm( F8.PAIN ~ HEIGHT2 + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa), type=3)

Anova( lm( F8.PAIN ~ BIRTHSEX + F5.APRONS.LW + F7.CONFIDENCE.TRAINEE + F3.RESPECT.ESTAFF + F2.NURSE.COOP + F1.TRNG.FEEDBACK.SENSITIVITY, data = FACTANAL.RIGOROUS.fa), type=3)





summary(lm( F8.PAIN ~ BIRTHSEX + FMTEACH_RATIO , data = FACTANAL.RIGOROUS.fa))
summary(lm( F8.PAIN ~ HEIGHTBAND + FMTEACH_RATIO , data = FACTANAL.RIGOROUS.fa))
summary(lm( F8.PAIN ~ TRAINING_LEVEL + FMTEACH_RATIO , data = FACTANAL.RIGOROUS.fa))









#12 FACTOR VERSION

tab_model(         
  lm( F11.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F03.APRONS.LW , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F01.NURSE.COOP , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F08.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F04.CONFIDENCE.TRAINEE , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F02.TRNG.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F06.TRNG.FEEDBACK , data = FACTANAL.RIGOROUS.fa),
  
  lm( F11.PAIN ~ BIRTHSEX + F03.APRONS.LW + F01.NURSE.COOP + F08.RESPECT.ESTAFF + F04.CONFIDENCE.TRAINEE + F02.TRNG.SENSITIVITY 
                          + F06.TRNG.FEEDBACK , data = FACTANAL.RIGOROUS.fa),
  lm( F11.PAIN ~ BIRTHSEX + F03.APRONS.LW + F01.NURSE.COOP + F08.RESPECT.ESTAFF + F04.CONFIDENCE.TRAINEE + F02.TRNG.SENSITIVITY 
      + F06.TRNG.FEEDBACK + F07.TRNG.TACTILE, data = FACTANAL.RIGOROUS.fa),
    show.intercept=F, show.ci=F, 
  title= "FACTANAL-MLE-12F-PROMAX: Models Showing Impact on Pain Experienced by Female Trainees<br>.")


          

# PAIN STEPWISE-12

painmodel.sexbased.f12 <- lm(   F11.PAIN ~ TRAINING_LEVEL 
                             + F01.NURSE.COOP
                             + F02.TRNG.SENSITIVITY
                             + F03.APRONS.LW
                             + F04.CONFIDENCE.TRAINEE
                             + F05.TRNG.POSTURAL
                             + F06.TRNG.FEEDBACK
                             + F07.TRNG.TACTILE
                             + F08.RESPECT.ESTAFF
                             + F10.DIALEXT
                             + F12.MONITORS
                             + FMTEACH_RATIO, data = FACTANAL.RIGOROUS.fa)


stepsex.f12 <-step( painmodel.sexbased.f12, scope= list( lower= F11.PAIN ~ BIRTHSEX,
                                                       upper= painmodel.sexbased.f12), direction='backward')





summary(stepsex.f12)
round(summary(stepsex.f12)$adj.r.squared,2)
stepsex.f12$anova$AIC[length(stepsex.f12$anova$AIC)]
Anova(stepsex.f12, type=3)












 # IF USING A MINRES SOLUTION
          
          FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
            rename( F1.TRNG.FEEDBACK = MR1,
                    F2.NURSE.COOPEARTION = MR2,
                    F3.CONFIDENCE = MR3,
                    F4.PAIN = MR4,
                    F5.RESPECT.SRSTAFF = MR8,
                    F6.MONITORS = MR6,
                    F7.APRONS.FIT.AVAIL = MR5,
                    F8.TIMEOUTS = MR7)
          YR.fa <-
            FACTANAL.RIGOROUS.fa %>% 
            pivot_longer( cols= contains("."),
                          names_to = "FACTOR",
                          values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:HTCATSEXADJ, FACTOR, FACTOR.SCORES)
          
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = BIRTHSEX,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'factanal - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          pairs.panels( FACTANAL.RIGOROUS.fa[, c(50:57)] , pch=21, stars=T)
          
          




          
          

          
          
          # IF USING PRINCIPAL AXIS SOLUTION (R STANDARD)
          
          #pearson correlation
          
          # 
          # FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
          #   rename( F1.TRNG.FEEDBACK.SENSITIVITY = PA1,
          #           F2.RESPECT.ESTAFF = PA5,
          #           F3.APRONS.SPECIALTY = PA2,
          #           F4.NURSE.COOP = PA3,
          #           F5.TRNG.TACTILE = PA4,
          #           F6.APRONS.LW = PA6,
          #           F7.APRONS.STD = PA8,
          #           F8.PAIN = PA7,
          #           F9.TRNG.SPEC.EQUIP = PA9)
          # YR.fa <-
          #   FACTANAL.RIGOROUS.fa %>% 
          #   pivot_longer( cols= contains("."),
          #                 names_to = "FACTOR",
          #                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:HTCATSEXADJ, FACTOR, FACTOR.SCORES)
          # 
          # 
          # grouped_ggbetweenstats(
          #   data = YR.fa,
          #   x = BIRTHSEX,
          #   y = FACTOR.SCORES,
          #   type="parametric",
          #   bf.message=F,
          #   caption = 'factanal - PROMAX ROTATION - 9-FACTOR SOLUTION',
          #   grouping.var = FACTOR )
          # 
          # 
          # pairs.panels( FACTANAL.RIGOROUS.fa[, c(59:67)] , pch=21, stars=T) 
          
          
          
          
          
          
          FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
            rename( F1.TRNG.FEEDBACK.SENSITIVITY = PA1,
                    F2.RESPECT.ESTAFF = PA6,
                    F3.NURSE.COOP = PA2,
                    F4.TRNG.TACTILE = PA4,
                    F5.APRONS.LW = PA7,
                    F6.APRONS.STD = PA3,
                    F7.DIALEXT = PA5,
                    F8.PAIN = PA8)
          YR.fa <-
            FACTANAL.RIGOROUS.fa %>% 
            pivot_longer( cols= contains("."),
                          names_to = "FACTOR",
                          values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:HTCATSEXADJ, FACTOR, FACTOR.SCORES)
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = BIRTHSEX,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'factanal - PROMAX ROTATION - 8-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          pairs.panels( FACTANAL.RIGOROUS.fa[, c(61:68)] , pch=21, stars=T) 
          
          
          
          #polychoric correlation
          
          # FACTANAL.RIGOROUS.fa <- cbind( FACTOR_BASE_RIGOROUS, fapsych.out$scores ) %>% 
          #   rename( F1.TRNG.FEEDBACK.SENSITIVITY = PA1,
          #           F2.NURSE.COOP = PA5,
          #           F3.APRONS.LW = PA6,
          #           F4.PAIN = PA2,
          #           F5.APRONS.STD = PA3,
          #           F6.TRNG.TACTILE = PA4,
          #           F7.RESPECT = PA7)
          # YR.fa <-
          #   FACTANAL.RIGOROUS.fa %>% 
          #   pivot_longer( cols= contains("."),
          #                 names_to = "FACTOR",
          #                 values_to = "FACTOR.SCORES")  %>% select( RECORD_ID, BIRTHSEX:HTCATSEXADJ, FACTOR, FACTOR.SCORES)
          
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = BIRTHSEX,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'factanal - PROMAX ROTATION - 7-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          pairs.panels( FACTANAL.RIGOROUS.fa[, c(47:53)] , pch=21, stars=T, method="p")
          # psych::omega( X, fm="pa", nfactors=7, rotate="promax")
          
          ?pairs.panels            
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = BIRTHSEX,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'fa - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = HEIGHTBAND,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'fa - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = HTCATSEXADJ,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'fa - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = TRAINING_LEVEL,
            y = FACTOR.SCORES,
            type="parametric",
            bf.message=F,
            caption = 'fa - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          grouped_ggbetweenstats(
            data = YR.fa,
            x = HTCATSEXADJ,
            y = FACTOR.SCORES,
            type="parametric",
            adjust='none',
            bf.message=F,
            caption = 'fa - PROMAX ROTATION - 6-FACTOR SOLUTION',
            grouping.var = FACTOR )
          
          
          
          tab_model(
            lm( F8.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa), 
            lm( F8.PAIN ~ BIRTHSEX + F5.APRONS.LW, data = FACTANAL.RIGOROUS.fa), 
            lm( F8.PAIN ~ BIRTHSEX + F3.NURSE.COOP, data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ BIRTHSEX + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ BIRTHSEX + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
            
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F3.NURSE.COOP + F5.APRONS.LW + BIRTHSEX +F1.TRNG.FEEDBACK.SENSITIVITY + BIRTHSEX  , data = FACTANAL.RIGOROUS.fa   ),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F3.NURSE.COOP + F5.APRONS.LW + BIRTHSEX +F1.TRNG.FEEDBACK.SENSITIVITY + F4.TRNG.TACTILE + BIRTHSEX  , data = FACTANAL.RIGOROUS.fa   ), show.intercept =F, show.ci = F)
          
          vif(  lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX +F1.TRNG.FEEDBACK.SENSITIVITY + BIRTHSEX  , data = FACTANAL.RIGOROUS.fa )  )
          
 
 
 
 # MODELS ONLY - BASED OFF OBLIQUE ROTATIONS ONLY
          
          
          pairs.panels( FACTANAL.RIGOROUS.fa[, c(59:66)] , pch=21, stars=T) 
          
          
          # Relationship with ES Nurses predicts PAIN/INJURY?
          
          levels(FACTANAL.RIGOROUS.fa$BIRTHSEX)
          
          summary( lm( F8.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ relevel(BIRTHSEX, ref='F') , data = FACTANAL.RIGOROUS.fa))
          
          summary( lm( F8.PAIN ~ F4.NURSE.COOP + BIRTHSEX, data = FACTANAL.RIGOROUS.fa))
          
          
          # Properly fitting LW Aprons predict PAIN/INJURY?
          
          summary( lm( F8.PAIN ~ F6.APRONS.LW , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ F6.APRONS.LW + TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ F6.APRONS.LW + BIRTHSEX , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ F6.APRONS.LW + BIRTHSEX + TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa))
          
          
          
          anova ( lm( F6.APRONS.LW ~ TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa))
          
          
          levels( FACTANAL.RIGOROUS$TRAINING_LEVEL)
          
          
          
          # Feeling Recognized by ES Sr Staff predicts PAIN/INJURY?
          
          summary( lm( F8.PAIN ~ F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ BIRTHSEX + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + BIRTHSEX , data = FACTANAL.RIGOROUS.fa))
          summary( lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX , data = FACTANAL.RIGOROUS.fa))
          
          
          
          
          test.emmeans <- emmeans (lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX , data = FACTANAL.RIGOROUS.fa),  ~BIRTHSEX, type='response')
          
          pairs(test.emmeans, reverse=T)
          
      
          
#TEMPORARY ASSIGNEMNT         
FACTANAL.RIGOROUS.fa$BIRTHSEX <- relevel( FACTANAL.RIGOROUS.fa$BIRTHSEX, ref= 'F')

          tab_model(
            lm( F8.PAIN ~ BIRTHSEX , data = FACTANAL.RIGOROUS.fa), 
            lm( F8.PAIN ~ BIRTHSEX + F6.APRONS.LW, data = FACTANAL.RIGOROUS.fa), 
            lm( F8.PAIN ~ BIRTHSEX + F4.NURSE.COOP, data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ BIRTHSEX + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ BIRTHSEX + F1.TRNG.FEEDBACK.SENSITIVITY , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + BIRTHSEX , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX +F1.TRNG.FEEDBACK.SENSITIVITY + BIRTHSEX  , data = FACTANAL.RIGOROUS.fa   ), show.intercept =F)
          
          
          
#REVERT ASSIGNEMNT         
FACTANAL.RIGOROUS.fa$BIRTHSEX <- relevel( FACTANAL.RIGOROUS.fa$BIRTHSEX, ref= 'M')
          
          
          
          
          ?vif
          
          tab_model(
            lm( F8.PAIN ~ HEIGHTBAND , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ HEIGHTBAND + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + HEIGHTBAND , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + HEIGHTBAND , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + BIRTHSEX +F1.TRNG.FEEDBACK.SENSITIVITY + HEIGHTBAND  , data = FACTANAL.RIGOROUS.fa   ), show.intercept =F)
          
          
          tab_model(
            lm( F8.PAIN ~ GLOVE , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ GLOVE + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + GLOVE , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + GLOVE , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW +F1.TRNG.FEEDBACK.SENSITIVITY + GLOVE  , data = FACTANAL.RIGOROUS.fa   ), show.intercept =F)
          
          tab_model(
            lm( F8.PAIN ~ TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ TRAINING_LEVEL + F2.RESPECT.ESTAFF , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW + TRAINING_LEVEL , data = FACTANAL.RIGOROUS.fa),
            lm( F8.PAIN ~ F2.RESPECT.ESTAFF + F4.NURSE.COOP + F6.APRONS.LW  +F1.TRNG.FEEDBACK.SENSITIVITY + TRAINING_LEVEL  , data = FACTANAL.RIGOROUS.fa   ), show.intercept =F)
          
          
          
          
          
          
          
          
          
          
          
          


histogram( FACTANAL.RIGOROUS.fa$F4.RESPECT)
histogram( FACTANAL.RIGOROUS.fa$F5.PAIN)


FACTANAL.RIGOROUS.fa %>%
  select( contains( ".")) %>% 
  summarize_all( ., min)
FACTANAL.RIGOROUS.fa %>%
  select( contains( ".")) %>% 
  summarize_all( ., max)
FACTANAL.RIGOROUS.fa %>%
  select( contains( ".")) %>% 
  summarize_all( ., median)


test <-
FACTANAL.RIGOROUS.fa %>% 
  mutate_at(  vars(contains( ".")),
              funs(case_when( is.na(.) ~ NA_real_,
                              . < median(.)  ~ 0,
                              TRUE ~ 1)))  %>% 
  mutate_at(  vars(contains( ".")),
              funs( recode_factor(factor(.), '0'='N','1'='Y'))) %>% select( contains("."), RECORD_ID:GLOVE )


?factor



  test %>% 
  select(contains(".")) %>% 
  summarize_each( funs( glm( . ~ test$BIRTHSEX, data=test, family="binomial"(logit))))
                    
                    
  
summary(glm( F1.TRNG.FEEDBACK.SENSITIVITY ~ BIRTHSEX, data= test, family="binomial"(logit)))
summary(glm( F2.NURSE.RESPONSIVENESS ~ BIRTHSEX, data= test, family="binomial"(logit)))
summary(glm( F3.CONFIDENCE ~ BIRTHSEX, data= test, family="binomial"(logit)))
summary(glm( F4.RESPECT ~ BIRTHSEX, data= test, family="binomial"(logit)))
summary(glm( F5.PAIN ~ BIRTHSEX, data= test, family="binomial"(logit)))
summary(glm( F6.EQUIP.FIT ~ BIRTHSEX, data= test, family="binomial"(logit)))


tab_model(
glm( F5.PAIN ~ F2.NURSE.RESPONSIVENESS, data= test, family="binomial"(logit)),
glm( F5.PAIN ~ F4.RESPECT, data= test, family="binomial"(logit)),
glm( F5.PAIN ~ F6.EQUIP.FIT, data= test, family="binomial"(logit)),
glm( F5.PAIN ~ BIRTHSEX , data= test, family="binomial"(logit)) ,
glm( F5.PAIN ~ BIRTHSEX + F4.RESPECT + F6.EQUIP.FIT, data= test, family="binomial"(logit)) ,
show.intercept=F
)

 pairs(emmeans(glm( F5.PAIN ~ RACE2 + F4.RESPECT + F6.EQUIP.FIT, data= test, family="binomial"(logit)), ~RACE2, type="response"), reverse=T)



 
 ##########################
 
 # purr MAP practice 
 
 lm_map_model_tests <-
 
 FACTANAL.RIGOROUS.fa %>% 
   select( contains("."), c('BIRTHSEX', 'TRAINING_LEVEL'), RACE=RACE2 ) %>% 

   pivot_longer(cols= contains("."), 
                names_to = 'FACTOR',
                values_to = 'VALUE' ) %>% 
   relocate( c('FACTOR','BIRTHSEX','TRAINING_LEVEL', "RACE",'VALUE')) %>% 
   arrange( FACTOR, BIRTHSEX, TRAINING_LEVEL, RACE) %>% 
   
   
   nest( BIRTHSEX : VALUE ) %>% 
   
   mutate(LM_UNADJ=    map( .x=data,  ~ summary(lm( VALUE ~ BIRTHSEX, data= .x))) ,
          LM_ADJ1=     map( .x=data,  ~ summary(lm( VALUE ~ BIRTHSEX + TRAINING_LEVEL, data= .x))) ,
          LM_ADJ2=     map( .x=data,  ~ summary(lm( VALUE ~ BIRTHSEX + TRAINING_LEVEL + RACE, data= .x))), 
          LM_ANOVA1=   map( .x=data,  ~ anova(lm( VALUE ~ BIRTHSEX + TRAINING_LEVEL, data= .x))) ,
          LM_ANOVA2=   map( .x=data,  ~ anova(lm( VALUE ~ BIRTHSEX + TRAINING_LEVEL + RACE, data= .x))) ) %>% 
   rename( COVAR.DATA = data)
 
 
 
 print(lm_map_model_tests$LM_UNADJ)
 print(lm_map_model_tests$LM_ANOVA1[3])
 
 
 
 print(lm_map_model_tests$LM_ADJ1[3])
 print(lm_map_model_tests$LM_ANOVA1[3])
 
 
 
 #############################
 
 
 
 fa.diagram(fapsych.out, sort=T, cut= .3, rsize=.35, main="Principal Axis Factor Analysis - 6-Factor Solution - Oblique (Promax) Rotation")
 
 
 ta<- alpha(X, check.keys=T)
 ta$total$std.alpha
 
 
 f1 <- X[ , c("TRNG_FEEDBACK", "TRNG_FEEDBACKSX", "TRNG_MANEUVERS", "TRNG_PEDISCOPE", "TRNG_SENSITIVITY", "TRNG_SENSITIVITYSX",
              "TRNG_FORMAL", "EQUIP_TO_INFORMAL", "TRNG_BEDANG")]
 
 f1a<- alpha(f1, check.keys=T)
 f1a$total$std.alpha
 
 f2 <- X[ , c("RESPECT_NFREQ_FATT", "RESPECT_NFREQ_MATT", "RESPECT_NFREQ_TRAINEE")]
 f2a<- alpha(f2, check.keys=T)
 f2a$total$std.alpha
 
 f3 <- X[ , c("TRNG_TACTILE_MALES", "TRNG_TACTILE_FEMALES")]
 f3a<- alpha(f3, check.keys=T)
 f3a$total$std.alpha
 
 
 f4 <-  X[ , c("RESPECT_GPAINS", "PAIN_HAND", "PAIN_NECKSH")]
 f4a<- alpha(f4, check.keys=T)
 f4a$total$std.alpha
 
 f5 <- X[ , c("EQUIP_LAPRON_AVAIL", "EQUIP_APRONS_CT", "ERGO_QUIZ")]
 f5a<- alpha(f5, check.keys=T)
 f5a$total$std.alpha
 
 f6 <- X[ , c("RESPECT_ANES", "RESPECT")]
 f6a<- alpha(f6, check.keys=T)
 f6a$total$std.alpha
 
 
 ta<- alpha(X, check.keys=T)
 ta$total$std.alpha
 
 
 
 ?omega
 
 psych::omega( X, fm="mle", nfactors=8, rotate="promax")
 
 library(lavaan)
 
 

 PAF6.model <- ' TRAINING  =~ TRNG_FEEDBACKSX +  TRNG_FEEDBACK + TRNG_INFORMAL + TRNG_POSTURAL + TRNG_MONITOR + TRNG_BEDHT +
                              TRNG_MANEUVERS +EQUIP_MONITORS_EASYADJ + TRNG_SENSITIVITY + TRNG_BEDANG + TRNG_EXERCISE + TRNG_FORMAL +
                              TRNG_PEDISCOPE + EQUIP_TO_INFORMAL
                              
                 NURSES =~    RESPECT_NFREQ_FATT + RESPECT_NFREQ_MATT +RESPECT_NFREQ_TRAINEE
                 
                 CONFIDENCE =~ CONFIDENT_NURSES + CONFIDENT_TECHS
                 
                 RESPECT =~   RESPECT_ANES + RESPECT_ESTAFF + RESPECT_GIATT +RESPECT_FNAME
                 
                 PAIN =~      PAIN_NECKSH + PAIN_LEG + PAIN_BACK + PAIN_FOOT +  RESPECT_GPAINS + PAIN_INJURY + PAIN_HAND
                 
                 EQUIP =~     EQUIP_LAPRON_AVAIL + EQUIP_GLOVE_AVAIL + TRNG_OPTIM + EQUIP_APRONS_CT'
 
 
 fit <- cfa(PAF6.model, data= X)
 
 summary(fit, fit.measures = TRUE)
 
 
 
 reliability(fit)
 
 
