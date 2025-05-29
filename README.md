/data/
Data Source = [AGYWPharmacyPrEPRCT-JKExport_DATA_2025-05-28_1636.csv](https://github.com/user-attachments/files/20496477/AGYWPharmacyPrEPRCT-JKExport_DATA_2025-05-28_1636.csv)
/notebooks/
[Uploading the---
title: "Thesis"
author: "Jared Keirn"
format: html
editor: visual
---

```{r}
library("dplyr")
library("tidyr")
```

Creating a data frame by reading in a .csv file from REDCap. This data set is named agyw_redcap and consists of 9716 observations and 70 variables. A vector is defined as the chronological order of REDCap event names. This vector will be used to remove the longitudinal REDCap structure, removing any lag of ACE collection and treating each ACE response as collected during enrollment. This step is completed before the filter to remove participants younger than 18. After initial manipulation the data set named agyw_filtered, with 1605 participants and 67 variables.

```{r}
# Read the CSV file into a data frame named 'agyw_redcap'.
agyw_redcap <- read.csv("/Users/jaredkeirn/Dropbox/My Mac (Jrod)/Desktop/Thesis/AGYWPharmacyPrEPRCT-JKExport_DATA_2025-05-28_1636.csv")
head(agyw_redcap)
summary(agyw_redcap)
str(agyw_redcap)

# Define a vector 'event_order' specifying the chronological or logical order of REDCap event names.
# This order will be used to determine the 'first' response within each participant's data.
event_order <- c("enrollment_arm_1", "4_month_fup_arm_1", "7_month_fup_arm_1", "10_month_fup_arm_1", "termination_arm_1", "1_month_fup_arm_1", "interim_visit_arm_1")

# Collapse the data: One row per participant
agyw_collapsed <- agyw_redcap %>%
  group_by(clt_ptid) %>% summarise(
    # For ACE variables: Find the first non-missing value across all events for this participant. Each ACE question is answered once per participant.
    across(starts_with("ace_"), ~ .[!is.na(.)][1], .names = "{.col}"),
    # For other variables prioritize value from the 'enrollment_arm_1' event. If the enrollment value is missing or NA, fall back to the first encountered value.
    across(c(-redcap_event_name, -starts_with("ace_")),
           ~ {
             # Try to find the value specifically from the enrollment event row(s)
             # Note: This handles cases where a participant might somehow have multiple enrollment rows, taking the first.
             enroll_val <- .[redcap_event_name == "enrollment_arm_1"]
             # Check if an enrollment value was found and if it's not NA
             if (length(enroll_val) > 0 && !is.na(enroll_val[1])) {
               # If yes, use the first non-NA enrollment value
               enroll_val[1]
             } else {
               # If no valid enrollment value, fall back to the overall first value for this participant
               first(.)}},.names = "{.col}" # Keep original column names
    ),.groups = 'drop') # Remove grouping structure after summarising
head(agyw_collapsed)
summary(agyw_collapsed)


# Further processing: Filter by age and remove REDCap administrative columns
agyw_filtered <- agyw_collapsed %>%
  # Filter participants who are 18 or older based on dem_age.
  # Ensure dem_age is not NA before comparing.
  filter(!is.na(dem_age) & dem_age >= 18) %>%
  # Remove any remaining columns starting with "redcap_" (like redcap_event_name, redcap_data_access_group etc.)
  select(-starts_with("redcap_"))
head(agyw_filtered)
summary(agyw_filtered)
```

Exploration of ACE variables as Exposures. The ACE columns are identified and Frequencies are provided

```{r}
ace_columns <- names(agyw_filtered)[startsWith(names(agyw_filtered), "ace_")]

# Start a loop that iterates through each column name in the 'ace_columns' vector.
for (col in ace_columns) {
  # Print a descriptive message indicating which column's frequency is being displayed.
  # The paste() function combines the string "Frequency for column:" with the current column name.
  print(paste("Frequency for column:", col))

  # Use the table() function to calculate and display the frequency of each unique value
  # within the current column. 'agyw_collapsed[[col]]' accesses the column by its name (stored in 'col').
  print(table(agyw_filtered[[col]], useNA = "ifany"))

  # Print a newline character using cat("\n") to add a blank line after the frequency table
  # for better readability of the output when multiple columns are processed.
  cat("\n")
}
```

Convert Special Codes to NA and Assess Missingness

```{r}
#Identify columns containing 77 = Refused and 88 = Not sure
col_with_77 <- c("ace_married", "ace_marchoose", "ace_marconsent", "ace_parprob", "ace_parknow",
                 "ace_parfood","ace_pardrunk", "ace_parschool", "ace_famaddict", "ace_famdepr", 
                 "ace_famjail", "ace_pardivor", "ace_pardie", "ace_hminsult", "ace_attemptintercorse",
                 "ace_abandon", "ace_hmviolence", "ace_forceintercourse", "ace_hmviolenceobj", "ace_insult",
                 "ace_violence", "ace_bullied", "ace_violenceobj", "ace_bullymeth", "ace_fight",
                 "ace_sexassult", "ace_cmbeat", "ace_forcetouch", "ace_cmstab", "ace_cmthreat", "ace_move",
                 "ace_destruct", "ace_colbeat","ace_colfmviol")

col_with_88 <- c("ace_marchoose", "ace_pardivor", "ace_pardie")

#Recode these columns as NA
agyw_filtered <- agyw_filtered %>%
  mutate(across(all_of(col_with_77), ~na_if(., 77)))

agyw_filtered <- agyw_filtered %>%
  mutate(across(all_of(col_with_88), ~na_if(., 88)))
```

```{r}
#Renaming [dem_age] to Age(years)
agyw_trans <- agyw_filtered %>% rename("Age (years)" = dem_age)
```

Inspecting Marriage variables

```{r}
#1. Have you ever been married? 0 = No, 1 = Yes, 77 = Refused
table(agyw_trans$ace_married)

#1a. At what age were you first married? 14-24 years old
table(agyw_trans$ace_marriedage)
age_marriage_frequency <- table(agyw_trans$ace_marriedage)
total_appearances <- sum(age_marriage_frequency)
print(total_appearances)
# Identify the ages 18 and below that are present in the table
early_marriage_ages <- names(age_marriage_frequency)[as.numeric(names(age_marriage_frequency)) <= 18]
# Sum the frequencies for those ages
early_marriage_count <- sum(age_marriage_frequency[early_marriage_ages])
print(early_marriage_count)

#1b. At the time of your first marriage did you yourself choose your husband/wife? Asked if yes (1) to Q1. 1 = Yes, 0 = No, 88 = Not Sure
table(agyw_trans$ace_marchoose)

#1c. At the time of your first marriage if you did not choose your husband/wife yourself, did you give your consent to the choice? Asked if [ace_married] = 1 and [ace_marchoose] = 0
#1 = Yes, ) = No, 88 = Not Sure
table(agyw_trans$ace_marconsent)

#2. If you are a mother or father what was your age when your first child was born?
table(agyw_trans$ace_agefirstch)
```

Inspecting and Recoding Relationship with Parents/Guardians Emotional Neglect: P1, P2

```{r}
#2.1 (P1) Did your parents/guardians understand your problems and worries? 1 = Always, 2 = Most of the time, 3 = Sometimes, 4 = Rarely, 5 = Never, 77 = Refused
table(agyw_trans$ace_parprob)
#Recoding to num 0 = Yes, 1 = No 
agyw_trans <- agyw_trans %>%
  mutate(
    ace_parprob_yn = ifelse(ace_parprob %in% c(1, 2, 3, 4), 0, 1)
  )
table(agyw_trans$ace_parprob_yn)

#	2.2 (P2) Did your parents/guardians really know what you were doing with your free time when you were not at school or work? 1 = Always, 2 = Most of the time, 3 = Sometimes, 4 = Rarely, 5 = Never, 77 = Refused
table(agyw_trans$ace_parknow)
#Recoding to num 0 = Yes, 1 = No, 77 = refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_parknow_yn = case_when(
       ace_parknow %in% c(1, 2, 3, 4) ~ 0,  # Yes
       ace_parknow == 5 ~ 1,              # No
        ace_parknow == 77 ~ 77,             # Refused
     )
   )
table(agyw_trans$ace_parknow_yn)
```

Inspecting and Recoding Relationship with Parents/Guardians Physical Neglect: P3, P4, P5

```{r}
#2.3 (P3) How often did your parents/guardians not give you enough food even when they could easily have done so? 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_parfood)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_parfood_yn = case_when(
       ace_parfood == 1 ~ 1,             # Yes
       ace_parfood %in% c(2,3,0) ~ 0,    # No
        ace_parfood == 77 ~ 77,))        # Refused
table(agyw_trans$ace_parfood_yn)

#	2.4 (P4) Were your parents/guardians too drunk or intoxicated by drugs to take care of you?  1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_pardrunk)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_pardrunk_yn = case_when(
       ace_pardrunk == 1 ~ 1,             # Yes
       ace_pardrunk %in% c(2,3,0) ~ 0,    # No
        ace_pardrunk == 77 ~ 77,))        # Refused
table(agyw_trans$ace_pardrunk_yn)

#	2.5 (P5) How often did your parents/guardians not send you to school even when it was available? 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_parschool)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_parschool_yn = case_when(
       ace_parschool == 1 ~ 1,             # Yes
       ace_parschool %in% c(2,3,0) ~ 0,    # No
        ace_parschool == 77 ~ 77,))        # Refused
table(agyw_trans$ace_parschool_yn)
```

Family Environment - Alcohol and/or Drug Abuser, Someone chronically depressed, or incarcerated in ousehold: F1, F2, F3

```{r}
# 3.1 (F1) Did you live with a household member who was a problem drinker or alcoholic, or misused street or prescription drugs?
# 1 = Yes, 0 = No, 77 = Refused
table(agyw_trans$ace_famaddict)

# 3.2 (F2) Did you live with a household member who was depressed, mentally ill or suicidal?
# 1 = Yes, 0 = No, 77 = Refused
table(agyw_trans$ace_famdepr)

# 3.3 (F3) Did you live with a household member who was ever sent to jail or prison?
# 1 = Yes, 0 = No, 77 = Refused
table(agyw_trans$ace_famjail)
```

Family Environment - One or no parents, parental separation, or divorce: F4, F5

```{r}
# 3.4 (F4) Were your parents ever separated or divorced?
# 1 = Yes, 0 = No, 77 = Refused, 88 = Not applicable
table(agyw_trans$ace_pardivor)

# 3.5 (F5) Did your mother, father, or guardian die?
# 1 = Yes, 0 = No, 77 = Refused, 88 = Don't Know
table(agyw_trans$ace_pardie)
```

Family Environment - Household member treated violently: F6, F7, F8

```{r}
# 3.6 (F6) Did you see or hear a parent or household member in your home being yelled at, screamed at, sworn at, insulted or humiliated?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_hminsult)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_hminsult_yn = case_when(
       ace_hminsult == 1 ~ 1,             # Yes
       ace_hminsult %in% c(2,3,0) ~ 0,    # No
        ace_hminsult == 77 ~ 77,))        # Refused
table(agyw_trans$ace_hminsult_yn)

# 3.9 (F7) Did you see or hear a parent or household member in your home being slapped, kicked, punched or beaten up?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_hmviolence)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_hmviolence_yn = case_when(
       ace_hmviolence == 1 ~ 1,             # Yes
       ace_hmviolence %in% c(2,3,0) ~ 0,    # No
        ace_hmviolence == 77 ~ 77,))        # Refused
table(agyw_trans$ace_hmviolence_yn)

# 3.11 (F8) Did you see or hear a parent or household member in your home being hit or cut with an object, such as a stick (or cane), bottle, club, knife, whip etc.?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_hmviolenceobj)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_hmviolenceobj_yn = case_when(
       ace_hmviolenceobj == 1 ~ 1,             # Yes
       ace_hmviolenceobj %in% c(2,3,0) ~ 0,    # No
        ace_hmviolenceobj == 77 ~ 77,))        # Refused
table(agyw_trans$ace_hmviolenceobj_yn)
```

Abuse Questions - Contact Sexual Abuse: A5, A6, A7, A8

```{r}
# 6.2 (A5) Did someone touch or fondle you in a sexual way when you did not want them to?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_sexassult)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_sexassult_yn = case_when(
       ace_sexassult == 1 ~ 1,             # Yes
       ace_sexassult %in% c(2,3,0) ~ 0,    # No
        ace_sexassult == 77 ~ 77,))        # Refused
table(agyw_trans$ace_sexassult_yn)

# 7.1 (A6) Did someone make you touch their body in a sexual way when you did not want them to?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_forcetouch)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_forcetouch_yn = case_when(
       ace_forcetouch == 1 ~ 1,             # Yes
       ace_forcetouch %in% c(2,3,0) ~ 0,    # No
        ace_forcetouch == 77 ~ 77,))        # Refused
table(agyw_trans$ace_forcetouch_yn)

# 3.7 (A7) Did someone attempt oral, anal, or vaginal intercourse with you when you did not want them to?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_attemptintercorse)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_attemptintercorse_yn = case_when(
       ace_attemptintercorse == 1 ~ 1,             # Yes
       ace_attemptintercorse %in% c(2,3,0) ~ 0,    # No
        ace_attemptintercorse == 77 ~ 77,))        # Refused
table(agyw_trans$ace_attemptintercorse_yn)

# 3.10 (A8) Did someone actually have oral, anal, or vaginal intercourse with you when you did not want them to?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_forceintercourse)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_forceintercourse_yn = case_when(
       ace_forceintercourse == 1 ~ 1,             # Yes
       ace_forceintercourse %in% c(2,3,0) ~ 0,    # No
        ace_forceintercourse == 77 ~ 77,))        # Refused
table(agyw_trans$ace_forceintercourse_yn)
```

Abuse Questions - Emotional Abuse: A1, A2

```{r}
#4.1. (A1) Did a parent, guardian or other household member yell, scream or swear at you, insult or humiliate you?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_insult)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_insult_yn = case_when(
       ace_insult == 1 ~ 1,             # Yes
       ace_insult %in% c(2,3,0) ~ 0,    # No
        ace_insult == 77 ~ 77,))        # Refused
table(agyw_trans$ace_insult_yn)

# 3.8 (A2) Did a parent, guardian or other household member threaten to, or actually, abandon you or throw you out of the house?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_abandon)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_abandon_yn = case_when(
       ace_abandon == 1 ~ 1,             # Yes
       ace_abandon %in% c(2,3,0) ~ 0,    # No
        ace_abandon == 77 ~ 77,))        # Refused
table(agyw_trans$ace_abandon_yn)
```

Abuse Questions - Physical Abuse: A3, A4

```{r}
#4.2. (A3) Did a parent, guardian or other household member spank, slap, kick, punch or beat you up?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_violence)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_violence_yn = case_when(
       ace_violence == 1 ~ 1,             # Yes
       ace_violence %in% c(2,3,0) ~ 0,    # No
        ace_violence == 77 ~ 77,))        # Refused
table(agyw_trans$ace_violence_yn)

# 5.2 (A4) Did a parent, guardian or other household member hit or cut you with an object, such as a stick (or cane), bottle, club, knife, whip etc?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_violenceobj)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_violenceobj_yn = case_when(
       ace_violenceobj == 1 ~ 1,             # Yes
       ace_violenceobj %in% c(2,3,0) ~ 0,    # No
        ace_violenceobj == 77 ~ 77,))        # Refused
table(agyw_trans$ace_violenceobj_yn)
```

Peer Violence - Bullying: V1, V2

```{r}
#5.1 (V1) How often were you bullied?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_bullied)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_bullied_yn = case_when(
       ace_bullied == 1 ~ 1,             # Yes
       ace_bullied %in% c(2,3,0) ~ 0,    # No
        ace_bullied == 77 ~ 77,))        # Refused
table(agyw_trans$ace_bullied_yn)

#5.3 (V2) How were you bullied most often? 
# 1 = I was hit, kicked, pushed, shoved around, or locked indoors, 2 = I was made fun of because of my race, nationality or colour, 3 = I was made fun of because of my religion, 4 = I was made fun of with sexual jokes, comments, or gestures, 5 = I was left out of activities on purpose or completely ignored, 6 = I was made fun of because of how my body or face looked, 99 = I was bullied in some other way, 77 = Refused ()
table(agyw_trans$ace_bullymeth)
```

Peer Violence - Physical Fights: V3

```{r}
# 6.1 (V3) How often were you in a physical fight?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_fight)
#Transforming to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_fight_yn = case_when(
       ace_fight == 1 ~ 1,             # Yes
       ace_fight %in% c(2,3,0) ~ 0,    # No
        ace_fight == 77 ~ 77,))        # Refused
table(agyw_trans$ace_fight_yn)
```

Violence - Witnessing Community Violence: V4, V5, V6

```{r}
# 6.3 (V4) Did you see or hear someone being beaten up in real life?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_cmbeat)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_cmbeat_yn = case_when(
       ace_cmbeat == 1 ~ 1,             # Yes
       ace_cmbeat %in% c(2,3,0) ~ 0,    # No
        ace_cmbeat == 77 ~ 77,))        # Refused
table(agyw_trans$ace_cmbeat_yn)

# 7.2 (V5) Did you see or hear someone being stabbed in real life?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_cmstab)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_cmstab_yn = case_when(
       ace_cmstab == 1 ~ 1,             # Yes
       ace_cmstab %in% c(2,3,0) ~ 0,    # No
        ace_cmstab == 77 ~ 77,))        # Refused
table(agyw_trans$ace_cmstab_yn)

# 7.3 (V6) Did you see or hear someone being threatened with a knife or gun in real life?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_cmthreat)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_cmthreat_yn = case_when(
       ace_cmthreat == 1 ~ 1,             # Yes
       ace_cmthreat %in% c(2,3,0) ~ 0,    # No
        ace_cmthreat == 77 ~ 77,))        # Refused
table(agyw_trans$ace_cmthreat_yn)
```

Violence - Exposure to War/Collective Violence: V7, V8, V9, V10

```{r}
# 8.1 (V7) Were you forced to go and live in another place due to any of these events?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_move)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_move_yn = case_when(
       ace_move == 1 ~ 1,             # Yes
       ace_move %in% c(2,3,0) ~ 0,    # No
        ace_move == 77 ~ 77,))        # Refused
table(agyw_trans$ace_move_yn)

# 8.2 (V8) Did you experience the deliberate destruction of your home due to any of these events?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_destruct)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_destruct_yn = case_when(
       ace_destruct == 1 ~ 1,             # Yes
       ace_destruct %in% c(2,3,0) ~ 0,    # No
        ace_destruct == 77 ~ 77,))        # Refused
table(agyw_trans$ace_destruct_yn)

# 8.3 (V9) Were you beaten up by soldiers, police, militia, or gangs?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_colbeat)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_colbeat_yn = case_when(
       ace_colbeat == 1 ~ 1,             # Yes
       ace_colbeat %in% c(2,3,0) ~ 0,    # No
        ace_colbeat == 77 ~ 77,))        # Refused
table(agyw_trans$ace_colbeat_yn)

# 8.4 (V10) Was a family member or friend killed or beaten up by soldiers, police, militia, or gangs?
# 1 = Many Times, 2 = A few times, 3 = Once, 0 = Never, 77 = Refused
table(agyw_trans$ace_colfmviol)
#Recoding to a numeric 1 = Yes, 0 = No, 77 = Refused
agyw_trans <- agyw_trans %>%
   mutate(
     ace_colfmviol_yn = case_when(
       ace_colfmviol == 1 ~ 1,             # Yes
       ace_colfmviol %in% c(2,3,0) ~ 0,    # No
        ace_colfmviol == 77 ~ 77,))        # Refused
table(agyw_trans$ace_colfmviol_yn)
```

Covariates

```{r}
#Number of Completed Years of education: (0-20 years) Completed primary = 8 years, Secondary = 12 years
table(agyw_trans$dem_school_year)

#What kind of area do you live in? 1 = Urban, 2 = Peri-urban (right outside of city), 3 = Rural, 4 = Prefer not to answer
table(agyw_trans$dem_area_live_in)

#Are you formally employed? (exchanging your time for money)
table(agyw_trans$dem_formally_employed)
```

Outcomes

```{r}
#Had sex without a condom? 1 = Yes, 0 = No
table(agyw_trans$dem_without_condom)

#Engaged in sex in exchange of money or other favors? 1 = Yes, 0 = No
table(agyw_trans$dem_sex_money)

#Engaged in intravenous drug use? 1 = Yes, 0 = No
table(agyw_trans$dem_intravenous_drugs)
```

Load data.

```{r}
# Define the path to your thesis folder (use the path from your read.csv call)
thesis_folder_path <- "/Users/jaredkeirn/Dropbox/My Mac (Jrod)/Desktop/Thesis/"

# Define the full path and filename for your output file
output_file_rds <- file.path(thesis_folder_path, "agyw_processed.rds")

# Save the agyw_trans data frame to the specified RDS file
saveRDS(agyw_trans, file = output_file_rds)

# Confirmation message (optional)
print(paste("Data frame saved successfully to:", output_file_rds))
```
sis_ETL.qmd…]()

