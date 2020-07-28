# Cleaning the Chem Survey:

#setwd("~/Dropbox/Chemistry Survey - Students")
setwd("C:/Users/wbigb/OneDrive/Documents/1_University/Classes/Stat-Consulting/chem-surveys")
library(tidyverse)

FA19 <- read_csv("Chemistry_DFW_FA19.csv")
SP15 <- read_csv("Chemistry_DFW_SP15_FA18.csv")

# store in new tibble: only students that 1. completed the survey, 2. gave consent, and 3. D/F/W:
# also add column Semester:

FA19_2 <- FA19 %>%
  # 'filter' here keeps rows only when condition evaluates true:
  filter(Progress == 100) %>% 
  filter(Consent == "I consent, begin the study") %>% 
  filter(`DFW?` == "Yes") %>%
  # 'mutate' here adds a new column:
  mutate(Semester = "FA19")

# repeat for SP15:

SP15_2 <- SP15 %>%
  filter(Progress == 100) %>% 
  filter(Consent == "I consent, begin the study") %>%
  filter(`DFW?` == "Yes") %>%
  mutate(Semester = "SP15")

# separate Factors into 3 columns:
# separate only on commas: sep = ","

FA19_3 <- FA19_2 %>%
  separate(., Factors, into = c("Factor1", "Factor2", "Factor3"), sep = ",")

SP15_3 <- SP15_2 %>%
  separate(., Factors, into = c("Factor1", "Factor2", "Factor3"), sep = ",")

# create 2 columns out of all the class columns: (mutate fn?)

# 1. names of classes marked with D/F/W, separated by commas
# 2. respective grades received in those classes, separated by commas

# look at gather fn and spread fn
# use gather to get D/F/W in the same column?
# Use sep = "," to separate values by commas

# rename columns to simplify:
# *Note: surveys have different original names for 'Duration'

FA19_4 <- FA19_3 %>%
  dplyr::rename(
    GC1 = `General Chemistry 1`,
    GC1L = `General Chemistry 1 Lab`,
    GC2 = `General Chemistry 2`,
    GC2L = `General Chemistry 2 Lab`,
    QAL = `Quantitative Analysis & Lab`,
    OC1 = `Organic Chemistry 1`,
    OC1L = `Organic Chemistry 1 Lab`,
    OC2 = `Organic Chemistry 2`,
    OC2L = `Organic Chemistry 2 Lab`,
    FIA = `Fundamentals of Instrumental Analysis`,
    PC1 = `Physical Chemistry 1`,
    PC2 = `Physical Chemistry 2`,
    EPC = `Experimental Physical Chemistry`,
    B1 = `Biochemistry 1`,
    AO = `Advanced Organic`,
    ICL = `Inorganic Chemistry and Lab`,
    Start = `Start Date`,
    End = `End Date`,
    Duration = `Duration (in seconds)`,
    Date = `Recorded Date`,
    ID = `Response ID`,
    Classification = `Class`
  )

SP15_4 <- SP15_3 %>%
  dplyr::rename(
    GC1 = `General Chemistry 1`,
    GC1L = `General Chemistry 1 Lab`,
    GC2 = `General Chemistry 2`,
    GC2L = `General Chemistry 2 Lab`,
    QAL = `Quantitative Analysis & Lab`,
    OC1 = `Organic Chemistry 1`,
    OC1L = `Organic Chemistry 1 Lab`,
    OC2 = `Organic Chemistry 2`,
    OC2L = `Organic Chemistry 2 Lab`,
    FIA = `Fundamentals of Instrumental Analysis`,
    PC1 = `Physical Chemistry 1`,
    PC2 = `Physical Chemistry 2`,
    EPC = `Experimental Physical Chemistry`,
    B1 = `Biochemistry 1`,
    AO = `Advanced Organic`,
    ICL = `Inorganic Chemistry and Lab`,
    Start = `Start Date`,
    End = `End Date`,
    Duration = `Time(sec)`,
    Date = `Recorded Date`,
    ID = `Response ID`,
  )

# need to change all 'class' columns to char data type (some are logical):

FA19_4$GC1 <- as.character(FA19_4$GC1)
FA19_4$GC1L <- as.character(FA19_4$GC1L)
FA19_4$GC2 <- as.character(FA19_4$GC2)
FA19_4$GC2L <- as.character(FA19_4$GC2L)
FA19_4$QAL <- as.character(FA19_4$QAL)
FA19_4$OC1 <- as.character(FA19_4$OC1)
FA19_4$OC1L <- as.character(FA19_4$OC1L)
FA19_4$OC2 <- as.character(FA19_4$OC2)
FA19_4$OC2L <- as.character(FA19_4$OC2L)
FA19_4$FIA <- as.character(FA19_4$FIA)
FA19_4$PC1 <- as.character(FA19_4$PC1)
FA19_4$PC2 <- as.character(FA19_4$PC2)
FA19_4$EPC <- as.character(FA19_4$EPC)
FA19_4$B1 <- as.character(FA19_4$B1)
FA19_4$AO <- as.character(FA19_4$AO)
FA19_4$ICL <- as.character(FA19_4$ICL)

SP15_4$GC1 <- as.character(SP15_4$GC1)
SP15_4$GC1L <- as.character(SP15_4$GC1L)
SP15_4$GC2 <- as.character(SP15_4$GC2)
SP15_4$GC2L <- as.character(SP15_4$GC2L)
SP15_4$QAL <- as.character(SP15_4$QAL)
SP15_4$OC1 <- as.character(SP15_4$OC1)
SP15_4$OC1L <- as.character(SP15_4$OC1L)
SP15_4$OC2 <- as.character(SP15_4$OC2)
SP15_4$OC2L <- as.character(SP15_4$OC2L)
SP15_4$FIA <- as.character(SP15_4$FIA)
SP15_4$PC1 <- as.character(SP15_4$PC1)
SP15_4$PC2 <- as.character(SP15_4$PC2)
SP15_4$EPC <- as.character(SP15_4$EPC)
SP15_4$B1 <- as.character(SP15_4$B1)
SP15_4$AO <- as.character(SP15_4$AO)
SP15_4$ICL <- as.character(SP15_4$ICL)

# combine 2 parts of survey into one single survey: (use tibble or append?)

Survey_1 <- bind_rows(FA19_4, SP15_4)

# delete Variables that all have same contents: Finished (TRUE), DFW? (Yes), Progress (100), Consent (Given)
# delete Date: Date and End variables are redundant:

Survey_2 <- Survey_1 %>%
  select(-Date, -Finished, -`DFW?`, -Progress, -Consent)

# create variables for Class and Grade (may need to make this part of course condensing)
# The following code was written by my professor, Tracy Morris, PhD:

Survey_3 <- Survey_2 %>% 
  pivot_longer(cols = GC1:ICL, names_to = "Course", values_to = "Grade") %>% 
  filter(is.na(Grade) == F) %>% 
  pivot_longer(cols = Factor1:Factor3, values_to = "Factor") %>% 
  filter(is.na(Factor) == F) %>% 
  select(-name)

table_age <- Survey_2 %>% 
  group_by(Age) %>% 
  summarize(n = sum(!is.na(Age)))





# Notes

# pseudocode:

# for each observation {
#   for each class col {
#     if (col has letter) {
#       put col-name into Class variable (add comma + space?)
#       put letter into Grade variable (add comma + space?)
#     }
#   }
# }
#
# delete all old class columns


# some practice, unrelated to survey:

#practice <- group_by(Survey, Progress, Duration)
#summarize(Survey, 
#          new = mean(Duration))


