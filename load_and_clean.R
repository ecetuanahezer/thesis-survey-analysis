library(readr)
library(dplyr)
library(psych)      # For Cronbach's Alpha and descriptive statistics
library(car)        # For ANOVA
library(ggplot2)    # For visualization
library(readxl)
library(tidyr)
# Load data (Replace file name with your actual file path)
data <- read_excel("Perceptions of AI-Assisted Content Creation_ Self vs. Other_December 1, 2025_21.50.xlsx", col_names = TRUE)
View(data)

# Data Cleaning and Filtering
data_clean <- data %>%
  # Remove metadata columns
  select(-c("StartDate", "EndDate", "IPAddress", "Progress", "RecordedDate",
            "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference",
            "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage", "Q_TerminateFlag")) %>%
  # Filter for completed, recorded responses where consent (Q1) was given
  filter(Status == "0.0", Finished == "1.0", Q1 == "1.0")

View(data_clean)
print(paste("Cleaned participant count:", nrow(data_clean)))

# Convert Likert and relevant columns to numeric type
data_clean <- data_clean %>%
  mutate(across(c(Q4,Q6_1:Q27), as.numeric))

# Create Condition Variable (IV) based on Q2 (Presence of Q2 = Self Condition)
data_clean <- data_clean %>%
  mutate(
    # Condition: Self if Q2 is not NA, Other if Q2 is NA
    Condition_IV = if_else(!is.na(Q2), "Self", "Other"),
    Condition_Code = if_else(Condition_IV == "Self", 1, 2)
  )

data_clean %>%
  group_by(Condition_IV) %>%
  summarise(Count = n())

# Check condition assignment consistency
table(is.na(data_clean$Q2), data_clean$Condition_IV)

gender_analysis <- data_clean %>%
  group_by(Condition_IV, Q27) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Condition_IV) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  rename(Gender_Category = Q27) %>%
  #  (Self vs Other)
  pivot_wider(
    names_from = Condition_IV,
    values_from = c(n, percent),
    names_glue = "{Condition_IV}_{.value}",
    values_fill = 0 
  )

print(gender_analysis)


# 2. Age Distribution Analysis (Q28)
age_analysis <- data_clean %>%
  group_by(Condition_IV, Q28) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Condition_IV) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  rename(Age_Category = Q28) %>%
  pivot_wider(
    names_from = Condition_IV,
    values_from = c(n, percent),
    names_glue = "{Condition_IV}_{.value}",
    values_fill = 0
  )

print("--- Age Distribution (Q28) ---")
print(age_analysis)

# AI Attitude Scale (Moderator)
data_clean <- data_clean %>%
  mutate(
    # Reverse Coding for Q24_4 and Q24_5 (assuming 1-5 scale)
    Q24_4_R = 6 - Q24_4,
    Q24_5_R = 6 - Q24_5
  ) %>%
  rowwise() %>%
  mutate(
    # Calculate Composite Attitude Score
    Attitude_Score = mean(c(Q24_1, Q24_2, Q24_3, Q24_4_R, Q24_5_R), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Center the Moderator for Regression/ANOVA analysis
  mutate(
    Attitude_C = scale(Attitude_Score, center = TRUE, scale = FALSE)
  )

# 1. Calculating Age Category Frequency
age_frequency <- data_clean %>%
# choose the answers where last question is filled
  filter(!is.na(Q28)) %>% 
  group_by(Q28) %>%
  summarise(
    Count = n(),
    Percent = n() / nrow(.) * 100 
  ) %>%
  arrange(desc(Count)) 

print("--- Age Groups Frequency Distribution ---")
print(age_frequency)

# 2. Modu Most Repeated Age Category
mode_age_category <- age_frequency$Q28[1]

print(paste("Mode (Median Category):", mode_age_category))
library(tidyr)
library(dplyr)


