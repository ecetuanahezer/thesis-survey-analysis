# --- PART A: COMPOSITE SCORE CALCULATION ---

# Condition 1 Scores (Self-Creation)
data_clean <- data_clean %>%
  rowwise() %>%
  mutate(
    # Self Human-Written Text Evaluation
    Creativity_Self_Human = mean(c(Q6_1, Q6_2, Q6_3), na.rm = TRUE),
    Quality_Self_Human = mean(c(Q7_1, Q7_2, Q7_3), na.rm = TRUE),
    Willingness_Self_Human = mean(c(Q8_1, Q8_2), na.rm = TRUE),
    Effort_Self_Human = mean(c(Q9_1, Q9_2), na.rm = TRUE), # Mediator
    
    # Self AI-Assisted Text Evaluation
    Creativity_Self_AI = mean(c(Q10_1, Q10_2, Q10_3), na.rm = TRUE),
    Quality_Self_AI = mean(c(Q11_1, Q11_2, Q11_3), na.rm = TRUE),
    Willingness_Self_AI = mean(c(Q12_1, Q12_2), na.rm = TRUE),
    Effort_Self_AI = mean(c(Q13_1, Q13_2), na.rm = TRUE) # Mediator
  ) %>%
  ungroup()

# Condition 2 Scores (Controlled Evaluation / Other)
data_clean <- data_clean %>%
  rowwise() %>%
  mutate(
    # Other Human-Written Text Evaluation
    Creativity_Other_Human = mean(c(Q16_1, Q16_2, Q16_3), na.rm = TRUE),
    Quality_Other_Human = mean(c(Q17_1, Q17_2, Q17_3), na.rm = TRUE),
    Willingness_Other_Human = mean(c(Q18_1, Q18_2), na.rm = TRUE),
    Effort_Other_Human = mean(c(Q19_1, Q19_2), na.rm = TRUE), # Mediator
    
    # Other AI-Assisted Text Evaluation
    Creativity_Other_AI = mean(c(Q20_1, Q20_2, Q20_3), na.rm = TRUE),
    Quality_Other_AI = mean(c(Q21_1, Q21_2, Q21_3), na.rm = TRUE),
    Willingness_Other_AI = mean(c(Q22_1, Q22_2), na.rm = TRUE),
    Effort_Other_AI = mean(c(Q23_1, Q23_2), na.rm = TRUE) # Mediator
  ) %>%
  ungroup()

# 1. Rerun the transformation, ensuring Q25 and Q26 are selected
data_long <- data_clean %>%
  # --- MODIFIED SELECTION STEP ---
  # Include Q25 and Q26 along with other necessary predictors
  select(Condition_IV, Attitude_C, ResponseId, Q25, Q26, 
         Creativity_Self_Human, Creativity_Self_AI, 
         Effort_Self_Human, Effort_Self_AI,
         Quality_Self_Human, Quality_Self_AI,
         Willingness_Self_Human, Willingness_Self_AI,
         Creativity_Other_Human, Creativity_Other_AI, 
         Effort_Other_Human, Effort_Other_AI,
         Quality_Other_Human, Quality_Other_AI,
         Willingness_Other_Human, Willingness_Other_AI) %>%
  # --------------------------------

# 2. Pivot Longer (Q25 and Q26 will be carried forward automatically)
pivot_longer(
  cols = starts_with(c("Creativity_", "Effort_", "Quality_", "Willingness_")),
  names_to = c(".value", "Source_Type", "AI_Use_Factor"),
  names_sep = "_", 
) %>%
  
  # 3. Create Final Factors
  mutate(
    Attribution_IV = factor(Source_Type),
    AI_Use_IV = factor(AI_Use_Factor, levels = c("Human", "AI"), labels = c("Human_Written", "AI_Assisted"))
  ) %>%
  
  # 4. Final Clean-up and Attitude Categorization
  select(ResponseId, Attribution_IV, AI_Use_IV, Attitude_C, Q25, Q26, 
         Creativity, Quality, Effort, Willingness) %>%
  na.omit() 

# Apply the Categorical Attitude creation logic (which uses Attitude_C)
median_attitude <- median(data_long$Attitude_C, na.rm = TRUE)

data_long <- data_long %>%
  mutate(
    Attitude_Category = factor(
      if_else(Attitude_C >= median_attitude, "High_Attitude", "Low_Attitude"),
      levels = c("Low_Attitude", "High_Attitude")
    )
  )

print("--- New data_long columns ready ---")
print(head(data_long %>% select(Q25, Q26, Attitude_Category)))
# --- PART B: RELIABILITY CHECKS (CRONBACH'S ALPHA) ---
library(psych)

# 1. Creativity_Self_Human Alpha
alpha_csh <- psych::alpha(data_clean %>% select(Q6_1, Q6_2, Q6_3), check.keys = TRUE)
print(paste("Creativity_Self_Human Alpha:", alpha_csh$total$raw_alpha))
alpha_csh
# 2. Quality_Self_Human (Q7_1, Q7_2, Q7_3)

alpha_qsh <- psych::alpha(data_clean %>% select(Q7_1, Q7_2, Q7_3), check.keys = TRUE)
print(paste("Quality_Self_Human Alpha (Q7):",alpha_qsh$total$raw_alpha))
alpha_qsh
# 3. Willingness_Self_Human (Q8_1, Q8_2)

alpha_wsh <- psych::alpha(data_clean %>% select(Q8_1, Q8_2), check.keys = TRUE)
print(paste("Willingness_Self_Human Alpha(Q8):",alpha_wsh$total$raw_alpha))
alpha_wsh
# 4. Effort_Self_Human (Q9_1, Q9_2) - Mediator

alpha_esh <- psych::alpha(data_clean %>% select(Q9_1, Q9_2), check.keys = TRUE)
print(paste("Effort_Self_Human Alpha (Q9):",alpha_esh$total$raw_alpha))
alpha_esh
# 5. Creativity_Self_AI (Q10_1, Q10_2, Q10_3)

alpha_csa <- psych::alpha(data_clean %>% select(Q10_1, Q10_2, Q10_3), check.keys = TRUE)
print(paste("Creativity_Self_AI Alpha (Q10):",alpha_csa$total$raw_alpha))
alpha_csa
# 6. Quality_Self_AI (Q11_1, Q11_2, Q11_3)

alpha_qsa <- psych::alpha(data_clean %>% select(Q11_1, Q11_2, Q11_3), check.keys = TRUE)
print(paste("Quality_Self_AI Alpha (Q11):",alpha_qsa$total$raw_alpha))
alpha_qsa
# 7. Willingness_Self_AI (Q12_1, Q12_2)

alpha_wsa <- psych::alpha(data_clean %>% select(Q12_1, Q12_2), check.keys = TRUE)
print(paste("Willingness_Self_AI Alpha (Q12):",alpha_wsa$total$raw_alpha))
alpha_wsa
# 8. Effort_Self_AI (Q13_1, Q13_2) - Mediator

alpha_esa <- psych::alpha(data_clean %>% select(Q13_1, Q13_2), check.keys = TRUE)
print(paste("Effort_Self_AI Alpha (Q13):",alpha_esa$total$raw_alpha))
alpha_esa

# -------------------------------------------------------------------

# B. OTher's Evaluation

# -------------------------------------------------------------------

# 9. Creativity_Other_Human (Q16_1, Q16_2, Q16_3)

alpha_coh <- psych::alpha(data_clean %>% select(Q16_1, Q16_2, Q16_3), check.keys = TRUE)
print(paste("Creativity_Other_Human Alpha (Q16):",alpha_coh$total$raw_alpha))
alpha_coh
# 10. Quality_Other_Human (Q17_1, Q17_2, Q17_3)

alpha_qoh <- psych::alpha(data_clean %>% select(Q17_1, Q17_2, Q17_3), check.keys = TRUE)
print(paste("Quality_Other_Human Alpha (Q17):",alpha_qoh$total$raw_alpha))
alpha_qoh
# 11. Willingness_Other_Human (Q18_1, Q18_2)

alpha_woh <- psych::alpha(data_clean %>% select(Q18_1, Q18_2), check.keys = TRUE)
print(paste("Willingness_Other_Human Alpha (Q18):",alpha_woh$total$raw_alpha))
alpha_woh
# 12. Effort_Other_Human (Q19_1, Q19_2) - Mediator

alpha_eoh <- psych::alpha(data_clean %>% select(Q19_1, Q19_2), check.keys = TRUE)
print(paste("Effort_Other_Human Alpha (Q19):",alpha_eoh$total$raw_alpha))
alpha_eoh
# 13. Creativity_Other_AI (Q20_1, Q20_2, Q20_3)

alpha_coa <- psych::alpha(data_clean %>% select(Q20_1, Q20_2, Q20_3), check.keys = TRUE)
print(paste("Creativity_Other_AI Alpha (Q20):",alpha_coa$total$raw_alpha))
alpha_coa
# 14. Quality_Other_AI (Q21_1, Q21_2, Q21_3)

alpha_qoa <- psych::alpha(data_clean %>% select(Q21_1, Q21_2, Q21_3), check.keys = TRUE)
print(paste("Quality_Other_AI Alpha (Q21):",alpha_qoa$total$raw_alpha))
alpha_qoa
# 15. Willingness_Other_AI (Q22_1, Q22_2)

alpha_woa <- psych::alpha(data_clean %>% select(Q22_1, Q22_2), check.keys = TRUE)
print(paste("Willingness_Other_AI Alpha (Q22):",alpha_woa$total$raw_alpha))
alpha_woa
# 16. Effort_Other_AI (Q23_1, Q23_2) - Mediator

alpha_eoa <- psych::alpha(data_clean %>% select(Q23_1, Q23_2), check.keys = TRUE)
print(paste("Effort_Other_AI Alpha (Q23):",alpha_eoa$total$raw_alpha))
alpha_eoa
