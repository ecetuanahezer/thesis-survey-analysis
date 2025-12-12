# --self preference
# 1. Isolate the Self-Condition data (where Q6 was answered)
data_self_preference <- data_clean %>% 
  filter(Condition_IV == "Self") %>%
  # Filter out NA if participant skipped the question
  filter(!is.na(Q4)) 

# 2. Count the number of people who chose AI (let's assume AI is coded as '2')
ai_preference_count <- sum(data_self_preference$Q4 == 2, na.rm = TRUE)
total_count <- nrow(data_self_preference)

# 3. Perform the Binomial Test (H0: Preference is 50/50, p=0.5)
# x = Number of AI choices, n = Total choices, p = Expected probability under H0
print("--- BINOMIAL TEST: SELF PREFERENCE (AI vs. HUMAN) ---")
binom.test(x = ai_preference_count, n = total_count, p = 0.5)
#  If $p < 0.05$ and the $\text{AI}$ choice percentage is $> 50\%$, $\text{H}1\text{c}$ is strongly supported (participants significantly choose the $\text{AI}$ augmented text).

# ---other preference
# 1. Isolate the Self-Condition data 
data_other_preference <- data_clean %>% 
  filter(Condition_IV == "Other") %>%
  # Filter out NA if participant skipped the question
  filter(!is.na(Q14)) 

# 2. Count the number of people who chose AI (let's assume AI is coded as '2')
ai_preference_count <- sum(data_other_preference$Q14 == 2, na.rm = TRUE)
total_count <- nrow(data_other_preference)

# 3. Perform the Binomial Test (H0: Preference is 50/50, p=0.5)
# x = Number of AI choices, n = Total choices, p = Expected probability under H0
print("--- BINOMIAL TEST: OTHER PREFERENCE (AI vs. HUMAN) ---")
binom.test(x = ai_preference_count, n = total_count, p = 0.5)
