# --- PART 1: SELF-CREATION CONDITION (PAIRED T-TESTS) ---

# 1. Filter data for the Self-Creation Condition
data_self <- data_clean %>% filter(Condition_IV == "Self")

# 2. Comparison of Creativity (DV)
# Hypothesis: AI-Assisted texts are perceived as more creative than Human-Written texts.
print("--- 1. CREATIVITY (AI vs. HUMAN) ---")
t.test(data_self$Creativity_Self_AI, data_self$Creativity_Self_Human, paired = TRUE)
# p < 0.05 meaning statistically meaningful.
# Mean of the Differences is positive meaning AI creativity > human (as suggested by t value) in self case.

# 3. Comparison of Quality (DV)
# Hypothesis: AI-Assisted texts are perceived as higher in quality than Human-Written texts.
print("--- 2. QUALITY (AI vs. HUMAN) ---")
t.test(data_self$Quality_Self_AI, data_self$Quality_Self_Human, paired = TRUE)

# 4. Comparison of Willingness to Use (DV)
# Hypothesis: AI-Assisted texts are more likely to be preferred for use.
print("--- 3. WILLINGNESS TO USE (AI vs. HUMAN) ---")
t.test(data_self$Willingness_Self_AI, data_self$Willingness_Self_Human, paired = TRUE)

# 5. Comparison of Perceived Effort (Mediator)
# Hypothesis: AI-Assisted texts are perceived as requiring less effort than Human-Written texts.
print("--- 4. PERCEIVED EFFORT (AI vs. HUMAN) ---")
t.test(data_self$Effort_Self_AI, data_self$Effort_Self_Human, paired = TRUE)


# --- PART 2: SELF-OTHER BIAS (INDEPENDENT T-TESTS) ---

# 1. Pooling Creativity Scores (AI output only)
data_clean <- data_clean %>%
  mutate(
    # Scores for the Self group should be in Creativity_Self_AI, and Other group scores in Creativity_Other_AI.
    Pooled_Creativity_AI = coalesce(Creativity_Self_AI, Creativity_Other_AI)
  )

# 2. Pooling Quality Scores (AI output only)
data_clean <- data_clean %>%
  mutate(
    Pooled_Quality_AI = coalesce(Quality_Self_AI, Quality_Other_AI)
  )

# 3. Pooling Willingness to Use Scores (AI output only)
data_clean <- data_clean %>%
  mutate(
    Pooled_Willingness_AI = coalesce(Willingness_Self_AI, Willingness_Other_AI)
  )

# 4. Pooling Perceived Effort Scores (Mediator, AI output only)
data_clean <- data_clean %>%
  mutate(
    Pooled_Effort_AI = coalesce(Effort_Self_AI, Effort_Other_AI)
  )

# -------------------------------------------------------------------
# Comparison: Self-AI Text vs. Other-AI Text (Using Pooled Scores)
# -------------------------------------------------------------------

# 1. Comparison of Creativity (DV)
print("--- 1. CREATIVITY (Self-AI vs. Other-AI) ---")
t.test(Pooled_Creativity_AI ~ Condition_IV, data = data_clean)

# 2. Comparison of Quality (DV)
print("--- 2. QUALITY (Self-AI vs. Other-AI) ---")
t.test(Pooled_Quality_AI ~ Condition_IV, data = data_clean)

# 3. Comparison of Willingness to Use (DV)
print("--- 3. WILLINGNESS TO USE (Self-AI vs. Other-AI) ---")
t.test(Pooled_Willingness_AI ~ Condition_IV, data = data_clean)

# 4. Comparison of Perceived Effort (Mediator)
# The result here will indicate if the Effort Heuristic is affected by the Self-Other Bias.
print("--- 4. PERCEIVED EFFORT (Self-AI vs. Other-AI) ---")
t.test(Pooled_Effort_AI ~ Condition_IV, data = data_clean)


library(car)

# --- ANCOVA for CREATIVITY (H1a Test) ---
# The formula tests Attribution * AI_Use interaction, controlling for Q2 and Q3.
ancova_creativity <- aov(Creativity ~ Attribution_IV * AI_Use_IV + Q25 + Q26, 
                         data = data_long)
print("--- ANCOVA: Perceived CREATIVITY (H1a) ---")
print(Anova(ancova_creativity, type = 3))

# --- ANCOVA for QUALITY (H1b Test) ---
ancova_quality <- aov(Quality ~ Attribution_IV * AI_Use_IV + Q25 + Q26, 
                      data = data_long)
print("--- ANCOVA: Perceived QUALITY (H1b) ---")
print(Anova(ancova_quality, type = 3))

# --- ANCOVA for WILLINGNESS TO USE (H1c Test) ---
ancova_wtu <- aov(Willingness ~ Attribution_IV * AI_Use_IV + Q25 + Q26, 
                  data = data_long)
print("--- ANCOVA: WILLINGNESS TO USE (H1c) ---")
print(Anova(ancova_wtu, type = 3))

# --- ANCOVA for PERCEIVED EFFORT (H2 Initial Check) ---
ancova_effort <- aov(Effort ~ Attribution_IV * AI_Use_IV + Q25 + Q26, 
                     data = data_long)
print("--- ANCOVA: Perceived EFFORT (H2 Check) ---")
print(Anova(ancova_effort, type = 3))

