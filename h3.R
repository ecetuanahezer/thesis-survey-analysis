library(dplyr)
library(ggplot2)
library(car)

# Calculate the median for all participants
median_attitude <- median(data_long$Attitude_C, na.rm = TRUE)

# Create the Categorical Attitude variable
data_long <- data_long %>%
  mutate(
    Attitude_Category = factor(
      # Assign 'High_Attitude' if score is greater than or equal to the median, 'Low_Attitude' otherwise
      if_else(Attitude_C >= median_attitude, "High_Attitude", "Low_Attitude"),
      # Define factor levels
      levels = c("Low_Attitude", "High_Attitude")
    )
  )

# 1a. Run the ANOVA Model (Testing H3)
# The asterisk (*) includes all main effects, 2-way interactions, and the 3-way interaction.
anova_model_h3a <- aov(Creativity ~ Attribution_IV * AI_Use_IV * Attitude_Category, 
                      data = data_long)
# 1b. Run the ANOVA Model (Testing H3)
# The asterisk (*) includes all main effects, 2-way interactions, and the 3-way interaction.
anova_model_h3b <- aov(Quality ~ Attribution_IV * AI_Use_IV * Attitude_Category, 
                      data = data_long)
# 1c. Run the ANOVA Model (Testing H3)
# The asterisk (*) includes all main effects, 2-way interactions, and the 3-way interaction.
anova_model_h3c <- aov(Willingness ~ Attribution_IV * AI_Use_IV * Attitude_Category, 
                      data = data_long)# 2a. View Results (Type III Sum of Squares is recommended)
print("--- H3 HYPOTHESIS: 3-WAY ANOVA RESULTS ---")
Anova(anova_model_h3a, type = 3)

print("--- H3 HYPOTHESIS: 3-WAY ANOVA RESULTS ---")
Anova(anova_model_h3b, type = 3)

print("--- H3 HYPOTHESIS: 3-WAY ANOVA RESULTS ---")
Anova(anova_model_h3c, type = 3)
# --- SIMPLE MAIN EFFECTS ANALYSIS ---

# Filter data for the Low Attitude group only
data_low_attitude <- data_long %>% filter(Attitude_Category == "Low_Attitude")

print("--- LOW ATTITUDE GROUP: HUMAN vs. AI CREATIVITY ---")
# Test for mean difference between Human-Written and AI-Assisted within the Low Attitude group
t.test(Creativity ~ AI_Use_IV, data = data_low_attitude)
t.test(Quality ~ AI_Use_IV, data = data_low_attitude)
t.test(Willingness ~ AI_Use_IV, data = data_low_attitude)
# Filter data for the High Attitude group only
data_high_attitude <- data_long %>% filter(Attitude_Category == "High_Attitude")

print("--- HIGH ATTITUDE GROUP: HUMAN vs. AI CREATIVITY ---")
# Test for mean difference between Human-Written and AI-Assisted within the High Attitude group
t.test(Creativity ~ AI_Use_IV, data = data_high_attitude)
t.test(Quality ~ AI_Use_IV, data = data_high_attitude)
t.test(Willingness ~ AI_Use_IV, data = data_high_attitude)


# --- INTERACTION VISUALIZATION (Simple Slopes Plot) ---

# Calculate Mean and Standard Error (SE) for plotting
summary_data <- data_long %>%
  group_by(AI_Use_IV, Attitude_Category) %>%
  summarise(
    Mean_Creativity = mean(Creativity, na.rm = TRUE),
    SE = sd(Creativity, na.rm = TRUE) / sqrt(n())
  )

# Create the Interaction Plot (Line Plot with Error Bars)
ggplot(summary_data, aes(x = AI_Use_IV, y = Mean_Creativity, group = Attitude_Category, color = Attitude_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_Creativity - SE, ymax = Mean_Creativity + SE), width = 0.1) +
  labs(
    title = "Moderating Effect of AI Attitude on AI Use",
    x = "Text Type",
    y = "Mean Perceived Creativity",
    color = "AI Attitude Category"
  ) +
  theme_minimal()

#quality
summary_data <- data_long %>%
  group_by(AI_Use_IV, Attitude_Category) %>%
  summarise(
    Mean_Quality = mean(Quality, na.rm = TRUE),
    SE = sd(Quality, na.rm = TRUE) / sqrt(n())
  )

# Create the Interaction Plot (Line Plot with Error Bars)
ggplot(summary_data, aes(x = AI_Use_IV, y = Mean_Quality, group = Attitude_Category, color = Attitude_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_Quality - SE, ymax = Mean_Quality + SE), width = 0.1) +
  labs(
    title = "Moderating Effect of AI Attitude on AI Use",
    x = "Text Type",
    y = "Mean Perceived Quality",
    color = "AI Attitude Category"
  ) +
  theme_minimal()

#willingness
summary_data <- data_long %>%
  group_by(AI_Use_IV, Attitude_Category) %>%
  summarise(
    Mean_Willingness = mean(Willingness, na.rm = TRUE),
    SE = sd(Willingness, na.rm = TRUE) / sqrt(n())
  )

# Create the Interaction Plot (Line Plot with Error Bars)
ggplot(summary_data, aes(x = AI_Use_IV, y = Mean_Willingness, group = Attitude_Category, color = Attitude_Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_Willingness - SE, ymax = Mean_Willingness + SE), width = 0.1) +
  labs(
    title = "Moderating Effect of AI Attitude on AI Use",
    x = "Text Type",
    y = "Mean Perceived Willingness",
    color = "AI Attitude Category"
  ) +
  theme_minimal()


# Create the 2x2 Factor
summary_2x2 <- data_long %>%
  group_by(Attribution_IV, AI_Use_IV) %>%
  summarise(Mean_Quality = mean(Quality, na.rm = TRUE), SE = sd(Quality, na.rm = TRUE) / sqrt(n()))

ggplot(summary_2x2, aes(x = Attribution_IV, y = Mean_Quality, fill = AI_Use_IV)) +
  geom_bar(stat = "identity", position = position_dodge(0.8)) + # Creates the bars
  geom_errorbar(aes(ymin = Mean_Quality - SE, ymax = Mean_Quality + SE), 
                width = 0.2, position = position_dodge(0.8)) +
  labs(
    title = "Self-Other Bias in Perceived Quality",
    x = "Attribution (Who Created the Text)",
    y = "Mean Perceived Quality",
    fill = "Text Type"
  ) +
  theme_minimal()
