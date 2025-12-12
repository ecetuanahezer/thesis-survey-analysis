library(car) # Required for Type III tests

# --- ANOVA for CREATIVITY ---
anova_creativity <- aov(Creativity ~ Attribution_IV * AI_Use_IV, data = data_long)
print("--- ANOVA: Perceived CREATIVITY (Attribution x AI Use) ---")
print(Anova(anova_creativity, type = 3))

# --- ANOVA for QUALITY ---
anova_quality <- aov(Quality ~ Attribution_IV * AI_Use_IV, data = data_long)
print("--- ANOVA: Perceived QUALITY (Attribution x AI Use) ---")
print(Anova(anova_quality, type = 3))

# --- ANOVA for WILLINGNESS TO USE (WTU) ---
anova_wtu <- aov(Willingness ~ Attribution_IV * AI_Use_IV, data = data_long)
print("--- ANOVA: WILLINGNESS TO USE (Attribution x AI Use) ---")
print(Anova(anova_wtu, type = 3))

# --- ANOVA for PERCEIVED EFFORT (Mediator) ---
anova_effort <- aov(Effort ~ Attribution_IV * AI_Use_IV, data = data_long)
print("--- ANOVA: Perceived EFFORT (Attribution x AI Use) ---")
print(Anova(anova_effort, type = 3))

# simple effects analysis
# 1. Effect of AI Use within the SELF condition (Self-AI vs Self-Human)
print("--- QUALITY: SIMPLE EFFECT IN SELF CONDITION ---")
t.test(Quality ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Self"))

# 2. Effect of AI Use within the OTHER condition (Other-AI vs Other-Human)
print("--- QUALITY: SIMPLE EFFECT IN OTHER CONDITION ---")
t.test(Quality ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Other"))

# 3. Effect of AI Use within the SELF condition (Self-AI vs Self-Human)
print("--- WTU: SIMPLE EFFECT IN SELF CONDITION ---")
t.test(Willingness ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Self"))

# 4. Effect of AI Use within the OTHER condition (Other-AI vs Other-Human)
print("--- WTU: SIMPLE EFFECT IN OTHER CONDITION ---")
t.test(Willingness ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Other"))

# 5. Effect of AI Use within the SELF condition (Self-AI vs Self-Human)
print("--- CREATİVİTY: SIMPLE EFFECT IN SELF CONDITION ---")
t.test(Creativity ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Self"))

# 6. Effect of AI Use within the OTHER condition (Other-AI vs Other-Human)
print("--- CREATIVITY: SIMPLE EFFECT IN OTHER CONDITION ---")
t.test(Creativity ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Other"))

# 7. Effect of AI Use within the SELF condition (Self-AI vs Self-Human)
print("--- EFFORT: SIMPLE EFFECT IN SELF CONDITION ---")
t.test(Effort ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Self"))

# 8. Effect of AI Use within the OTHER condition (Other-AI vs Other-Human)
print("--- EFFORT: SIMPLE EFFECT IN OTHER CONDITION ---")
t.test(Effort ~ AI_Use_IV, 
       data = subset(data_long, Attribution_IV == "Other"))
