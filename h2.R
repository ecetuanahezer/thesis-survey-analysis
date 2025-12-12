print("--- STEP 1: TOTAL EFFECT (PATH C) ---")
# We already saw the difference between Creativity ~ AI_Use_IV in the ANOVA (F=1.23, p=0.26736 - Not significant)
# But this can be confirmed with a simple regression.
summary(lm(Creativity ~ AI_Use_IV, data = data_long))

print("--- STEP 2: IV -> MEDIATOR (PATH A) ---")
summary(lm(Effort ~ AI_Use_IV, data = data_long))

print("--- STEP 3: MEDIATOR -> DV (PATH B) ---")
summary(lm(Creativity ~ Effort, data = data_long))

print("--- STEP 4: MEDIATOR CONTROL (PATH C') ---")
regression_c_prime <- summary(lm(Creativity ~ AI_Use_IV + Effort, data = data_long))
print(regression_c_prime)

# Convert factor variables to numeric contrast codes for interaction term calculation
data_long <- data_long %>% mutate(
    # Create simple numeric codes (e.g., 0/1) for each factor
    Attribution_Code = if_else(Attribution_IV == "Self", 1, 0),
    AI_Use_Code = if_else(AI_Use_IV == "AI_Assisted", 1, 0),
    
    # Create the Interaction Term (Attribution x AI_Use)
    Interaction_Term = Attribution_Code * AI_Use_Code
  )
library(lavaan)

mediation_multifactorial_syntax <- '
  # PATH A: The Mediator (Effort) is predicted by both IVs and their Interaction.
  # We are interested in the indirect effect through the Interaction_Term.
  Effort ~ a1*Attribution_Code + a2*AI_Use_Code + a3*Interaction_Term 

  # PATH B: DV (Creativity) is predicted by the Mediator (Effort) AND the IVs (for control)
  Creativity ~ b*Effort + c1_prime*Attribution_Code + c2_prime*AI_Use_Code + c3_prime*Interaction_Term

  # DEFINED INDIRECT EFFECTS: The indirect effect through the Interaction (a3*b)
  Indirect_Interaction := a3 * b
'
mediation_multifactorial_syntax2 <- '
  # PATH A: The Mediator (Effort) is predicted by both IVs and their Interaction.
  # We are interested in the indirect effect through the Interaction_Term.
  Effort ~ a1*Attribution_Code + a2*AI_Use_Code + a3*Interaction_Term 

  # PATH B: DV (Creativity) is predicted by the Mediator (Effort) AND the IVs (for control)
  Quality ~ b*Effort + c1_prime*Attribution_Code + c2_prime*AI_Use_Code + c3_prime*Interaction_Term

  # DEFINED INDIRECT EFFECTS: The indirect effect through the Interaction (a3*b)
  Indirect_Interaction := a3 * b
'
mediation_multifactorial_syntax3 <- '
  # PATH A: The Mediator (Effort) is predicted by both IVs and their Interaction.
  # We are interested in the indirect effect through the Interaction_Term.
  Effort ~ a1*Attribution_Code + a2*AI_Use_Code + a3*Interaction_Term 

  # PATH B: DV (Creativity) is predicted by the Mediator (Effort) AND the IVs (for control)
  Willingness ~ b*Effort + c1_prime*Attribution_Code + c2_prime*AI_Use_Code + c3_prime*Interaction_Term

  # DEFINED INDIRECT EFFECTS: The indirect effect through the Interaction (a3*b)
  Indirect_Interaction := a3 * b
'
print("--- MULTIFACTORIAL MEDIATION (H2) START ---")

mediation_fit_multifactorial <- sem(
  mediation_multifactorial_syntax,
  data = data_long,
  se = "bootstrap",     # Use Bootstrapping
  bootstrap = 5000      
)
mediation_fit_multifactorial2 <- sem(
  mediation_multifactorial_syntax2,
  data = data_long,
  se = "bootstrap",     # Use Bootstrapping
  bootstrap = 5000      
)
mediation_fit_multifactorial3 <- sem(
  mediation_multifactorial_syntax3,
  data = data_long,
  se = "bootstrap",     # Use Bootstrapping
  bootstrap = 5000      
)
# Output the key parameters, focusing on the indirect effect
parameterestimates(mediation_fit_multifactorial, 
                   level = 0.95, 
                   boot.ci.type = "perc")


# Assuming mediation_fit_multifactorial is already in memory from your previous run.

# 1. REPORT PATH A (IVs -> Mediator: Effort)
# We look at the top section of the output for the Effort regressions
print("--- PATH A (IVs -> EFFORT) ---")
parameterestimates(mediation_fit_multifactorial, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(lhs == "Effort")
print("--- PATH A (IVs -> EFFORT) ---")
parameterestimates(mediation_fit_multifactorial2, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(lhs == "Effort")
# 2a. REPORT PATH B & C' (Mediator & IVs -> DV: Creativity)
# We look at the middle section for the Creativity regressions
print("--- PATH B & C' (EFFORT & IVs -> CREATIVITY) ---")
parameterestimates(mediation_fit_multifactorial, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(lhs == "Creativity")
# 2b. REPORT PATH B & C' (Mediator & IVs -> DV: Quality)
# We look at the middle section for the Quality regressions
print("--- PATH B & C' (EFFORT & IVs -> Quality) ---")
parameterestimates(mediation_fit_multifactorial2, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(lhs == "Quality")
# 2c. REPORT PATH B & C' (Mediator & IVs -> DV: Willingness)
# We look at the middle section for the Willingness regressions
print("--- PATH B & C' (EFFORT & IVs -> Willingness) ---")
parameterestimates(mediation_fit_multifactorial3, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(lhs == "Willingness")
# 3a. REPORT INDIRECT EFFECT (H2 Test)
print("--- INDIRECT EFFECT (H2 TEST) ---")
parameterestimates(mediation_fit_multifactorial, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(label == "Indirect_Interaction")
# 3b. REPORT INDIRECT EFFECT (H2 Test)
print("--- INDIRECT EFFECT (H2 TEST) ---")
parameterestimates(mediation_fit_multifactorial2, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(label == "Indirect_Interaction")
# 3c. REPORT INDIRECT EFFECT (H2 Test)
print("--- INDIRECT EFFECT (H2 TEST) ---")
parameterestimates(mediation_fit_multifactorial3, 
                   level = 0.95, 
                   #se = "bootstrap",
                   boot.ci.type = "perc") %>%
  filter(label == "Indirect_Interaction")
