# --------------------------------------------------
# Author: Zacharia Makari
# Project: Anchor House GPA Study (UC Berkeley)
# Contact: zacharia.makari@berkeley.edu
# Last Updated: 05/16/2025
# Description: Full pipeline for statistical and ML-based analysis of dorm orientation and GPA outcomes.
# --------------------------------------------------

# CITATIONS:
# R Core Team (2024). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
# Wickham H, François R, Henry L, Müller K (2023). dplyr: A Grammar of Data Manipulation.
# Makowski D, Ben-Shachar M, Lüdecke D (2022). modelsummary: Summary tables and plots for statistical models. 
# Therneau T, Atkinson B (2019). rpart: Recursive Partitioning and Regression Trees. 
# Kassambara A (2020). ggplot2: Elegant Graphics for Data Analysis. 
# Lumley T (2023). tableone: Create 'Table 1' to Describe Baseline Characteristics. 
# Robinson D, Hayes A, Couch S, et al. (2024). broom: Convert statistical analysis objects into tidy tibbles. 
# Thomas J. Leeper (2023). margins: Marginal Effects for Model Objects.
# Wickham H (2022). stringr: Simple, Consistent Wrappers for Common String Operations. 
# Milborrow S (2022). rpart.plot: Plot 'rpart' Models.
# --------------------------------------------------

# -------------------------------
# 1. LOAD PACKAGES AND DATA
# -------------------------------

library(dplyr)
library(modelsummary)
library(rpart)
library(ggplot2)
library(tableone)
library(broom)
library(margins)
library(stringr)
library(rpart.plot)

# -------------------------------
# 2. DATA CLEANING & VARIABLE ENGINEERING
# -------------------------------

raw <- read.csv("Anchor_GPA.csv", skip = 1, header = FALSE)
headers <- as.character(unlist(raw[1, ]))
data <- raw[-1, ]
colnames(data) <- headers
rownames(data) <- NULL


data_clean <- data[, c(
  "What is your cumulative BERKELEY GPA (specific).",
  "What was your COMMUNITY COLLEGE GPA (range estimate).",
  "Inside or Outside facing dorm?",
  "Including Spring 2025, how many semesters have you lived in a dorm at Berkeley.",
  "What is your Major?",
  "What floor are you on."
)]


colnames(data_clean) <- c("GPA", "CC_GPA", "Orientation", "Semesters", "Major", "Floor")
data_clean$GPA <- as.numeric(data_clean$GPA)
data_clean$Floor <- as.numeric(data_clean$Floor)
data_clean$Semesters <- as.numeric(data_clean$Semesters)


data_clean <- subset(data_clean, !is.na(GPA))
data_clean$Orientation <- factor(data_clean$Orientation, levels = c("Inside", "Outside"))


data_clean$CC_GPA_Num <- NA
data_clean$CC_GPA_Num[data_clean$CC_GPA == "2.5-3.0"]   <- 2.75
data_clean$CC_GPA_Num[data_clean$CC_GPA == "3.0-3.3"]   <- 3.15
data_clean$CC_GPA_Num[data_clean$CC_GPA == "3.3-3.7"]   <- 3.5
data_clean$CC_GPA_Num[data_clean$CC_GPA == "3.7-3.99"]  <- 3.845
data_clean$CC_GPA_Num[data_clean$CC_GPA == "4.00"]      <- 4.0


data_clean$Discipline <- case_when(
  str_detect(tolower(data_clean$Major), "eecs|computer science|cs|physics|math|statistics|chemistry|bio|mcb|integrative biology|environmental biology|neuro|data science|engineering") ~ "STEM",
  str_detect(tolower(data_clean$Major), "business|marketing") ~ "Business",
  str_detect(tolower(data_clean$Major), "economics|econ|political science|psychology|sociology|cognitive|legal|geography|social welfare") ~ "Social Sciences",
  str_detect(tolower(data_clean$Major), "architecture|urban studies|media studies|art history") ~ "Arts/Design",
  str_detect(tolower(data_clean$Major), "english|history|philosophy|film|rhetoric|literature|theater|comp lit|spanish") ~ "Humanities",
  TRUE ~ "Other"
)


data_clean$Discipline <- relevel(as.factor(data_clean$Discipline), ref = "Social Sciences")


data_clean$GPA_Bucket <- cut(
  data_clean$GPA,
  breaks = c(2.1, 2.7, 3.7, 4.0),
  labels = c("C", "B", "A"),
  include.lowest = TRUE,
  right = TRUE
)


unique(data_clean$GPA[is.na(data_clean$GPA)])
summary(data_clean$GPA)
summary(data_clean$CC_GPA_Num)
data_clean$High_GPA <- ifelse(data_clean$GPA >= 3.5, 1, 0)


# -------------------------------
# 3. BIVARIATE ANALYSIS (OLS + Chi-Squared)
# -------------------------------

short_model <- lm(GPA ~ Orientation, data = data_clean)
summary(short_model)


table_chi <- table(data_clean$GPA_Bucket, data_clean$Orientation)
print(table_chi)


chisq.test(table_chi)
fisher.test(table_chi)

# -------------------------------
# 4. FULL MULTIVARIATE REGRESSION
# -------------------------------

full_model <- lm(GPA ~ Orientation + CC_GPA_Num + Semesters + Floor + Discipline, data = data_clean)
summary(full_model)


sink("full_model_summary.txt")
summary(full_model)
sink()



summary(lm(CC_GPA_Num ~ Orientation, data = data_clean))


variable_labels <- c(
  "OrientationOutside" = "Outside-facing Room",
  "CC_GPA_Num" = "Prior GPA",
  "Semesters" = "Semesters in Dorm",
  "Floor" = "Floor Number",
  "DisciplineArts/Design" = "Arts Major",
  "DisciplineBusiness" = "Business Major",
  "DisciplineHumanities" = "Humanities Major",
  "DisciplineOther" = "Other Major",
  "DisciplineSTEM" = "STEM Major"
)


modelsummary(
  list(
    "Bivariate Model" = short_model,
    "Full Model" = full_model
  ),
  coef_map = variable_labels,
  stars = TRUE,
  gof_omit = "IC|Log|Adj|RMSE",
  conf_level = 0.95,
  output = "regression_table.html"
)

# -------------------------------
# 5. COVARIATE BALANCE TABLE
# -------------------------------

covariates <- c("CC_GPA_Num", "Semesters", "Floor", "Discipline")
table1 <- CreateTableOne(
  vars = covariates, 
  strata = "Orientation", 
  data = data_clean, 
  factorVars = "Discipline"
)
print(table1, showAllLevels = TRUE, test = TRUE)

# -------------------------------
# 6. INTERACTION EFFECT MODEL
# -------------------------------


interaction_model <- lm(GPA ~ Orientation * CC_GPA_Num + Semesters + Floor + Discipline, data = data_clean)
summary(interaction_model)


sink("interaction_model_summary.txt")
summary(interaction_model)
sink()


# -------------------------------
# 7. DECISION TREE CLASSIFIER
# -------------------------------


tree_model <- rpart(
  High_GPA ~ Orientation + CC_GPA_Num + Semesters + Floor + Discipline,
  data = data_clean,
  method = "class",
  control = rpart.control(cp = 0.01)
)



png("tree_plot.png", width = 800, height = 600, bg = "white")
rpart.plot(
  tree_model,
  type = 2,
  extra = 106,
  fallen.leaves = TRUE,
  box.palette = "RdBu",
  main = paste0("Decision Tree: Predicting High GPA (n = ", nrow(data_clean), ")")
)
dev.off()


importance <- sort(tree_model$variable.importance)
png("tree_variable_importance.png", width = 800, height = 600)
par(mar = c(5, 6, 4, 2))
barplot(importance,
        horiz = TRUE,
        las = 1,                    
        col = "steelblue",
        cex.names = 0.9,           
        main = "Variable Importance in Decision Tree",
        xlab = "Importance Score",
        space = 0.5                  
)

dev.off()


pred <- predict(tree_model, type = "class")

table(Predicted = pred, Actual = data_clean$High_GPA)

mean(pred == data_clean$High_GPA)

printcp(tree_model)
plotcp(tree_model)
png("tree_cp_plot.png", width = 700, height = 500)
plotcp(tree_model)
dev.off()

# -------------------------------
# DECISION TREE CLASSIFIER (ENVIRONMENTAL VARIABLES ONLY)
# -------------------------------

env_tree_model <- rpart(
  High_GPA ~ Orientation + Semesters + Floor,
  data = data_clean,
  method = "class",
  control = rpart.control(cp = 0.01)
)


png("env_tree_plot.png", width = 800, height = 600, bg = "white")
rpart.plot(
  env_tree_model,
  type = 2,
  extra = 106,
  fallen.leaves = TRUE,
  box.palette = "GnBu",
  main = paste0("Environmental-Only Decision Tree (n = ", nrow(data_clean), ")")
)
dev.off()


env_importance <- sort(env_tree_model$variable.importance)

png("env_tree_variable_importance.png", width = 800, height = 600)
par(mar = c(5, 6, 4, 2))
barplot(env_importance,
        horiz = TRUE,
        las = 1,
        col = "darkgreen",
        cex.names = 0.9,
        main = "Variable Importance (Environmental-Only Tree)",
        xlab = "Importance Score",
        space = 0.5
)
dev.off()

env_pred <- predict(env_tree_model, type = "class")
table(Predicted = env_pred, Actual = data_clean$High_GPA)
mean(env_pred == data_clean$High_GPA)

png("env_tree_cp_plot.png", width = 700, height = 500)
plotcp(env_tree_model)
dev.off()

printcp(env_tree_model)


# -------------------------------
# 8. LOGISTIC REGRESSION ROBUSTNESS
# -------------------------------

logit_model <- glm(
  I(GPA >= 3.5) ~ Orientation + CC_GPA_Num + Semesters + Floor + Discipline,
  data = data_clean, 
  family = "binomial")
summary(logit_model)
margins(logit_model)


sink("logit_model_summary.txt")
summary(logit_model)
sink()


# -------------------------------
# 9. VISUALIZATIONS
# -------------------------------

sample_sizes <- table(data_clean$Orientation)
title_text_mean <- paste0("Average GPA by Dorm Orientation (n = ", sample_sizes["Inside"], " inside, ", sample_sizes["Outside"], " outside)")
title_text_box <- paste0("GPA Distribution by Dorm Orientation (n = ", sample_sizes["Inside"], " inside, ", sample_sizes["Outside"], " outside)")
title_text_bar <- paste0("GPA Bucket Distribution by Dorm Orientation (n = ", nrow(data_clean), ")")

# Mean GPA bar chart

plot_mean <-ggplot(data_clean, aes(x = Orientation, y = GPA, fill = Orientation)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6, color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(
    title = title_text_mean,
    x = "Dorm Orientation",
    y = "Average GPA"
  ) +
  theme_minimal()
ggsave("mean_orientation_gpa.png", plot = plot_mean, width = 7, height = 5, bg = "white")


plot_box <- ggplot(data_clean, aes(x = Orientation, y = GPA, fill = Orientation)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  labs(
    title = title_text_box,
    x = "Dorm Orientation",
    y = "GPA"
  ) +
  theme_minimal()
ggsave("gpa_boxplot_orientation.png", plot = plot_box, width = 7, height = 5, bg = "white")


plot_bar <- ggplot(data_clean, aes(x = GPA_Bucket, fill = Orientation)) +
  geom_bar(position = "dodge") +
  labs(title = title_text_bar,
       x = "GPA Range",
       y = "Number of Students"
  ) +
  scale_fill_manual(values = c("Inside" = "#FF6F61", "Outside" = "#00BFC4")) +
  theme_minimal()
ggsave("gpa_bucket_barplot.png", plot = plot_bar, width = 7, height = 5, bg = "white")

# -------------------------------
# 10. DESCRIPTIVE SUMMARY TABLES
# -------------------------------

datasummary_skim(
  data_clean %>% select(GPA, CC_GPA_Num, Semesters, Floor),
  type = "numeric"
  )


table <- data_clean %>%
  select(Major, Discipline) %>%
  distinct() %>%
  arrange(Discipline)
print(table)

# -------------------------------
# 11. SYNTHETIC REPLICATION (ROBUSTNESS CHECK)
# -------------------------------

set.seed(42)
n_simulations <- 100
n_students <- 5000


beta_0 <- 2.7
beta_1 <- 0.196  # Orientation
beta_2 <- 0.504  # CC GPA
beta_3 <- 0.01   # Semesters
beta_4 <- 0.005  # Floor
sigma <- 0.25    # Standard error of residuals


orientation_effects <- numeric(n_simulations)


for (i in 1:n_simulations) {
  sim_orientation <- rbinom(n_students, 1, 0.5)
  sim_ccgpa <- rnorm(n_students, mean = 3.5, sd = 0.25)
  sim_semesters <- sample(1:5, n_students, replace = TRUE)
  sim_floor <- sample(4:14, n_students, replace = TRUE)
  sim_noise <- rnorm(n_students, mean = 0, sd = sigma)
  
  sim_gpa <- beta_0 +
    beta_1 * sim_orientation +
    beta_2 * sim_ccgpa +
    beta_3 * sim_semesters +
    beta_4 * sim_floor +
    sim_noise
  
  
  sim_df <- data.frame(
    GPA = sim_gpa,
    Orientation = sim_orientation
  )
  
  sim_model <- lm(GPA ~ Orientation, data = sim_df)
  orientation_effects[i] <- coef(sim_model)["Orientation"]
}

sim_df$GPA <- pmin(pmax(sim_df$GPA, 2.1), 4.0)

sim_results <- data.frame(Orientation_Effect = orientation_effects)


write.csv(sim_results, "simulated_orientation_effects.csv", row.names = FALSE)


hist_plot <- ggplot(sim_results, aes(x = Orientation_Effect)) +
  geom_histogram(binwidth = 0.01, fill = "#56B4E9", color = "black") +
  geom_vline(xintercept = mean(orientation_effects), color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Simulated Orientation Effects (n = 5000, 100 Runs)",
    x = "Estimated Coefficient on Orientation",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("synthetic_orientation_effects_histogram.png", plot = hist_plot, width = 7, height = 5, dpi = 300, bg = "white")


# -------------------------------
# 12. SESSION INFO (APPENDIX)
# -------------------------------

sessionInfo()


sink("session_info.txt")
sessionInfo()
sink()


modelsummary(
  list(
    "Bivariate Model" = short_model,
    "Full Model" = full_model
  ),
  coef_map = variable_labels,
  stars = TRUE,
  gof_omit = "IC|Log|Adj|RMSE",
  conf_level = 0.95,
  output = "regression_table.docx"
)
