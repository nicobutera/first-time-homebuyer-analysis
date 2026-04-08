## Marketing Analytics Project

# Step 1: Load + Quick Audit

install.packages(c("tidyverse", "janitor", "skimr", "ggplot2"))

library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)

# Load the CSV
df_raw <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
str(df_raw)
colnames(df_raw)
print(dim(df_raw))

# Standardize column names (lowercase, underscores)
df <- df_raw %>% janitor::clean_names()
colnames(df)

# Basic structure checks
cat("Rows, Cols:\n")
print(dim(df))

cat("\nColumn types (glimpse):\n")
glimpse(df)

cat("\nOverall missing values in entire dataset:\n")
print(sum(is.na(df)))

# Confirm target variable exists and how it's coded
# (Option A target: borrower_first_time_homebuyer)
cat('\nTarget value counts:\n"')
print(table(df$borrower_first_time_homebuyer, useNA = "ifany"))

# Convert target to factor labels (No/Yes) for modeling/report tables
df <- df %>%
  mutate(
    borrower_first_time_homebuyer = factor(
      borrower_first_time_homebuyer,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

cat("\nTarget after labeling:\n")
print(table(df$borrower_first_time_homebuyer))

# Select the columns and audit them
model_cols <- c(
  "borrower_first_time_homebuyer",
  "borrower1age_at_application_years",
  "total_monthly_income_amount",
  "ltv_ratio_percent",
  "total_debt_expense_ratio_percent",
  "note_amount",
  "note_rate_percent",
  "mortgage_type",
  "loan_purpose_type"
)

model_df <- df %>% select(all_of(model_cols))
colnames(model_df)

cat("\nMissing values by model column\n")
print(colSums(is.na(model_df)))

cat("\nQuick numeric summaries:\n")
print(summary(model_df %>% select(
  borrower1age_at_application_years,
  total_monthly_income_amount,
  ltv_ratio_percent,
  total_debt_expense_ratio_percent,
  note_amount,
  note_rate_percent
)))

# Label coded categorical variables for readability in charts/tables
model_df <- model_df %>%
  mutate(
    loan_purpose_label = case_when(
      loan_purpose_type == 1 ~ "Purchase",
      loan_purpose_type == 2 ~ "No-Cash Out Refi",
      loan_purpose_type == 6 ~ "Cash-Out Refi",
      TRUE ~ "Other"
    ),
    mortgage_type_label = case_when(
      mortgage_type == 0 ~ "Conventional",
      mortgage_type == 1 ~ "FHA",
      mortgage_type == 2 ~ "VA",
      mortgage_type == 3 ~ "USDA Rural Housing (FSA Guaranteed)",
      TRUE ~ "Other"
    )
  )

cat("\nLoan purpose counts:\n")
print(table(model_df$loan_purpose_label, useNA = "ifany"))

cat("\nMortgage type counts:\n")
print(table(model_df$mortgage_type_label, useNA = "ifany"))

# Step 2: EDA (1 summary table + 2-3 plots)

install.packages(c("tidyverse","scales"))

library(tidyverse)
library(scales)

# A) Summary table (Report)
eda_table <- model_df %>% 
  group_by(borrower_first_time_homebuyer) %>% 
  summarise(
    n = n(),
    age_mean = mean(borrower1age_at_application_years),
    age_median = median(borrower1age_at_application_years),
    income_mean = mean(total_monthly_income_amount),
    income_median = median(total_monthly_income_amount),
    ltv_mean = mean(ltv_ratio_percent),
    ltv_median = median(ltv_ratio_percent),
    dti_mean = mean(total_debt_expense_ratio_percent),
    dti_median = median(total_debt_expense_ratio_percent),
    note_amt_mean = mean(note_amount),
    note_amt_median = median(note_amount),
    rate_mean = mean(note_rate_percent),
    rate_median = median(note_rate_percent),
    .groups = "drop"
  ) %>% 
  mutate(
    income_mean = dollar(income_mean),
    income_median = dollar(income_median),
    note_amt_mean = dollar(note_amt_mean),
    note_amt_median = dollar(note_amt_median),
    age_mean = round(age_mean, 2),
    age_median = round(age_median, 2),
    ltv_mean = round(ltv_mean, 2),
    ltv_median = round(ltv_median, 2),
    dti_mean = round(dti_mean, 2),
    dti_median = round(dti_median, 2),
    rate_mean = round(rate_mean, 3),
    rate_median = round(rate_median, 3)
  )

print(eda_table, width = Inf)

# B) Plots (2-3)

# 1) Age Distribution
ggplot(model_df, aes(x = borrower1age_at_application_years, fill = borrower_first_time_homebuyer)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(
    title = "Age Distribution by First-Time Homebuyer Status",
    x = "Borrower Age at Application (Years)",
    y = "Count",
    fill = "First-Time?"
  ) +
  theme_minimal()

ggsave("age_distribution.pdf", width = 8, height = 6)

# Income Distribution
ggplot(model_df, aes(x = total_monthly_income_amount, fill = borrower_first_time_homebuyer)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.5) +
  scale_x_log10(labels = comma) +
  labs(
    title = "Monthly Income Distribution by First-Time Homebuyer Status (Log Scale)",
    x = "Total Monthly Income (log10 scale)",
    y = "Count",
    fill = "First-Time?"
  ) +
  theme_minimal()

ggsave("income_distribution.pdf", width = 8, height = 6)

# Mortgage type share (percent)
mortgage_share <- model_df %>% 
  count(borrower_first_time_homebuyer, mortgage_type_label) %>% 
  group_by(borrower_first_time_homebuyer) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup()

print(mortgage_share)

ggplot(mortgage_share, aes(x = borrower_first_time_homebuyer, y = pct, fill = mortgage_type_label)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Mortgage Type Mix by First-Time Homebuyer Status",
    x = "First-Time Homebuyer",
    y = "Share of Loans",
    fill = "Mortage Type"
  ) +
  theme_minimal()

mortgage_share %>% 
  filter(mortgage_type_label != "Conventional") %>% 
  ggplot(aes(x = borrower_first_time_homebuyer, y = pct, fill = mortgage_type_label)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Non-Conventional Mortgage Types by First-Time Homebuyer Status",
    x = "First-Time Homebuyer",
    y = "Share of Loans",
    fill = "Mortgage Type"
  ) +
  theme_minimal()

ggsave("mortgage_type_fthb.pdf", width = 8, height = 6)

# Step 3: Logistic Regression
library(tidyverse)

# 1) Make sure categorical variables are factors
model_df <- model_df %>% 
  mutate(
    mortgage_type_label = factor(mortgage_type_label),
    loan_purpose_label = factor(loan_purpose_label)
  )

# Set reference categories so interpretation is clean
model_df <- model_df %>% 
  mutate(
    mortgage_type_label = relevel(mortgage_type_label, ref = "Conventional"),
    loan_purpose_label = relevel(loan_purpose_label, ref = "Purchase")
  )

# 2) Fit logistic regression model
logit_fit <- glm(
  borrower_first_time_homebuyer ~
    borrower1age_at_application_years +
    total_monthly_income_amount +
    ltv_ratio_percent +
    total_debt_expense_ratio_percent +
    note_amount +
    note_rate_percent +
    mortgage_type_label +
    loan_purpose_label,
  data = model_df,
  family = binomial()
)

summary(logit_fit)

# 3) Odds ratios + 95% CI table (much easier to interpret than raw log-odds)
or_table <- tibble(
  term = names(coef(logit_fit)),
  estimate = coef(logit_fit),
  OR = exp(coef(logit_fit))
) %>% 
  # Add Wald 95% CI
  mutate(
    se = sqrt(diag(vcov(logit_fit))),
    CI_low = exp(estimate - 1.96 * se),
    CI_high = exp(estimate + 1.96 * se)
  ) %>% 
  select(term, OR, CI_low, CI_high) %>% 
  arrange(desc(OR))

print(or_table, n = 50)

# The ORs for note amount and total monthly income are measured in 1 dollar units, so a "1 unit" change is tiny
# Rescale the money variables. For income, interpret them per $1,000, and for loan amount per $10,000.
names(model_df)

model_df2 <- model_df %>% 
  mutate(
    income_k = total_monthly_income_amount / 1000,
    loan_10k = note_amount / 10000
  )

names(model_df2)

logit_fit2 <- glm(
  borrower_first_time_homebuyer ~
    borrower1age_at_application_years +
    income_k +
    ltv_ratio_percent +
    total_debt_expense_ratio_percent +
    loan_10k +
    note_rate_percent +
    mortgage_type_label +
    loan_purpose_label,
  data = model_df2,
  family = binomial()
)

# Odds ratios for the rescaled model
or_table2 <- tibble(
  term = names(coef(logit_fit2)),
  estimate = coef(logit_fit2),
  OR = exp(coef(logit_fit2)),
  se = sqrt(diag(vcov(logit_fit2)))
) %>% 
  mutate(
    CI_low = exp(estimate - 1.96 * se),
    CI_high = exp(estimate + 1.96 * se)
  ) %>% 
  select(term, OR, CI_low, CI_high) %>% 

print(or_table2, n = 50)

or_table2_sorted <- or_table2 %>% arrange(desc(OR))
print(or_table2_sorted, n = 50)

# 4) Simple model performance (train-set accuracy + confusion matrix)
model_df2$pred_prob <- predict(logit_fit2, type = "response")

# Choose a threshold; 0.5 is standard
threshold <- 0.5
model_df2$pred_class <- ifelse(model_df2$pred_prob >= threshold, "Yes", "No") %>% 
  factor(levels = c("No", "Yes"))

actual <- model_df2$borrower_first_time_homebuyer

conf_mat <- table(Predicted = model_df2$pred_class, Actual = actual)
print(conf_mat)

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("\nAccuracy:", round(accuracy, 4), "\n")

# 5) AUC
install.packages("pROC")
library(pROC)

roc_obj <- roc(actual, model_df2$pred_prob, levels = c("No", "Yes"), direction = "<")
cat("AUC:", round(as.numeric(auc(roc_obj)), 4), "\n")

# Help put tables into Word
# Medians table
eda_medians <- eda_table %>% 
  select(
    borrower_first_time_homebuyer, n,
    age_median, income_median, ltv_median, dti_median,
    note_amt_median, rate_median
  )

write.csv(eda_medians, "Table_1_Medians_by_FirstTimeStatus.csv", row.names = FALSE)

# Means table
eda_means <- eda_table %>% 
  select(
    borrower_first_time_homebuyer, n,
    age_mean, income_mean, ltv_mean, dti_mean,
    note_amt_mean, rate_mean
  )

write.csv(eda_means, "Table_2_Means_by_FirstTimeStatus.csv", row.names = FALSE)

# OR table
or_report <- or_table2_sorted %>% 
  filter(term != "(Intercept)") %>% 
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3)
  )

write.csv(or_report, "Table_OR_OddsRatios.csv", row.names = FALSE)





