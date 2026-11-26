###############################################################################
# Replication: Edelman, Luca & Svirsky (2017)
# "Racial Discrimination in the Sharing Economy: Evidence from a Field Experiment"
# AEJ: Applied Economics 9(2): 1-22
# Group: Lars Baenziger, Alessandro van Ede, Jelena Naef and Jeremy Stieger
#
# Master script: runs the whole analysis from the raw data.
#   PART 1  Data cleanup (R translation of master_analysis.do)
#   PART 2  Replication: Figure 2, Table 1 (balance), Table 2  -> report Table 1, Figure 1
#   PART 3  Extension:   logit/probit, response types, Manski bounds -> report Tables 2 and 3
#
# Needed packages (install once):
# install.packages(c("haven", "readxl", "dplyr", "ggplot2", "sandwich", "lmtest",
#                    "marginaleffects", "MASS"))
###############################################################################

library(haven)            # read .dta
library(readxl)           # read .xlsx
library(dplyr)
library(ggplot2)
library(sandwich)         # clustered standard errors
library(lmtest)           # coeftest()
library(marginaleffects)  # average marginal effects (logit / probit)
# MASS::polr() is called with the MASS:: prefix (ordered logit), so MASS is not attached
# (attaching MASS would mask dplyr::select).

# ---- Folder structure (relative paths, no setwd needed) ---------------------
# Open the RStudio project (PS3&4.Rproj) in the main folder. RStudio then sets the
# working directory to that folder automatically, and all paths below are relative to it:
#
#   PS3&4/
#   |-- PS3&4.Rproj
#   |-- README.md
#   |-- replication_airbnb.R          <- this script
#   |-- replication_data/             <- raw data from the authors (never modified)
#   |     main_data.csv, hosts.dta, name_survey_results.xlsx
#   |-- output/                       <- all tables and figures (created by this script)
data_dir   <- "replication_data"
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)


###############################################################################
# PART 1: DATA CLEANUP  (do-file lines 1-320)
###############################################################################

# --- 1.1 Import (do-file l. 11-66) -------------------------------------------
# The csv has no header row, so Stata calls the columns v1...v49 and renames
# them. We read everything as text first (like Stata's "tostring _all").
raw <- read.csv(file.path(data_dir, "main_data.csv"), header = FALSE, colClasses = "character",
                na.strings = character(0))

names(raw) <- c(
  "host_response", "response_date", "number_of_messages", "automated_coding",
  "latitude", "longitude", "bed_type", "property_type", "cancellation_policy",
  "number_guests", "bedrooms", "bathrooms", "cleaning_fee", "price",
  "apt_rating", "property_setup", "city", "date_sent", "listing_down",
  "number_of_listings", "number_of_reviews", "member_since", "verified_id",
  "host_race", "super_host", "host_gender", "host_age", "host_gender_1",
  "host_gender_2", "host_gender_3", "host_race_1", "host_race_2", "host_race_3",
  "guest_first_name", "guest_last_name", "guest_race", "guest_gender",
  "guest_id", "population", "whites", "blacks", "asians", "hispanics",
  "available_september", "up_not_available_september", "september_price",
  "census_tract", "host_id", "new_number_of_listings")

# --- 1.2 Missing values (l. 71-76) --------------------------------------------
# "\N", "Null" and "-1" become missing for the variables response_date ... september_price.
# NOTE: host_response is NOT in this range, so host_response = -1 ("no response")
# stays -1. Also note that "NULL" (upper case) is NOT recoded, just like in Stata.
na_vars <- names(raw)[which(names(raw) == "response_date"):which(names(raw) == "september_price")]
for (v in na_vars) raw[[v]][raw[[v]] %in% c("\\N", "Null", "-1")] <- NA

# --- 1.3 Convert to numbers (l. 79-83) ----------------------------------------
num_vars <- c("host_response", "number_of_messages", "automated_coding", "latitude",
              "longitude", "number_guests", "bedrooms", "bathrooms", "cleaning_fee",
              "price", "apt_rating", "listing_down", "number_of_listings",
              "number_of_reviews", "verified_id", "super_host", "guest_id",
              "population", "whites", "blacks", "hispanics", "asians",
              "available_september", "up_not_available_september",
              "september_price", "host_id", "new_number_of_listings")
for (v in num_vars) raw[[v]] <- as.numeric(raw[[v]])

# --- 1.4 Guest race / gender dummies (l. 86-96) -------------------------------
main <- raw %>%
  mutate(
    response_date_r = as.POSIXct(response_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    date_sent_r     = as.POSIXct(date_sent,     format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    guest_black  = as.numeric(guest_race == "black"),
    guest_white  = as.numeric(guest_black == 0),
    guest_female = as.numeric(guest_gender == "female"),
    guest_male   = as.numeric(guest_gender == "male"),
    # city is "." in Stata when missing
    city = ifelse(is.na(city), ".", city),
    name_by_city = paste0(guest_first_name, city)   # cluster variable
  )

# --- 1.5 Merge the name survey (l. 103-111) -----------------------------------
# Stata's "merge m:1" keeps ALL rows from both files (full join).
# Two details we found in the data:
#   * The csv says "Kareem", the survey says "Kareen" -> Kareem gets no survey score.
#   * "Keisha" and "Kareen" are in the survey but not in the csv -> Stata adds
#     2 (almost empty) rows. They have no outcome, so they drop out of every
#     regression, but they enter some counts in Table 1.
survey <- read_excel(file.path(data_dir, "name_survey_results.xlsx"), sheet = "Sheet1")

df <- main %>%
  full_join(survey, by = "guest_first_name") %>%
  mutate(guest_race_continuous = guest_race_continuous - 1)   # 0 = Black ... 1 = White

# Stata stores missing strings as "" -> treat the added rows the same way
for (v in c("city", "host_race", "host_gender", "host_age", "property_setup",
            "cancellation_policy", "census_tract")) {
  df[[v]][is.na(df[[v]]) & is.na(df$host_response)] <- ""
}
df$census_tract[is.na(df$census_tract)] <- "."   # Stata's "." for recoded missing

# --- 1.6 Host characteristics (l. 114-166) ------------------------------------
# IMPORTANT Stata detail: a missing number counts as +infinity in comparisons.
# So "number_of_reviews >= 10" is TRUE when number_of_reviews is missing!
# We reproduce this on purpose with is.na(x) | ... (see the extension section).

stata_pctile <- function(x, p) quantile(x, p, type = 2, na.rm = TRUE)  # Stata's definition

p90_price <- stata_pctile(df$price, 0.90)
p50_price <- stata_pctile(df$price, 0.50)

df <- df %>%
  mutate(
    host_race_black = as.numeric(host_race %in% "black"),
    host_race_white = as.numeric(host_race %in% "white"),
    host_race_hisp  = as.numeric(host_race %in% "hisp"),
    host_race_asian = as.numeric(host_race %in% "asian"),
    host_race_mult  = as.numeric(host_race %in% "mult"),

    host_gender_F  = as.numeric(host_gender %in% "F"),
    host_gender_FF = as.numeric(host_gender %in% "FF"),
    host_gender_M  = as.numeric(host_gender %in% "M"),
    host_gender_MM = as.numeric(host_gender %in% "MM"),
    host_gender_MF = as.numeric(host_gender %in% "MF"),
    host_gender_same_sex = as.numeric(host_gender_MM == 1 | host_gender_FF == 1),

    host_age_cat = case_when(
      host_age %in% c("young", "young/UU", "UU/young", "young/NA", "NA/young") ~ 0,
      host_age %in% c("middle/young", "young/middle")                         ~ 1,
      host_age %in% c("middle", "middle/UU", "UU/middle", "middle/NA", "NA/middle") ~ 2,
      host_age %in% c("middle/old", "old/middle")                             ~ 3,
      host_age %in% c("old", "old/UU", "UU/old", "old/NA", "NA/old")           ~ 4,
      TRUE ~ NA_real_),

    # "missing = +infinity" rule from Stata:
    ten_reviews       = as.numeric(is.na(number_of_reviews)  | number_of_reviews >= 10),
    multiple_listings = as.numeric(is.na(number_of_listings) | number_of_listings > 1),
    pricey            = as.numeric(is.na(price) | price >= p90_price),
    price_median      = as.numeric(is.na(price) | price >  p50_price),

    # Alternative definition based on the updated listings count (new_number_of_listings).
    # This one reproduces the PUBLISHED Table 2, col. (3) -- see Part 2.
    multiple_listings_new = as.numeric(is.na(new_number_of_listings) | new_number_of_listings > 1),

    five_star_property  = as.numeric(apt_rating %in% 5),
    shared_property     = as.numeric(property_setup %in% c("Private Room", "Shared Room")),
    shared_bathroom     = as.numeric(shared_property == 1 & !is.na(bathrooms) & bathrooms < 1.5),
    has_cleaning_fee    = as.numeric(!is.na(cleaning_fee)),
    strict_cancellation = as.numeric(cancellation_policy %in% "Strict"),
    young  = as.numeric(host_age_cat %in% 0),
    middle = as.numeric(host_age_cat %in% c(1, 2)),
    old    = as.numeric(host_age_cat %in% c(3, 4)),

    log_price = log(price),

    # Stata returns missing when dividing by 0
    white_proportion    = ifelse(population > 0, whites    / population, NA),
    black_proportion    = ifelse(population > 0, blacks    / population, NA),
    asian_proportion    = ifelse(population > 0, asians    / population, NA),
    hispanic_proportion = ifelse(population > 0, hispanics / population, NA)
  ) %>%
  # Number of listings per census tract (l. 165). Missing latitude counts as > 0 in Stata.
  group_by(census_tract) %>%
  mutate(tract_listings = sum(is.na(latitude) | latitude > 0)) %>%
  ungroup() %>%
  mutate(log_tract_listings = log(tract_listings))

# --- 1.7 Outcome variables (l. 193-243) ---------------------------------------
df <- df %>%
  mutate(
    simplified_host_response = case_when(
      host_response == 1  ~ 1,  host_response == 4 ~ 2,  host_response == 6 ~ 3,
      host_response == 8  ~ 4,  host_response == 5 ~ 5,  host_response == 7 ~ 6,
      host_response == 2  ~ 7,  host_response %in% 9:11 ~ 8,
      host_response == -1 ~ 9,  host_response == 3 ~ 10, host_response == 0 ~ 11,
      TRUE ~ NA_real_),        # 12-14 -> missing

    graph_bins = case_when(
      simplified_host_response == 1                     ~ "Yes",
      simplified_host_response %in% 2:6                 ~ "Conditional Yes",
      simplified_host_response == 9                     ~ "No Response",
      simplified_host_response %in% c(7, 8, 10)         ~ "Conditional No",
      simplified_host_response == 11                    ~ "No",
      TRUE ~ NA_character_),

    # Main outcome: positive response
    yes = case_when(
      host_response %in% c(1, 4, 6)                       ~ 1,
      host_response %in% c(0, -1, 2, 3, 7, 8, 9, 10, 11, 12) ~ 0,
      TRUE ~ NA_real_)          # 5, 13, 14 -> missing
  )

# --- 1.8 Drop Tampa and Atlanta, city dummies (l. 290-297) --------------------
df <- df %>%
  filter(!(city %in% c("Tampa", "Atlanta"))) %>%
  mutate(baltimore   = as.numeric(city == "Baltimore"),
         dallas      = as.numeric(city == "Dallas"),
         los_angeles = as.numeric(city == "Los-Angeles"),
         sl          = as.numeric(city == "St-Louis"),
         dc          = as.numeric(city == "Washington"))

# --- 1.9 Merge past-guest data (l. 300-309) -----------------------------------
# keep all rows of the main data (drop _merge == 2  ->  left join)
hosts <- read_dta(file.path(data_dir, "hosts.dta")) %>% select(host_id, total_guests, raw_black, prop_black, any_black)
df <- df %>% left_join(hosts, by = "host_id")

cat("Observations after cleanup:", nrow(df), "\n")
cat("Observations with outcome 'yes':", sum(!is.na(df$yes)), "\n")


###############################################################################
# PART 2: MAIN RESULTS
###############################################################################

# --- Figure 2: Host responses by race of the guest (l. 327-330) --------------
fig2_data <- df %>%
  filter(!is.na(graph_bins), !is.na(guest_black)) %>%
  mutate(graph_bins = factor(graph_bins, levels = c("Yes", "Conditional Yes", "No Response",
                                                    "Conditional No", "No")),
         guest = ifelse(guest_black == 1, "Guest is African-American", "Guest is White")) %>%
  count(graph_bins, guest)

print(fig2_data)

fig2 <- ggplot(fig2_data, aes(x = graph_bins, y = n, fill = guest)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(breaks = seq(0, 1200, 300), limits = c(0, 1200)) +
  scale_fill_manual(values = c("Guest is African-American" = "#1b4f72",
                               "Guest is White"            = "#aab7b8")) +
  labs(x = NULL, y = "Number of responses", fill = NULL,
       title = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
print(fig2)
ggsave(file.path(output_dir, "figure2_response_by_race.png"), fig2, width = 8, height = 5, dpi = 300)

# --- Table 1: Summary statistics (l. 335-359) ---------------------------------
table1_vars <- c("host_race_white", "host_race_black", "host_gender_F", "host_gender_M",
                 "price", "bedrooms", "bathrooms", "number_of_reviews",
                 "multiple_listings", "any_black", "tract_listings", "black_proportion")

table1 <- do.call(rbind, lapply(table1_vars, function(v) {
  x  <- df[[v]]
  tt <- t.test(x ~ df$guest_black, var.equal = TRUE)   # like Stata's ttest
  data.frame(variable  = v,
             N         = sum(!is.na(x)),
             mean      = mean(x, na.rm = TRUE),
             sd        = sd(x, na.rm = TRUE),
             p25       = stata_pctile(x, 0.25),
             p75       = stata_pctile(x, 0.75),
             mean_white_guest = unname(tt$estimate[1]),
             mean_black_guest = unname(tt$estimate[2]),
             p_value   = tt$p.value)
}))
rownames(table1) <- NULL

# Labels and column names as in the paper
table1_labels <- c(
  host_race_white   = "Host is white",
  host_race_black   = "Host is African American",
  host_gender_F     = "Host is female",
  host_gender_M     = "Host is male",
  price             = "Price ($)",
  bedrooms          = "Number of bedrooms",
  bathrooms         = "Number of bathrooms",
  number_of_reviews = "Number of reviews",
  multiple_listings = "Host has multiple listings",
  any_black         = "Host has 1+ reviews from African American guests",
  tract_listings    = "Airbnb listings per census tract",
  black_proportion  = "Percent population African American (census tract)")

table1_pretty <- data.frame(
  "Variables"                      = table1_labels[table1$variable],
  "Mean"                           = round(table1$mean, 2),
  "SD"                             = round(table1$sd, 2),
  "25th percentile"                = round(table1$p25, 2),
  "75th percentile"                = round(table1$p75, 2),
  "Observations"                   = table1$N,
  "Mean, white accounts"           = round(table1$mean_white_guest, 2),
  "Mean, African American accounts" = round(table1$mean_black_guest, 2),
  "p-value"                        = round(table1$p_value, 2),
  check.names = FALSE, row.names = NULL)

print(table1_pretty, right = FALSE)
write.csv(table1_pretty, file.path(output_dir, "table1_summary_stats.csv"), row.names = FALSE)

# Check: the paper's overall mean of "1+ reviews from African American guests" (0.29)
# is reproduced when Tampa and Atlanta are NOT dropped (our final sample gives 0.28).
check_any_black <- raw %>%
  left_join(read_dta(file.path(data_dir, "hosts.dta")) %>% select(host_id, any_black),
            by = "host_id")
cat("Mean any_black incl. Tampa & Atlanta:",
    round(mean(check_any_black$any_black, na.rm = TRUE), 3), "\n")

# --- Table 2: The impact of race on the likelihood of acceptance (l. 364-372)
# Linear probability model (OLS), standard errors clustered by name x city.
# vcovCL(type = "HC1") uses the same small-sample correction as Stata.

m1 <- lm(yes ~ guest_black, data = df)
m2 <- lm(yes ~ guest_black + host_race_black + host_gender_M, data = df)
m3 <- lm(yes ~ guest_black + host_race_black + host_gender_M +
           multiple_listings + shared_property + ten_reviews + log_price, data = df)

cl <- function(m) vcovCL(m, cluster = ~ name_by_city, type = "HC1")

for (m in list(m1, m2, m3)) print(coeftest(m, vcov. = cl(m)))

# Nice side-by-side table like in the paper
reg_table <- function(models, vcov_fun, digits = 2) {
  vars <- unique(unlist(lapply(models, function(m) names(coef(m)))))
  vars <- c(setdiff(vars, "(Intercept)"), "(Intercept)")
  out <- sapply(models, function(m) {
    b  <- coef(m); se <- sqrt(diag(vcov_fun(m)))
    cell <- ifelse(vars %in% names(b),
                   paste0(formatC(b[vars], digits, format = "f"), " (",
                          formatC(se[vars], digits, format = "f"), ")"), "")
    c(cell, nobs(m), formatC(summary(m)$adj.r.squared, 3, format = "f"))
  })
  rownames(out) <- c(vars, "Observations", "Adjusted R2")
  colnames(out) <- paste0("(", seq_along(models), ")")
  noquote(out)
}

table2 <- reg_table(list(m1, m2, m3), cl)
print(table2)

# FINDING: With the do-file's definition (number_of_listings > 1), column (3) gives
# 0.06 (0.01) for "multiple listings" and adj. R2 = 0.039. The published table reports
# 0.09 (0.02) and 0.040. Using new_number_of_listings instead reproduces the paper exactly.
# (Table 1 in the paper is also inconsistent here: overall mean 0.16 = new variable,
#  means by guest race 0.32 / 0.33 = old variable.) The main coefficient is -0.09 either way.
m3_new <- lm(yes ~ guest_black + host_race_black + host_gender_M +
               multiple_listings_new + shared_property + ten_reviews + log_price, data = df)
table2_paper <- reg_table(list(m1, m2, m3_new), cl)
print(table2_paper)
cat("Mean multiple_listings (do-file):", round(mean(df$multiple_listings), 2),
    "| mean multiple_listings_new:", round(mean(df$multiple_listings_new), 2), "\n")
# Main table for the report = published version (col. 3 with new_number_of_listings).
# The do-file version is saved separately to document the difference.
write.csv(table2_paper, file.path(output_dir, "table2_main_regression.csv"))
write.csv(table2,       file.path(output_dir, "table2_main_regression_dofile_version.csv"))

# Interpretation helper: acceptance rates and the relative gap
rates <- df %>% filter(!is.na(yes)) %>% group_by(guest_black) %>% summarise(yes_rate = mean(yes))
print(rates)
cat("Relative gap: ", round(100 * coef(m1)["guest_black"] / rates$yes_rate[rates$guest_black == 0], 1),
    "% lower acceptance for African-American names\n")


###############################################################################
# PART 3: EXTENSION AND ROBUSTNESS (report Section 5.1)
###############################################################################

# =============================================================================
# 3.1 Logit and probit instead of the linear probability model
# =============================================================================

# Same two specifications as report Table 1, columns (1) and (3)
# (column 3 uses multiple_listings_new, i.e. the published version of the paper)
f1 <- yes ~ guest_black
f3 <- yes ~ guest_black + host_race_black + host_gender_M +
  multiple_listings_new + shared_property + ten_reviews + log_price

# Estimate the three models
lpm1   <- lm(f1, data = df)
lpm3   <- lm(f3, data = df)
logit1 <- glm(f1, family = binomial(link = "logit"),  data = df)
logit3 <- glm(f3, family = binomial(link = "logit"),  data = df)
probit3 <- glm(f3, family = binomial(link = "probit"), data = df)

# Raw logit coefficients (log-odds), SEs clustered as in the paper
coeftest(logit3, vcov. = vcovCL(logit3, cluster = ~ name_by_city, type = "HC1"))

# Average marginal effects (in probability points) -> comparable to the LPM
ame_logit1  <- avg_slopes(logit1,  variables = "guest_black", vcov = ~ name_by_city)
ame_logit3  <- avg_slopes(logit3,  variables = "guest_black", vcov = ~ name_by_city)
ame_probit3 <- avg_slopes(probit3, variables = "guest_black", vcov = ~ name_by_city)

# Comparison table
se_cl <- function(m) sqrt(vcovCL(m, cluster = ~ name_by_city, type = "HC1")["guest_black", "guest_black"])
ext1_table <- data.frame(
  Model       = c("LPM (paper)", "LPM (paper)", "Logit (AME)", "Logit (AME)", "Probit (AME)"),
  Controls    = c("No", "Yes", "No", "Yes", "Yes"),
  Effect      = round(c(coef(lpm1)["guest_black"], coef(lpm3)["guest_black"],
                        ame_logit1$estimate, ame_logit3$estimate, ame_probit3$estimate), 3),
  SE          = round(c(se_cl(lpm1), se_cl(lpm3),
                        ame_logit1$std.error, ame_logit3$std.error, ame_probit3$std.error), 3),
  N           = c(nobs(lpm1), nobs(lpm3), nobs(logit1), nobs(logit3), nobs(probit3)))
print(ext1_table)
write.csv(ext1_table, file.path(output_dir, "table_ext1_logit.csv"), row.names = FALSE)

# Odds ratio (optional, for interpretation)
exp(coef(logit3)["guest_black"])



# =============================================================================
# 3.2 Which types of responses change? (report Table 2)
# =============================================================================

# Sample: 20 names of the paper, valid responses only (codes 12-14 = errors/events)
d_resp <- df %>%
  filter(!guest_first_name %in% c("Jay", "Kareem", "Leroy"),
         !is.na(host_response), !host_response %in% 12:14)

# Groups of host responses (codes from the do-file, lines 171-187)
response_groups <- list(
  "Clear Yes"                         = 1,
  "Conditional Yes"                   = c(4, 5, 6, 7, 8),
  "  of which: Yes if you verify"     = 4,
  "Request for more information"      = 2,
  "Check back later / unsure"         = 9:11,
  "No response"                       = -1,
  "No unless you verify"              = 3,
  "Clear No"                          = 0)

# One linear probability model per response type
ext2_table <- do.call(rbind, lapply(names(response_groups), function(g) {
  d_resp$y <- as.numeric(d_resp$host_response %in% response_groups[[g]])
  m  <- lm(y ~ guest_black, data = d_resp)
  ct <- coeftest(m, vcov. = cl(m))["guest_black", ]
  data.frame(Response = g,
             Share_white_guests = round(mean(d_resp$y[d_resp$guest_black == 0]), 3),
             Effect_AfAm_guest  = round(ct[1], 3),
             SE = round(ct[2], 3), p_value = round(ct[4], 3))
}))
print(ext2_table, row.names = FALSE)

# Ordered logit: No < Conditional No < No Response < Conditional Yes < Yes
d_resp$response_ordered <- factor(d_resp$graph_bins,
                                  levels = c("No", "Conditional No", "No Response", "Conditional Yes", "Yes"), ordered = TRUE)
ologit <- MASS::polr(response_ordered ~ guest_black, data = d_resp, Hess = TRUE)
# Add the ordered logit as the last row of the table
ct_ologit <- coeftest(ologit, vcov. = vcovCL(ologit, cluster = ~ name_by_city, type = "HC1"))["guest_black", ]

ext2_table <- rbind(ext2_table,
                    data.frame(Response = "Ordered logit (all categories)",
                               Share_white_guests = NA,
                               Effect_AfAm_guest  = round(ct_ologit[1], 3),
                               SE = round(ct_ologit[2], 3), p_value = round(ct_ologit[4], 3)))

# Report version: nice column names, 3 decimals, p < 0.001
fmt <- function(x) ifelse(is.na(x), "", formatC(x, digits = 3, format = "f"))
ext2_report <- data.frame(
  "Response"                        = ext2_table$Response,
  "Share (white names)"             = fmt(ext2_table$Share_white_guests),
  "Effect of African American name" = fmt(ext2_table$Effect_AfAm_guest),
  "SE"                              = fmt(ext2_table$SE),
  "p-value"                         = ifelse(ext2_table$p_value < 0.001, "< 0.001", fmt(ext2_table$p_value)),
  check.names = FALSE)
print(ext2_report, row.names = FALSE)
write.csv(ext2_report, file.path(output_dir, "table_ext2_response_types.csv"), row.names = FALSE)

# =============================================================================
# 3.3 Differential attrition and Manski worst-case bounds (report Table 3)
# =============================================================================

# Step 1: Who has a missing outcome? ------------------------------------------
# yes is missing for code 5 (different place), 13 (date error), 14 (message not sent)
d_att <- df %>% filter(!is.na(host_response)) %>%       # all 6,390 real inquiries
  mutate(missing_outcome = as.numeric(is.na(yes)),
         not_sent        = as.numeric(host_response == 14))

attrition_table <- d_att %>% group_by(guest_black) %>%
  summarise(inquiries = n(), n_not_sent = sum(not_sent),
            n_missing = sum(missing_outcome),
            share_missing = round(mean(missing_outcome), 4))
print(attrition_table)

# Is the difference significant? (LPM, SEs clustered as in the paper)
m_att <- lm(missing_outcome ~ guest_black, data = d_att)
print(coeftest(m_att, vcov. = cl(m_att))["guest_black", ])

# Where does it happen? (messages not sent, by city and guest race)
print(table(city = d_att$city[d_att$not_sent == 1],
            guest_black = d_att$guest_black[d_att$not_sent == 1]))

# Step 2: Manski worst-case bounds -------------------------------------------
# Fill in every missing outcome in the way that is worst / best for the result:
# lower bound: missing African American = 0 (no), missing white = 1 (yes)
# upper bound: missing African American = 1 (yes), missing white = 0 (no)
manski_bounds <- function(d) {
  b <- d$guest_black == 1; y <- d$yes
  c(lower = mean(ifelse(is.na(y[b]), 0, y[b])) - mean(ifelse(is.na(y[!b]), 1, y[!b])),
    upper = mean(ifelse(is.na(y[b]), 1, y[b])) - mean(ifelse(is.na(y[!b]), 0, y[!b])))
}
manski <- manski_bounds(d_att)

# Step 3: Confidence interval: cluster bootstrap over name x city (999 draws) --
set.seed(1)
clusters    <- split(d_att[, c("yes", "guest_black")], d_att$name_by_city)
manski_boot <- t(replicate(999, manski_bounds(do.call(rbind, sample(clusters, replace = TRUE)))))

# Step 4: Summary table ------------------------------------------------------
baseline <- coef(lm(yes ~ guest_black, data = d_att))["guest_black"]
ext3_table <- data.frame(
  Approach = c("Baseline (missing outcomes dropped)", "Manski worst-case bounds",
               "Manski bounds, 95% CI"),
  Lower = round(c(baseline, manski["lower"], quantile(manski_boot[, "lower"], 0.025)), 3),
  Upper = round(c(baseline, manski["upper"], quantile(manski_boot[, "upper"], 0.975)), 3))
print(ext3_table, row.names = FALSE)
write.csv(ext3_table, file.path(output_dir, "table_ext3_manski_bounds.csv"), row.names = FALSE)
