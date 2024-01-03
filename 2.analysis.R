library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(car)
library(onewaytests)

natlog <- function(x) {
  tmp <- log(as.numeric(x) + .1)
  return(as.numeric(tmp))
}

# import data; all loan applications --------------------------------------

df <- fread("~/Dropbox/SPSP 2024/Adriana/1.processed data/1.loan2018-2020processed.csv")
df$race_composition <- as.factor(df$race_composition)
df$race_composition <- relevel(df$race_composition, ref="white_white")

# MODEL 1: all loans; approved and rejected; natlog(interest_rate)
m1 <- lm(natlog(interest_rate) ~ race_composition + applicant_sex + 
               applicant_age + co_applicant_age + loan_type + loan_purpose + 
               state_code + natlog(income) + hoepa_status + lien_status + 
               natlog(minority_population) + natlog(hud_median_family_income) + 
               debt_to_income_ratio + open_end_line_of_credit + 
               applicant_race_observed + co_applicant_race_observed + 
               applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
               applicant_sex_observed + co_applicant_sex_observed + 
               purchaser_type + interest_only_payment + 
               natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
               natlog(loan_amount_000s), data=df)

summary(m1)

# chi-sq hypothesis examining differences between couple type

amindian <- linearHypothesis(m1, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m1, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m1, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m1, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m1, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l1 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data1 <- data.frame(term = names(m1$coefficients)[2:11],
                    coef = unname(m1$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m1))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

data1$odds <- exp(as.numeric(data1$coef))

# remove prefix from term
data1$term <- gsub(".*race_composition", "", data1$term)
data1$term <- str_replace(data1$term, "_", "-")

# set upper and lower bounds
data1$lb <- data1$coef - data1$se
data1$ub <- data1$coef + data1$se

rm(m1)

ggplot(data1, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="log(Interest Rate) by Race Composition",
       subtitle="All Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

# MODEL 2: all loans; approved only; natlog(interest_rate)
m2 <- lm(natlog(interest_rate) ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df[df$loan_originated==1])

summary(m2)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m2, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m2, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m2, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m2, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m2, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l2 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data2 <- data.frame(term = names(m2$coefficients)[2:11],
                    coef = unname(m2$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m2))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data2$term <- gsub(".*race_composition", "", data2$term)
data2$term <- str_replace(data2$term, "_", "-")

# set upper and lower bounds
data2$lb <- data2$coef - data2$se
data2$ub <- data2$coef + data2$se

rm(m2)

# coefficient plot using approved loans only
ggplot(data2, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="log(Interest Rate) by Race Composition",
       subtitle="Approved Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

# MODEL 3: all loans; approved and rejected; raw interest_rate
m3 <- lm(interest_rate ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df)

summary(m3)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m3, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m3, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m3, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m3, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m3, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l3 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data3 <- data.frame(term = names(m3$coefficients)[2:11],
                    coef = unname(m3$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m3))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data3$term <- gsub(".*race_composition", "", data3$term)
data3$term <- str_replace(data3$term, "_", "-")

# set upper and lower bounds
data3$lb <- data3$coef - data3$se
data3$ub <- data3$coef + data3$se

rm(m3)

# coefficient plot using all loan applications
ggplot(data3, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="Interest Rate by Race Composition",
       subtitle="All Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

# MODEL 4: all loans; approved only; interest_rate
m4 <- lm(interest_rate ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df[df$loan_originated==1])

summary(m4)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m4, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m4, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m4, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m4, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m4, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l4 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data4 <- data.frame(term = names(m4$coefficients)[2:11],
                    coef = unname(m4$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m4))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data4$term <- gsub(".*race_composition", "", data4$term)
data4$term <- str_replace(data4$term, "_", "-")

# set upper and lower bounds
data4$lb <- data4$coef - data4$se
data4$ub <- data4$coef + data4$se

rm(m4)

# coefficient plot using approved loans only
ggplot(data4, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="Interest Rate by Race Composition",
       subtitle="Approved Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

# mapping -----------------------------------------------------------------

library(usmap)

# import data, filter so approved loans remain
df <- fread("~/Desktop/SPSP 2023/Adriana/1.processed data/1.loan2018-2020processed.csv")
df <- df %>% filter(loan_originated == 1)

# create dummy code for applications > median(intrate)
df$intrate_hi <- ifelse(df$interest_rate > median(df$interest_rate, na.rm=T), 1, 0)

# create dummy codes for couple race-composition
df$white <- ifelse(df$race_composition == "white_white", 1, 0)
df$asian <- ifelse(df$race_composition == "asian_asian" | 
                   df$race_composition == "asian_white", 1, 0)
df$other <- ifelse(df$race_composition != "white_white" &
                   df$race_composition != "asian_asian" &
                   df$race_composition != "asian_white", 1, 0)

# summarize descriptive statistics across state code
sum_accpt <- df %>% group_by(state_code) %>%
  summarise(num_total = n(),
            num_poc = sum(other, na.rm=T),
            num_poc_hi = sum(other[intrate_hi == 1], na.rm=T),
            num_white = sum(white, na.rm=T),
            num_white_hi = sum(white[intrate_hi == 1], na.rm=T),
            num_asian = sum(asian, na.rm=T),
            num_asian_hi = sum(asian[intrate_hi == 1], na.rm=T),
            perc_poc = (sum(other, na.rm=T)/n()),
            perc_poc_hi = (sum(other[intrate_hi == 1], na.rm=T)/sum(other, na.rm=T)),
            perc_white = (sum(white, na.rm=T)/n()),
            perc_white_hi = (sum(white[intrate_hi == 1], na.rm=T)/sum(white, na.rm=T)),
            perc_asian = (sum(asian, na.rm=T)/n()),
            perc_asian_hi = (sum(asian[intrate_hi == 1], na.rm=T)/sum(asian, na.rm=T)),
  )

sd(sum_accpt$perc_poc_hi)

rm(df)

# two-sample t-test
t.test(sum_accpt$perc_poc_hi, sum_accpt$perc_white_hi)

# get fips data for % poc application > median(intrate)
accpt <- select(sum_accpt, c(state_code, perc_poc_hi))
accpt$fips <- fips(accpt$state_code)
accpt <- accpt[,c("fips", "perc_poc_hi")]
names(accpt) <- c("fips", "values")

# get mapping information; change decimal to percent
accpt <- map_with_data(accpt, values="values", na=NA) %>%
  filter(!is.na(fips))
accpt$values <- (accpt$values*100)

# map % poc application > median(intrate)
plot_usmap(regions="states", 
           data=accpt[,9:10]) +
  scale_fill_gradientn(colors=brewer.pal(7, "YlOrRd"), na.value="white") +
  theme(legend.background=element_blank(), legend.position="right",
        legend.key.size=unit(.5, "cm")) +
  labs(fill="Percent POC Applications (%)", title="Percent POC Applications w/ High Interest Rates",
       subtitle="POC excludes Asian/Asian American and White/European American Applicants")

# identify missing U.S. territories
missing <- sum_accpt %>% filter(!state_code %in% accpt$abbr)

# supplementary analyses --------------------------------------------------

df <- fread("~/Desktop/SPSP 2023/1.processed data/1.loan2018-2020latinx.csv")

df$race_composition <- as.factor(df$race_composition)
df$race_composition <- relevel(df$race_composition, ref="white_white")

# MODEL 5: white-latinx excluded; approved and rejected; natlog(interest_rate)
m5 <- lm(natlog(interest_rate) ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df)

summary(m5)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m5, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m5, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m5, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m5, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m5, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l5 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data5 <- data.frame(term = names(m5$coefficients)[2:11],
                    coef = unname(m5$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m5))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data5$term <- gsub(".*race_composition", "", data5$term)
data5$term <- str_replace(data5$term, "_", "-")

# set upper and lower bounds
data5$lb <- data5$coef - data5$se
data5$ub <- data5$coef + data5$se

# coefficient plot using all loan applications
ggplot(data5, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="log(Interest Rate) by Race Composition (White-Latinx Excluded)",
       subtitle="All Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

rm(m5)

# MODEL 6: white-latinx excluded; approved only; natlog(interest_rate)
m6 <- lm(natlog(interest_rate) ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df[df$loan_originated == 1])

summary(m6)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m6, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m6, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m6, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m6, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m6, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l6 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data6 <- data.frame(term = names(m6$coefficients)[2:11],
                    coef = unname(m6$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m6))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data6$term <- gsub(".*race_composition", "", data6$term)
data6$term <- str_replace(data6$term, "_", "-")

# set upper and lower bounds
data6$lb <- data6$coef - data6$se
data6$ub <- data6$coef + data6$se

# coefficient plot using approved loans only
ggplot(data6, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="log(Interest Rate) by Race Composition (White-Latinx Excluded)",
       subtitle="Approved Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

rm(m6)

# MODEL 7: white-latinx excluded; approved and rejected; interest_rate
m7 <- lm(interest_rate ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df)

summary(m7)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m7, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m7, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m7, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m7, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m7, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l7 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data7 <- data.frame(term = names(m7$coefficients)[2:11],
                    coef = unname(m7$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m7))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data7$term <- gsub(".*race_composition", "", data7$term)
data7$term <- str_replace(data7$term, "_", "-")

# set upper and lower bounds
data7$lb <- data7$coef - data7$se
data7$ub <- data7$coef + data7$se

# coefficient plot using approved loans only
ggplot(data7, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="Interest Rate by Race Composition (White-Latinx Excluded)",
       subtitle="Approved Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

rm(m7)

# MODEL 8: white-latinx excluded; approved only; interest_rate
m8 <- lm(interest_rate ~ race_composition + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + natlog(income) + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) +
           natlog(loan_amount_000s), data=df[df$loan_originated==1])

summary(m8)

# chi-sq hypothesis examining differences between couple type
amindian <- linearHypothesis(m8, "race_compositionamindian_amindian - race_compositionamindian_white = 0")
asian <- linearHypothesis(m8, "race_compositionasian_asian - race_compositionasian_white = 0")
black <- linearHypothesis(m8, "race_compositionblack_black - race_compositionblack_white = 0")
latinx <- linearHypothesis(m8, "race_compositionlatinx_latinx - race_compositionlatinx_white = 0")
pacif <- linearHypothesis(m8, "race_compositionpacif_pacif - race_compositionpacif_white = 0")
l8 <- list(amindian, asian, black, latinx, pacif)

rm(amindian, asian, black, latinx, pacif)

## plotting race-composition coefficients for model ##

# create data frame containing model information
data8 <- data.frame(term = names(m8$coefficients)[2:11],
                    coef = unname(m8$coefficients)[2:11],
                    se = unname(sqrt(diag(vcov(m8))))[2:11],
                    type = rep(c("Same-Race","Interracial"), 5))

# remove prefix from term
data8$term <- gsub(".*race_composition", "", data8$term)
data8$term <- str_replace(data8$term, "_", "-")

# set upper and lower bounds
data8$lb <- data8$coef - data8$se
data8$ub <- data8$coef + data8$se

# coefficient plot using approved loans only
ggplot(data8, aes(term, coef, color=type)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1) +
  scale_color_manual(values=c("#D81B60", "#1E88E5")) +
  theme(axis.text.x = element_text(angle = 285)) +
  labs(title="Interest Rate by Race Composition (White-Latinx Excluded)",
       subtitle="Approved Mortgage Applications",
       x="Race Composition", y="Coefficient",
       color="Couple Composition")

rm(m8)

# moderation effect -------------------------------------------------------

df <- fread("~/Desktop/SPSP 2023/Adriana/1.processed data/1.loan2018-2020processed.csv")

df$race_composition <- as.factor(df$race_composition)
df$race_composition <- relevel(df$race_composition, ref="white_white")

m9 <- lm(natlog(interest_rate) ~ race_composition*natlog(income) + applicant_sex + 
           applicant_age + co_applicant_age + loan_type + loan_purpose + 
           state_code + hoepa_status + lien_status + 
           natlog(minority_population) + natlog(hud_median_family_income) + 
           debt_to_income_ratio + open_end_line_of_credit + 
           applicant_race_observed + co_applicant_race_observed + 
           applicant_ethnicity_observed + co_applicant_ethnicity_observed + 
           applicant_sex_observed + co_applicant_sex_observed + 
           purchaser_type + interest_only_payment + 
           natlog(combined_loan_to_value_ratio) + natlog(loan_term) + natlog(property_value) + 
           natlog(loan_amount_000s), data=df)

summary(m9)

