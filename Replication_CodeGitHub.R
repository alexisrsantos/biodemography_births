##########################  TITLE    ###############################
#   Ongoing Economic Crisis found to accelerate fertility decline ##
#   in Puerto Rico  (Replication Code)                            ##
####################################################################

#Open library to read excel files 
library(readxl)

#Use read excel function to read the data into R Studio
Data_fecun <- read_excel("/Data_fecun.xlsx")

Data_fecun$Crisis<-Data_fecun$Crisis2

#############################################
# Overall Interactions presented in Table 2 #
#############################################

#Model 1 in Table 2 - CBR
modelo1<-lm(log_BR~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo1)
confint(modelo1,level=0.95)

#Model 2 in Table 2 - ASFR 15-19
modelo2<-lm(log_asfr15_19~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo2)
confint(modelo2,level=0.95)

#Model 3 in Table 2 - ASFR 20-24
modelo3<-lm(log_asfr20_24~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo3)
confint(modelo3,level=0.95)

#Model 4 in Table 2  - ASFR 25-29
modelo4<-lm(log_asfr25_29~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo4)
confint(modelo4,level=0.95)

#Model 5 in Table 2 - ASFR 30-34
modelo5<-lm(log_asfr30_34~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo5)
confint(modelo5,level=0.95)

#Model 6 in Table 2 - ASFR 35-39
modelo6<-lm(log_asfr35_39~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo6)
confint(modelo6,level=0.95)

#Model 7 in Table 2 - ASFR 40 - 44
modelo7<-lm(log_asfr40_44~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo7)
confint(modelo7,level=0.95) 

#Model 8 in Table 2 - TFR
modelo08<-lm(log_TFR~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo08)
confint(modelo08,level=0.95)

#Model 9 in Table 2 - % Women 15-44 
modelo9<-lm(porctotal15_44~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo9)
confint(modelo9,level=0.95)

###################################################
# Effects for Pre-crisis in Table 2 (1990-2004)   #
###################################################

data_fecun2<-subset(Data_fecun,Crisis==0)

#Model 1 for Pre-Crisis in Table 2
modelo20<-lm(log_BR~ID,data=data_fecun2)
summary(modelo20)
confint(modelo20,'ID',level=0.95)

#Model 2 for Pre-Crisis in Table 2
modelo23<-lm(log_asfr15_19~ID,data=data_fecun2)
summary(modelo23)
confint(modelo23,'ID',level=0.95)

#Model 3 for Pre-Crisis in Table 2
modelo24<-lm(log_asfr20_24~ID,data=data_fecun2)
summary(modelo24)
confint(modelo24,'ID',level=0.95)

#Model 4 for Pre-Crisis in Table 2
modelo25<-lm(log_asfr25_29~ID,data=data_fecun2)
summary(modelo25)
confint(modelo25,'ID',level=0.95)

#Model 5 for Pre-Crisis in Table 2
modelo26<-lm(log_asfr30_34~ID,data=data_fecun2)
summary(modelo26)
confint(modelo26,'ID',level=0.95)

#Model 6 for Pre-Crisis in Table 2
modelo27<-lm(log_asfr35_39~ID,data=data_fecun2)
summary(modelo27)
confint(modelo27,'ID',level=0.95)

#Model 7 for Pre-Crisis in Table 2
modelo28<-lm(log_asfr40_44~ID,data=data_fecun2)
summary(modelo28)
confint(modelo28,'ID',level=0.95)

#Model 8 for Pre-Crisis in Table 2
modelo21<-lm(log_TFR~ID,data=data_fecun2)
summary(modelo21)
confint(modelo21,'ID',level=0.95)

#Model 9 for Pre-Crisis in Table 2
modelo22<-lm(porctotal15_44~ID,data=data_fecun2)
summary(modelo22)
confint(modelo22,'ID',level=0.95)

###################################################
# Effects for Crisis period in Table 2 (2005-2020 #
###################################################

#Subset data for the Crisis Period
data_fecun3<-subset(Data_fecun,Crisis==1)

#Model 1 in Crisis Period in Table 2
modelo29<-lm(log_BR~ID,data=data_fecun3)
summary(modelo29)
confint(modelo29,'ID',level=0.95)

#Model 2 in Crisis Period in Table 2
modelo32<-lm(log_asfr15_19~ID,data=data_fecun3)
summary(modelo32)
confint(modelo32,'ID',level=0.95)

#Model 3 in Crisis Period in Table 2
modelo33<-lm(log_asfr20_24~ID,data=data_fecun3)
summary(modelo33)
confint(modelo33,'ID',level=0.95)

#Model 4 in Crisis Period in Table 2
modelo34<-lm(log_asfr25_29~ID,data=data_fecun3)
summary(modelo34)
confint(modelo34,'ID',level=0.95)

#Model 5 in Crisis Period in Table 2
modelo35<-lm(log_asfr30_34~ID,data=data_fecun3)
summary(modelo35)
confint(modelo35,'ID',level=0.95)

#Model 6 in Crisis Period in Table 2
modelo36<-lm(log_asfr35_39~ID,data=data_fecun3)
summary(modelo36)
confint(modelo36,'ID',level=0.95)

#Model 7 in Crisis Period in Table 2
modelo37<-lm(log_asfr40_44~ID,data=data_fecun3)
summary(modelo37)
confint(modelo37,'ID',level=0.95)

#Model 8 in Crisis Period in Table 2
modelo30<-lm(log_TFR~ID,data=data_fecun3)
summary(modelo30)
confint(modelo30,'ID',level=0.95)

#Model 9 in Crisis Period in Table 2
modelo31<-lm(porctotal15_44~ID,data=data_fecun3)
summary(modelo31)
confint(modelo31,'ID',level=0.95)

##############################
##          THE END         ##
##############################


#############################################
# Overall Interactions presented in Table 2 #
#############################################

#Model 1 in Table 2 - CBR
modelo1<-lm(log_BR~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo1)
confint(modelo1,level=0.95)

#Model 2 in Table 2 - ASFR 15-19
modelo2<-lm(log_asfr15_19~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo2)
confint(modelo2,level=0.95)

#Model 3 in Table 2 - ASFR 20-24
modelo3<-lm(log_asfr20_24~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo3)
confint(modelo3,level=0.95)

#Model 4 in Table 2  - ASFR 25-29
modelo4<-lm(log_asfr25_29~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo4)
confint(modelo4,level=0.95)

#Model 5 in Table 2 - ASFR 30-34
modelo5<-lm(log_asfr30_34~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo5)
confint(modelo5,level=0.95)

#Model 6 in Table 2 - ASFR 35-39
modelo6<-lm(log_asfr35_39~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo6)
confint(modelo6,level=0.95)

#Model 7 in Table 2 - ASFR 40 - 44
modelo7<-lm(log_asfr40_44~ID+Crisis+ID*Crisis,data = Data_fecun)
summary(modelo7)
confint(modelo7,level=0.95) 

#Model 8 in Table 2 - TFR
modelo8<-lm(log_TFR~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo8)
confint(modelo8,level=0.95)

#Model 9 in Table 2 - % Women 15-44 
modelo9<-lm(porctotal15_44~ID+Crisis+ID*Crisis,data=Data_fecun)
summary(modelo9)
confint(modelo9,level=0.95)

###################################################
# Effects for Pre-crisis in Table 2 (1990-2004)   #
###################################################

data_fecun2<-subset(Data_fecun,Crisis==0)

#Model 1 for Pre-Crisis in Table 2
modelo20<-lm(log_BR~ID,data=data_fecun2)
summary(modelo20)
confint(modelo20,'ID',level=0.95)

#Model 2 for Pre-Crisis in Table 2
modelo23<-lm(log_asfr15_19~ID,data=data_fecun2)
summary(modelo23)
confint(modelo23,'ID',level=0.95)

#Model 3 for Pre-Crisis in Table 2
modelo24<-lm(log_asfr20_24~ID,data=data_fecun2)
summary(modelo24)
confint(modelo24,'ID',level=0.95)

#Model 4 for Pre-Crisis in Table 2
modelo25<-lm(log_asfr25_29~ID,data=data_fecun2)
summary(modelo25)
confint(modelo25,'ID',level=0.95)

#Model 5 for Pre-Crisis in Table 2
modelo26<-lm(log_asfr30_34~ID,data=data_fecun2)
summary(modelo26)
confint(modelo26,'ID',level=0.95)

#Model 6 for Pre-Crisis in Table 2
modelo27<-lm(log_asfr35_39~ID,data=data_fecun2)
summary(modelo27)
confint(modelo27,'ID',level=0.95)

#Model 7 for Pre-Crisis in Table 2
modelo28<-lm(log_asfr40_44~ID,data=data_fecun2)
summary(modelo28)
confint(modelo28,'ID',level=0.95)

#Model 8 for Pre-Crisis in Table 2
modelo21<-lm(log_TFR~ID,data=data_fecun2)
summary(modelo21)
confint(modelo21,'ID',level=0.95)

#Model 9 for Pre-Crisis in Table 2
modelo22<-lm(porctotal15_44~ID,data=data_fecun2)
summary(modelo22)
confint(modelo22,'ID',level=0.95)

###################################################
# Effects for Crisis period in Table 2 (2005-2020 #
###################################################

#Subset data for the Crisis Period
data_fecun3<-subset(Data_fecun,Crisis==1)

#Model 1 in Crisis Period in Table 2
modelo29<-lm(log_BR~ID,data=data_fecun3)
summary(modelo29)
confint(modelo29,'ID',level=0.95)

#Model 2 in Crisis Period in Table 2
modelo32<-lm(log_asfr15_19~ID,data=data_fecun3)
summary(modelo32)
confint(modelo32,'ID',level=0.95)

#Model 3 in Crisis Period in Table 2
modelo33<-lm(log_asfr20_24~ID,data=data_fecun3)
summary(modelo33)
confint(modelo33,'ID',level=0.95)

#Model 4 in Crisis Period in Table 2
modelo34<-lm(log_asfr25_29~ID,data=data_fecun3)
summary(modelo34)
confint(modelo34,'ID',level=0.95)

#Model 5 in Crisis Period in Table 2
modelo35<-lm(log_asfr30_34~ID,data=data_fecun3)
summary(modelo35)
confint(modelo35,'ID',level=0.95)

#Model 6 in Crisis Period in Table 2
modelo36<-lm(log_asfr35_39~ID,data=data_fecun3)
summary(modelo36)
confint(modelo36,'ID',level=0.95)

#Model 7 in Crisis Period in Table 2
modelo37<-lm(log_asfr40_44~ID,data=data_fecun3)
summary(modelo37)
confint(modelo37,'ID',level=0.95)

#Model 8 in Crisis Period in Table 2
modelo30<-lm(log_TFR~ID,data=data_fecun3)
summary(modelo30)
confint(modelo30,'ID',level=0.95)

#Model 9 in Crisis Period in Table 2
modelo31<-lm(porctotal15_44~ID,data=data_fecun3)
summary(modelo31)
confint(modelo31,'ID',level=0.95)

###Now I make the tables
library(stargazer)

## -----------------------------------------------------------
## Table 2: Interaction, pre-crisis, crisis, and "S" flag
## -----------------------------------------------------------

# Make sure stargazer is loaded
# install.packages("stargazer")
library(stargazer)

# --- 1. List your models in the correct order ----------------

overall_models <- list(
  modelo1,  # CBR
  modelo2,  # ASFR 15-19
  modelo3,  # ASFR 20-24
  modelo4,  # ASFR 25-29
  modelo5,  # ASFR 30-34
  modelo6,  # ASFR 35-39
  modelo7,  # ASFR 40-44
  modelo8,  # TFR   (note: modelo8 must be defined, not modelo18)
  modelo9   # % women 15-44
)

pre_models <- list(
  modelo20, # CBR
  modelo23, # 15-19
  modelo24, # 20-24
  modelo25, # 25-29
  modelo26, # 30-34
  modelo27, # 35-39
  modelo28, # 40-44
  modelo21, # TFR
  modelo22  # % women 15-44
)

crisis_models <- list(
  modelo29, # CBR
  modelo32, # 15-19
  modelo33, # 20-24
  modelo34, # 25-29
  modelo35, # 30-34
  modelo36, # 35-39
  modelo37, # 40-44
  modelo30, # TFR
  modelo31  # % women 15-44
)

# --- 2. Helper to extract coef, CI, and star -----------------

get_stats <- function(mod, term) {
  sm <- summary(mod)$coefficients
  beta <- sm[term, "Estimate"]
  pval <- sm[term, "Pr(>|t|)"]
  ci   <- confint(mod, term, level = 0.95)
  star <- ifelse(pval < 0.05, "*", "")
  list(beta = beta,
       lower = ci[1],
       upper = ci[2],
       star  = star)
}

fmt <- function(x) sprintf("%.3f", x)

# interaction term name in the overall models:
interaction_term <- "ID:Crisis"  # change to "Crisis:ID" if needed

# --- 3. Extract stats ----------------------------------------

overall_stats <- lapply(overall_models, get_stats, term = interaction_term)
pre_stats     <- lapply(pre_models,     get_stats, term = "ID")
crisis_stats  <- lapply(crisis_models,  get_stats, term = "ID")

# Turn lists into vectors
ov_beta  <- sapply(overall_stats, `[[`, "beta")
ov_low   <- sapply(overall_stats, `[[`, "lower")
ov_up    <- sapply(overall_stats, `[[`, "upper")
ov_star  <- sapply(overall_stats, `[[`, "star")

pre_beta <- sapply(pre_stats, `[[`, "beta")
pre_low  <- sapply(pre_stats, `[[`, "lower")
pre_up   <- sapply(pre_stats, `[[`, "upper")
pre_star <- sapply(pre_stats, `[[`, "star")

cr_beta  <- sapply(crisis_stats, `[[`, "beta")
cr_low   <- sapply(crisis_stats, `[[`, "lower")
cr_up    <- sapply(crisis_stats, `[[`, "upper")
cr_star  <- sapply(crisis_stats, `[[`, "star")

# --- 4. "S" indicator for non-overlapping CIs ----------------

difference_flag <- ifelse(pre_up < cr_low | cr_up < pre_low, "S", "")

# --- 5. Build display strings (coef+*, CI) -------------------

overall_coef_str <- paste0(fmt(ov_beta), ov_star)
overall_ci_str   <- paste0(fmt(ov_low), ", ", fmt(ov_up))

pre_coef_str     <- paste0(fmt(pre_beta), pre_star)
pre_ci_str       <- paste0(fmt(pre_low), ", ", fmt(pre_up))

crisis_coef_str  <- paste0(fmt(cr_beta), cr_star)
crisis_ci_str    <- paste0(fmt(cr_low), ", ", fmt(cr_up))

# --- 6. Row labels -------------------------------------------

indicator_labels <- c(
  "Crude Birth Rate (CBR)",
  "ASFR 15-19",
  "ASFR 20-24",
  "ASFR 25-29",
  "ASFR 30-34",
  "ASFR 35-39",
  "ASFR 40-44",
  "Total Fertility Rate (TFR)",
  "% Population Women 15-44"
)

tab2 <- data.frame(
  Indicator      = indicator_labels,
  Interaction    = overall_coef_str,
  CI_overall     = overall_ci_str,
  AnnualChangePre  = pre_coef_str,
  CI_pre         = pre_ci_str,
  AnnualChangeCrisis = crisis_coef_str,
  CI_crisis      = crisis_ci_str,
  Difference     = difference_flag,
  stringsAsFactors = FALSE
)

# Rename columns to match headings in the LaTeX table
colnames(tab2) <- c(
  "Indicator",
  "Interaction",
  "95% C.I.",
  "Annual Change",
  "95% C.I.",
  "Annual Change",
  "95% C.I.",
  "Difference"
)

# --- 7. Print LaTeX table with grouped headers ---------------

stargazer(
  tab2,
  summary      = FALSE,
  rownames     = FALSE,
  type         = "html",   # use "text" to inspect in console first
  out          = "Table2.html",  # <-- writes to file
  title        = "Change in fertility indicators and demographic composition in Puerto Rico 1990-2020, as a function of time and the economic crisis.",
  digits       = 2,
  column.labels   = c("", "Overall", "Pre-Crisis (1990-2004)", "Crisis (2005-2020)", ""),
  column.separate = c(1, 2, 2, 2, 1),
  header       = FALSE
)


##############################
##          THE END         ##
##############################

# --- Extract R-squared for each set of models ---------------------

rsq_overall <- sapply(overall_models, function(m) summary(m)$r.squared)
rsq_pre     <- sapply(pre_models,     function(m) summary(m)$r.squared)
rsq_crisis  <- sapply(crisis_models,  function(m) summary(m)$r.squared)

# Format them
rsq_overall_str <- sprintf("%.3f", rsq_overall)
rsq_pre_str     <- sprintf("%.3f", rsq_pre)
rsq_crisis_str  <- sprintf("%.3f", rsq_crisis)

tab2 <- data.frame(
  Indicator      = indicator_labels,
  
  # Overall
  Interaction    = overall_coef_str,
  CI_overall     = overall_ci_str,
  R2_overall     = rsq_overall_str,
  
  # Pre-Crisis
  AnnualChangePre  = pre_coef_str,
  CI_pre           = pre_ci_str,
  R2_pre           = rsq_pre_str,
  
  # Crisis
  AnnualChangeCrisis = crisis_coef_str,
  CI_crisis          = crisis_ci_str,
  R2_crisis          = rsq_crisis_str,
  
  # Difference flag
  Difference     = difference_flag,
  stringsAsFactors = FALSE
)

colnames(tab2) <- c(
  "Indicator",
  "Interaction",
  "95% C.I.",
  "R²",
  
  "Annual Change",
  "95% C.I.",
  "R²",
  
  "Annual Change",
  "95% C.I.",
  "R²",
  
  "Difference"
)

stargazer(
  tab2,
  summary      = FALSE,
  rownames     = FALSE,
  type         = "html",
  out          = "Table5.html",
  title        = "Change in fertility indicators in Puerto Rico 1990-2020",
  digits       = 2,
  column.labels   = c("", "Overall", "", "", "Pre-Crisis (1990-2004)", "", "", "Crisis (2005-2020)", "", "", ""),
  column.separate = c(1, 3, 3, 3, 1),
  header       = FALSE
)

rstudioapi::viewer("Table5.html")


####################################################################
##  Newey-West robust inference + serial correlation diagnostics   ##
##  Standalone supplement. Run AFTER the main replication script.  ##
##  Requires in environment: overall_models, pre_models,           ##
##  crisis_models, indicator_labels, interaction_term, Data_fecun  ##
####################################################################

library(sandwich)
library(lmtest)
library(stargazer)

## ---------------------------------------------------------------
## 1. Lag selection and NW extraction
## ---------------------------------------------------------------

nw_lag <- function(mod) {
  n <- length(residuals(mod))
  max(1, floor(4 * (n / 100)^(2/9)))
}

nw_stats <- function(mod, term) {
  L  <- nw_lag(mod)
  V  <- NeweyWest(mod, lag = L, prewhite = FALSE, adjust = TRUE)
  ct <- coeftest(mod, vcov. = V)
  ci <- coefci(mod, parm = term, level = 0.95, vcov. = V)
  ols <- summary(mod)$coefficients
  list(beta     = unname(ct[term, "Estimate"]),
       se_nw    = unname(ct[term, "Std. Error"]),
       se_ols   = unname(ols[term, "Std. Error"]),
       p_nw     = unname(ct[term, "Pr(>|t|)"]),
       p_ols    = unname(ols[term, "Pr(>|t|)"]),
       lower    = ci[1],
       upper    = ci[2],
       star     = ifelse(ct[term, "Pr(>|t|)"] < 0.05, "*", ""),
       lag      = L,
       n        = length(residuals(mod)))
}

fmt <- function(x) sprintf("%.3f", x)

## ---------------------------------------------------------------
## 2. Apply to all three model sets
## ---------------------------------------------------------------

ov_nw <- lapply(overall_models, nw_stats, term = interaction_term)
pr_nw <- lapply(pre_models,     nw_stats, term = "ID")
cr_nw <- lapply(crisis_models,  nw_stats, term = "ID")

grab <- function(lst, field) sapply(lst, `[[`, field)

## ---------------------------------------------------------------
## 3. Serial correlation diagnostics (Breusch-Godfrey, orders 1-2)
## ---------------------------------------------------------------

bg_row <- function(mod) {
  b1 <- bgtest(mod, order = 1)
  b2 <- bgtest(mod, order = 2)
  c(bg1_stat = unname(b1$statistic), bg1_p = unname(b1$p.value),
    bg2_stat = unname(b2$statistic), bg2_p = unname(b2$p.value),
    dw       = unname(dwtest(mod)$statistic))
}

bg_ov <- t(sapply(overall_models, bg_row))
bg_pr <- t(sapply(pre_models,     bg_row))
bg_cr <- t(sapply(crisis_models,  bg_row))

diag_tab <- data.frame(
  Indicator = indicator_labels,
  BG1_p_overall = fmt(bg_ov[, "bg1_p"]),
  BG2_p_overall = fmt(bg_ov[, "bg2_p"]),
  DW_overall    = fmt(bg_ov[, "dw"]),
  BG1_p_pre     = fmt(bg_pr[, "bg1_p"]),
  BG2_p_pre     = fmt(bg_pr[, "bg2_p"]),
  DW_pre        = fmt(bg_pr[, "dw"]),
  BG1_p_crisis  = fmt(bg_cr[, "bg1_p"]),
  BG2_p_crisis  = fmt(bg_cr[, "bg2_p"]),
  DW_crisis     = fmt(bg_cr[, "dw"]),
  stringsAsFactors = FALSE
)

colnames(diag_tab) <- c("Indicator",
                        "BG(1) p", "BG(2) p", "DW",
                        "BG(1) p", "BG(2) p", "DW",
                        "BG(1) p", "BG(2) p", "DW")

stargazer(diag_tab, summary = FALSE, rownames = FALSE,
          type = "html", out = "TableS1_serialcorr.html",
          title = paste("Serial correlation diagnostics.",
                        "Breusch-Godfrey p-values (orders 1 and 2)",
                        "and Durbin-Watson statistics."),
          column.labels   = c("", "Overall", "Pre-Crisis (1990-2004)",
                              "Crisis (2005-2020)"),
          column.separate = c(1, 3, 3, 3),
          header = FALSE)

## ---------------------------------------------------------------
## 4. Main table, Newey-West standard errors
## ---------------------------------------------------------------

nw_tab <- data.frame(
  Indicator = indicator_labels,
  ov_coef = paste0(fmt(grab(ov_nw, "beta")), grab(ov_nw, "star")),
  ov_ci   = paste0(fmt(grab(ov_nw, "lower")), ", ", fmt(grab(ov_nw, "upper"))),
  pr_coef = paste0(fmt(grab(pr_nw, "beta")), grab(pr_nw, "star")),
  pr_ci   = paste0(fmt(grab(pr_nw, "lower")), ", ", fmt(grab(pr_nw, "upper"))),
  cr_coef = paste0(fmt(grab(cr_nw, "beta")), grab(cr_nw, "star")),
  cr_ci   = paste0(fmt(grab(cr_nw, "lower")), ", ", fmt(grab(cr_nw, "upper"))),
  stringsAsFactors = FALSE
)


round(cbind(overall = sapply(overall_models, function(m) summary(m)$r.squared),
            pre     = sapply(pre_models,     function(m) summary(m)$r.squared),
            crisis  = sapply(crisis_models,  function(m) summary(m)$r.squared)), 3)

colnames(nw_tab) <- c("Indicator",
                      "Interaction", "95% C.I.",
                      "Annual Change", "95% C.I.",
                      "Annual Change", "95% C.I.")

stargazer(nw_tab, summary = FALSE, rownames = FALSE,
          type = "html", out = "TableS2_neweywest.html",
          title = paste0("Fertility indicators, Puerto Rico 1990-2020. ",
                         "Newey-West HAC standard errors, ",
                         nw_lag(overall_models[[1]]), " lags."),
          column.labels   = c("", "Overall", "Pre-Crisis (1990-2004)",
                              "Crisis (2005-2020)"),
          column.separate = c(1, 2, 2, 2),
          header = FALSE)

## ---------------------------------------------------------------
## 5. OLS vs NW comparison (for your own inspection / appendix)
## ---------------------------------------------------------------

compare_tab <- data.frame(
  Indicator = indicator_labels,
  Beta      = fmt(grab(ov_nw, "beta")),
  SE_OLS    = fmt(grab(ov_nw, "se_ols")),
  SE_NW     = fmt(grab(ov_nw, "se_nw")),
  Ratio     = fmt(grab(ov_nw, "se_nw") / grab(ov_nw, "se_ols")),
  p_OLS     = fmt(grab(ov_nw, "p_ols")),
  p_NW      = fmt(grab(ov_nw, "p_nw")),
  Changed   = ifelse((grab(ov_nw, "p_ols") < 0.05) !=
                       (grab(ov_nw, "p_nw")  < 0.05), "YES", ""),
  stringsAsFactors = FALSE
)

colnames(compare_tab) <- c("Indicator", "Coefficient", "SE (OLS)", "SE (NW)",
                           "Ratio NW/OLS", "p (OLS)", "p (NW)",
                           "Inference changed")

stargazer(compare_tab, summary = FALSE, rownames = FALSE,
          type = "html", out = "TableS3_ols_vs_nw.html",
          title = paste("Interaction term: classical versus",
                        "Newey-West standard errors."),
          header = FALSE)

cat("\n--- Interaction term: OLS vs Newey-West ---\n")
print(compare_tab, right = FALSE)

## ---------------------------------------------------------------
## 6. Sensitivity: 2005 coded as pre-crisis
## ---------------------------------------------------------------

Data_alt <- Data_fecun
Data_alt$Crisis <- ifelse(Data_alt$Year == 2005, 0, Data_alt$Crisis)

outcomes <- c("log_BR", "log_asfr15_19", "log_asfr20_24", "log_asfr25_29",
              "log_asfr30_34", "log_asfr35_39", "log_asfr40_44",
              "log_TFR", "porctotal15_44")

alt_models <- lapply(outcomes, function(y) {
  lm(as.formula(paste0(y, " ~ ID + Crisis + ID*Crisis")), data = Data_alt)
})

alt_nw <- lapply(alt_models, nw_stats, term = interaction_term)

sens_tab <- data.frame(
  Indicator   = indicator_labels,
  Break2005   = paste0(fmt(grab(ov_nw, "beta")),  grab(ov_nw, "star")),
  CI_2005     = paste0(fmt(grab(ov_nw, "lower")), ", ", fmt(grab(ov_nw, "upper"))),
  Break2006   = paste0(fmt(grab(alt_nw, "beta")), grab(alt_nw, "star")),
  CI_2006     = paste0(fmt(grab(alt_nw, "lower")), ", ", fmt(grab(alt_nw, "upper"))),
  stringsAsFactors = FALSE
)

colnames(sens_tab) <- c("Indicator", "Interaction", "95% C.I.",
                        "Interaction", "95% C.I.")

stargazer(sens_tab, summary = FALSE, rownames = FALSE,
          type = "html", out = "TableS4_break_sensitivity.html",
          title = paste("Sensitivity of the interaction term to the placement",
                        "of the crisis onset. Newey-West standard errors."),
          column.labels   = c("", "Onset 2005", "Onset 2006"),
          column.separate = c(1, 2, 2),
          header = FALSE)

## ---------------------------------------------------------------
## 7. Reporting summary
## ---------------------------------------------------------------

cat("\n--- For the methods section ---\n")
cat("Sample sizes: overall n =", ov_nw[[1]]$n,
    "| pre-crisis n =", pr_nw[[1]]$n,
    "| crisis n =", cr_nw[[1]]$n, "\n")
cat("Newey-West lags: overall =", ov_nw[[1]]$lag,
    "| pre-crisis =", pr_nw[[1]]$lag,
    "| crisis =", cr_nw[[1]]$lag, "\n")
cat("Models where inference changed under NW:",
    sum(compare_tab$`Inference changed` == "YES"), "of 9\n\n")

####################################################################
##                            END   OF PAPER                              ##
####################################################################



#SUPP ANALYSIS
library(sandwich)
library(lmtest)

# --- assumes Data_fecun is loaded, with Year and the outcome columns ---

outcomes <- c("log_BR", "log_asfr15_19", "log_asfr20_24", "log_asfr25_29",
              "log_asfr30_34", "log_asfr35_39", "log_asfr40_44",
              "log_TFR", "porctotal15_44")

indicator_labels <- c("Crude Birth Rate (CBR)", "ASFR 15-19", "ASFR 20-24",
                      "ASFR 25-29", "ASFR 30-34", "ASFR 35-39", "ASFR 40-44",
                      "Total Fertility Rate (TFR)", "% Population Women 15-44")

nw_lag <- function(mod) {
  n <- length(residuals(mod))
  max(1, floor(4 * (n / 100)^(2/9)))
}

# fit interaction model for one outcome under a given crisis coding,
# return NW coef + CI on the interaction term
nw_interaction <- function(y, data) {
  m  <- lm(as.formula(paste0(y, " ~ ID + Crisis + ID*Crisis")), data = data)
  L  <- nw_lag(m)
  V  <- NeweyWest(m, lag = L, prewhite = FALSE, adjust = TRUE)
  ct <- coeftest(m, vcov. = V)
  ci <- coefci(m, parm = "ID:Crisis", level = 0.95, vcov. = V)
  data.frame(beta = ct["ID:Crisis","Estimate"],
             lower = ci[1], upper = ci[2],
             p = ct["ID:Crisis","Pr(>|t|)"])
}

# --- Build the two codings explicitly from Year ---
d2005 <- Data_fecun; d2005$Crisis <- ifelse(d2005$Year >= 2005, 1, 0)
d2006 <- Data_fecun; d2006$Crisis <- ifelse(d2006$Year >= 2006, 1, 0)

res2005 <- do.call(rbind, lapply(outcomes, nw_interaction, data = d2005))
res2006 <- do.call(rbind, lapply(outcomes, nw_interaction, data = d2006))

fmt <- function(x) sprintf("%.3f", x)
star <- function(p) ifelse(p < 0.05, "*", "")

sens_tab <- data.frame(
  Indicator  = indicator_labels,
  Int_2005   = paste0(fmt(res2005$beta), star(res2005$p)),
  CI_2005    = paste0(fmt(res2005$lower), ", ", fmt(res2005$upper)),
  Int_2006   = paste0(fmt(res2006$beta), star(res2006$p)),
  CI_2006    = paste0(fmt(res2006$lower), ", ", fmt(res2006$upper)),
  stringsAsFactors = FALSE
)

print(sens_tab, right = FALSE)
