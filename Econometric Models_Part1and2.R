################################################################################
# CUSTOMER ANALYTICS AND CUSTOMER INSIGHTS WS 2021-22                          #
################################################################################
# CBC: Econometric Models - Part 1 and 2                                       #
################################################################################

# Preliminary Steps ============================================================
# Set the working directory ----------------------------------------------------
# Example
setwd("~/Users/MUSTERMANN/Desktop/R")  # (Mac)
setwd("C:/Users/MUSTERMANN/Desktop/R") # (Windows)

# For me
mainPath <- file.path("C:", "Users", "narin", "Dropbox", "TEACHING",
                      "CACI", "CACI_WS2122", "Rcode", "8_CBC")
setwd(mainPath)

# (Install and) Load the necessary packages ------------------------------------
pacman::p_load(reshape2, ggplot2, fastDummies, mlogit)


# Input Data ===================================================================
# Design matrix ----------------------------------------------------------------
design <- read.csv("CBC_design_matrix.csv")
head(design)

# Rename the attribute columns and label the values
columnNames = c("Version", "Task", "Alt", "Storage", "Size", "Color", "Price")

storage = c(4, 8, 16)
size = c(5, 6, 7)
color = c("black", "white", "silver")
price = c(79, 99, 119, 139)

colnames(design) = columnNames
design$Storage = factor(design$Storage, labels = storage)
design$Size = factor(design$Size, labels = size)
design$Color = factor(design$Color, labels = color)
design$Price = factor(design$Price, labels = price)

head(design)

# Coding of attributes ---------------------------------------------------------
# Dummy coding of all attributes (we can do this manually, but this is tedious.
# Instead, we will use the package fastDummies)
design = dummy_cols(design, select_columns = c("Storage", "Size", 
                                               "Color", "Price"),
                    remove_first_dummy = TRUE)
head(design)

# Effect-coding, we will create based on the dummy coding. Whenever the reference 
# (left-out) level was shown, the rest take value of -1
attr = c("Storage", "Size", "Color", "Price")

for(i in attr){
  # select column representing dummies for an attribute
  coltemp = names(design[, grep(colnames(design), pattern = paste0(i, "_"))])
  
  # compute rowSum
  design[, "sumtemp"] = rowSums(design[, coltemp])
  
  # If the sum is 0 all effect-coded variables representing the attribute are -1,
  # else the same as dummy coding
  for(m in 1:length(coltemp)){
    design[, paste0("E_", coltemp[m])] = ifelse(design$sumtemp == 0, -1, 
                                                design[, coltemp[m]])
  }
}
design$sumtemp = NULL

head(design)
str(design)

# Add linear coding for numeric attributes Storage, Size, Price
# and overwrite corresponding variables (currently are factors)
storage
design$Storage = as.numeric(design$Storage)
design$Storage = ifelse(design$Storage == 1, 4, 
                        ifelse(design$Storage == 2, 8, 16))

size
design$Size = as.numeric(design$Size)
design$Size = ifelse(design$Size == 1, 5, 
                     ifelse(design$Size == 2, 6, 7))

price
design$Price = as.numeric(design$Price)
design$Price = ifelse(design$Price == 1, 79, 
                      ifelse(design$Price == 2, 99, 
                             ifelse(design$Price == 3, 119, 139)))

head(design)
str(design)

# Choice matrix ----------------------------------------------------------------
choiceData <- read.csv("CBC_choice_matrix.csv")
head(choiceData)
dim(choiceData)
# This is what the typical input after data collection from Sawtooth will look like
# If needed you have time stamps, and can compute the total response time and 
# response time on each page of the questionnaire

# We will now select only those variables we are interested in
choiceData = choiceData[, c("sys_RespNum", "Gender", "Age", 
                            "sys_CBCVersion_CBC", 
                            paste0("CBC_Random", 1:10))]
head(choiceData)

# Recode Gender into a Female dummy (currently value = 1 is Male, = 2 is Female)
choiceData$Gender = ifelse(choiceData$Gender == 1, 0, 1)

# Rename the columns
colnames(choiceData) = c("id", "Female", "Age", "Version", paste0("Task_", 1:10))

# Reshape to long format
choiceData = melt(choiceData, id.vars = c("id", "Female", "Age", "Version"),
                  variable.name = "Task", value.name = "Choice")
head(choiceData)
str(choiceData)

# recode Task as numeric
choiceData$Task = as.character(choiceData$Task)
choiceData$Task = sapply(strsplit(choiceData$Task, "_"), `[`, 2)
choiceData$Task = as.integer(choiceData$Task)
head(choiceData)

# We will now expand (prelong) the data structure such that
# each row represents id-task-alt
N = length(unique(choiceData$id))     # number of resp.
T = length(unique(choiceData$Task))   # number of tasks per resp.
J = length(unique(choiceData$Choice)) # number of Alt (including none)

expand = data.frame(id = rep(1:N, each = T*J),
                    Task = rep(1:T, each = J),
                    Alt = 1:J)
head(expand, 20)
dim(expand)

# merge with choiceData
choiceData = merge(choiceData, expand, by = c("id", "Task"))
head(choiceData)
dim(choiceData)

# reorder the columns
choiceData = choiceData[, c("id", "Female", "Age", "Version",
                            "Task", "Alt", "Choice")]
head(choiceData)

# recode Choice as a dummy
choiceData$Choice = ifelse(choiceData$Choice == choiceData$Alt, 1, 0)
head(choiceData)

# Create a dummy for None
choiceData$None = ifelse(choiceData$Alt == 4, 1, 0)
head(choiceData)
str(choiceData)

# sort by id, Task, Alt
choiceData = choiceData[order(choiceData$id, choiceData$Task, choiceData$Alt), ]
rownames(choiceData) = NULL # reset the rownames
head(choiceData)
dim(choiceData)

# Merge with the design matrix -------------------------------------------------
choiceData = merge(choiceData, design, 
                   by = c("Version", "Task", "Alt"),
                   all.x = TRUE)
head(choiceData)
dim(choiceData)
str(choiceData)

# Note that we do not have values for the None alternative in the
# design matrix. After merging all attribute values are NA
# For the None, we code all attribute values as 0
# as we do not have any other missing values, we can simply 
# replace any NA with 0
choiceData[is.na(choiceData)] = 0
# There is a warning message as Color is a factor and NA is retained
# which is fine
head(choiceData)


# Estimation of MNL model ======================================================
# Create unique choice task counter corresponding 
# to unique id x Task combinatinations (we will call this chid)
# NOTE: recreate chid if you are subseting and using a 
#       subset of the original data
chid <- unique(choiceData[, c("id", "Task")])
dim(chid)
chid$chid <- 1:nrow(chid)
head(chid, 20)

choiceData <- merge(chid, choiceData, by = c("id", "Task"))
dim(choiceData)
head(choiceData)


# Indexing using dfidx() this will be the input into mlogit function 
# Note that the input into dfidx() SHOULD BE a data.frame object
choiceData_ml <- dfidx(choiceData, 
                       # specify the data structure (here in long format)
                       shape = "long", 
                       choice = "Choice", # name of the variable representing choice dummy
                       idx = list(c("chid", "id")), # outline panel structure 
                       idnames = c(NA, "alt"))
head(choiceData_ml)


# MNL -------------------------------------------------------------
# Log-likelihood of Null model, i.e., random choice
# T * I * ln(1/J), where
#   T - number of choice tasks
#   I - number of respondents
#   J - number of alternatives (including none)
T = length(unique(choiceData$Task))
I = length(unique(choiceData$id))
J = length(unique(choiceData$Alt))

LL0 = T * I * log(1/J) 
LL0 # -2772.589

# MNL: Linear coding for Price and dummy coding for other attributes
mnl_dummy <- mlogit(Choice ~ None + Price + Storage_8 + Storage_16 +
                      Size_6 + Size_7 + Color_white + Color_silver| 0, 
                    data = choiceData_ml)
summary(mnl_dummy)

# Linear coding for Price and effect-coding for other attributes
mnl_effect <- mlogit(Choice ~ None + Price + E_Storage_8 + E_Storage_16 +
                      E_Size_6 + E_Size_7 + E_Color_white + E_Color_silver| 0, 
                    data = choiceData_ml)
summary(mnl_effect)

# Log-likelihood
LL = as.numeric(mnl_dummy$logLik)

# Pseudo-R^2
r2 = 1 - (LL / LL0)
r2

# AIC and BIC
K = length(mnl_dummy$coefficients)
AIC = 2*K - 2*LL
BIC = K*log(T*I) - 2*LL

AIC
BIC

# to produce LaTeX table
library(stargazer)
stargazer(mnl_dummy, mnl_effect, align = TRUE)

# Recover left-out part-worth utilities
alpha = mnl_effect$coefficients
alpha["E_Storage_4"] = -(alpha["E_Storage_8"] + alpha["E_Storage_16"])
alpha["E_Size_5"] = -(alpha["E_Size_6"] + alpha["E_Size_7"])
alpha["E_Color_black"] = -(alpha["E_Color_white"] + alpha["E_Color_silver"])
round(alpha, 3)

# for dummy coding coefficients for reference level is fixed to 0
beta = mnl_dummy$coefficients
beta[c("Storage_4", "Size_5", "Color_black")] = 0

# rename the parameter names for convenience
# careful with the order
names(alpha) = c("None", "Price", paste0("Storage_", c(8, 16)),
                 paste0("Size_", 6:7),
                 paste0("Color_", c("white", "silver")),
                 "Storage_4", "Size_5", "Color_black")


# Let's work with more meaningful data ============================
# Simulated data on the example of ebook readers 
# from Eggers et al. (2020)
cbc = read.csv("ebook_readers.csv")
head(cbc)
dim(cbc)

I = length(unique(cbc$id))
T = length(unique(cbc$Task))
J = length(unique(cbc$Alt))


# Estimate several MNL models -------------------------------------
# Create a unique chid counter and index the data object
chid <- unique(cbc[, c("id", "Task")])
dim(chid)
chid$chid <- 1:nrow(chid)
head(chid, 20)

cbc <- merge(chid, cbc, by = c("id", "Task"))
dim(cbc)
head(cbc)
str(cbc)

# sort
cbc = cbc[order(cbc$id, cbc$chid, cbc$Alt), ]


# Indexing using dfidx() this will be the input into mlogit function 
# Note that the input into dfidx() SHOULD BE a data.frame object
cbc_ml <- dfidx(cbc, 
                shape = "long", 
                choice = "Choice", # name of the variable representing choice dummy
                idx = list(c("chid", "id")), # outline panel structure 
                idnames = c(NA, "alt"))
head(cbc_ml)


# LL of Null model
LL0 = T * I * log(1/J) 
LL0 # -2772.589

# 1. Main effect model with linear price effect and dummy-coding 
#    for other attributes
mnl1 <- mlogit(Choice ~ None + Price + Storage_4 + Storage_8 +
                Size_5 + Size_6 + Color_black + Color_white | 0, 
              data = cbc_ml)

summary(mnl1)


# 2. Main effect model with linear price effect and effect-coding 
#    for other attributes
mnl2 <- mlogit(Choice ~ None + Price + E_Storage_4 + E_Storage_8 +
                 E_Size_5 + E_Size_6 + E_Color_black + E_Color_white| 0, 
               data = cbc_ml)
summary(mnl2)


# 3. Main effect model with linear (mean-centered) price effect and 
#    effect-coding for other attributes 
# Mean-center price
cbc_ml$Price_MC = ifelse(cbc_ml$None == 0, cbc_ml$Price - mean(c(79, 99, 119, 139)), 0)

mnl3 <- mlogit(Choice ~ None + Price_MC + E_Storage_4 + E_Storage_8 +
                 E_Size_5 + E_Size_6 + E_Color_black + E_Color_white| 0, 
               data = cbc_ml)
summary(mnl3)


# Recover left-out part-worth utilities
coef_summary = as.data.frame(rbind(mnl2$coefficients, mnl3$coefficients))
coef_summary["E_Storage_16"] = -(coef_summary["E_Storage_4"] + coef_summary["E_Storage_8"])
coef_summary["E_Size_7"] = -(coef_summary["E_Size_5"] + coef_summary["E_Size_6"])
coef_summary["E_Color_silver"] = -(coef_summary["E_Color_black"] + coef_summary["E_Color_white"])
round(coef_summary, 3)

coef_summary$model = c("mnl2", "mnl3")
coef_summary = melt(coef_summary, id.vars = "model", variable.name = "param")

coef_summary$param = as.character(coef_summary$param)
coef_summary[!coef_summary$param %in% c("Price", "None"), ]$param = substr(coef_summary[!coef_summary$param %in% c("Price", "None"), ]$param, 
                                                             3, 
                                                             nchar(coef_summary[!coef_summary$param %in% c("Price", "None"), ]$param))

# estimates from dummy coding
beta = data.frame(model = "mnl1", 
                  param = coef_summary[coef_summary$model == "mnl2", ]$param,
                  value = c(as.vector(mnl1$coefficients), rep(0, 3)))

coef_summary = rbind(coef_summary, beta)

coef_summary = dcast(coef_summary, param ~ model)

coef_summary$param = factor(coef_summary$param, 
                            levels = c("None", "Price", 
                                       paste0("Storage_", c(4, 8, 16)),
                                       paste0("Size_", c(5, 6, 7)),
                                       paste0("Color_", c("black", "white", "silver"))))

# sort
coef_summary = coef_summary[order(coef_summary$param), ]
coef_summary


# Interpretation --------------------------------------------------
# Compute Relative importance (RI) of attributes
# Delete the None
coef_summary = coef_summary[-1, ]

# add attribute column
coef_summary$attr = c("Price", rep("Storage", 3),
                      rep("Size", 3), rep("Color", 3))


# Compute range of attributes (we will do this based on mnl2,
# Note that coefficient for mnl2 and mnl3 are the same,
# we would also get the same ranges based on mnl1
Range = tapply(coef_summary$mnl2, coef_summary$attr, 
            FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = coef_summary[coef_summary$param == "Price", ]$mnl2
price # price levels

# compute Range for price
Range["Price"] = abs(coef_price)*(max(price) - min(price))

# save as a data frame and merge with coef_summary
Range = data.frame(Range)
Range$attr = rownames(Range)
rownames(Range) = NULL

coef_summary = merge(coef_summary, Range, by = "attr")

# sort
coef_summary = coef_summary[order(coef_summary$param), ]
coef_summary

# Compute Relative importance
total_range = sum(Range$Range)
coef_summary$RI = coef_summary$Range / total_range * 100
coef_summary


# Utility in Monetary terms
# Utility in Monetary terms: divide the alphas by price coefficient
coef_summary$UM = coef_summary$mnl2/-coef_price
coef_summary[coef_summary$param == "Price", ]$UM = NA
coef_summary

# WTP (in reference to the 1st attribute levels)
# We compute based on parameters from dummy coding (betas), 
# but can similarly compute from partworth estimates (alphas)
coef_summary$WTP = coef_summary$mnl1/-coef_price
coef_summary[coef_summary$param == "Price", ]$WTP = NA
coef_summary


# Market Simulation ======================================================
# function for predictions
predict.mnl = function(X, beta, I, T, J) {
  
  beta = matrix(c(beta), nrow = I, 
                ncol = ncol(X), byrow = TRUE)
  
  data = NULL
  for (i in 1:I) {
    xi = as.matrix(X)
    Ti = T
    
    data[[i]] = data.frame(id = i, 
                           Task = rep(1:T, each = J), 
                           Alt = rep(1:J, T),
                           u = c(xi %*% beta[i,]))
  }
  data = do.call(rbind, data) 
  
  data$exp_u = exp(data$u)
  data$sum_exp_u = rep(tapply(data$exp_u, list(data$id, data$Task), sum), each = J)
  
  data$p = data$exp_u/data$sum_exp_u
  data$pred_choice = rep(tapply(data$p, list(data$id, data$Task), which.max), each = J)
  data$pred_choice = ifelse(data$pred_choice == data$Alt, 1, 0)
  
  
  data[, c("exp_u", "sum_exp_u")] = NULL
  return(data)
}

# market scenario: 
# alt 1: eBook reader with 4GB storage, 6-in. screen, in the color black for ???139
# alt 2: None
X = data.frame(None = c(0, 1),
               Price = c(139, 0),
               E_Storage_4 = c(1, 0),
               E_Storage_8 = c(0, 0),
               E_Size_5 = c(0, 0),
               E_Size_6 = c(1, 0),
               E_Color_black = c(1, 0),
               E_Color_white = c(0, 0))
X

predict.mnl(X, mnl2$coefficients, I = 1, T = 1, J = 2)


# Calculate the demand function ------------------------------------------
sim_price = seq(0, 150, by = 10)

p_hat = data.frame(price = sim_price, eBook = 0, None = 0)

for(i in 1:length(sim_price)){
  # rewrite price in X matrix
  X[1, "Price"] = sim_price[i]
  
  # compute predicted choice probs
  p_hat[i, c(2,3)] = predict.mnl(X, mnl2$coefficients, I = 1, T = 1, J = 2)$p
  
}
p_hat

# reshape to long format
p_hat = melt(p_hat, id.vars = "price", variable.name = "Alt")

# plot
ggplot(data = p_hat, aes(x = price, y = value, color = Alt)) +
  geom_point() + geom_line() +
  labs(x = "Price", y = "Demand (share of buyers)", color = "") +
  theme_bw()


# Incorporating systematic taste variations ==============================
# (related to observed variables: e.g., gender)
# Option 1: estimate the MNL for sub-samples
# Option 2: directly incorporate in the model through interaction terms
# Example on the RANDOMLY simulated data
mnl <- mlogit(Choice ~ None + Price + E_Storage_8 + E_Storage_16 +
                 E_Size_6 + E_Size_7 + E_Color_white + 
                 E_Color_silver +
                 
                 # interact each parameter with Female dummy
                 None:Female + Price:Female + E_Storage_8:Female + E_Storage_16:Female +
                 E_Size_6:Female + E_Size_7:Female + E_Color_white:Female + 
                 E_Color_silver:Female| 0, 
                     data = choiceData_ml)
summary(mnl)
