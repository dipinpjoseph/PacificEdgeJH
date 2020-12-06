# Loading packages for backend, since most logical work is done here and server.R
library(shiny)
library(dplyr)
library(mosaicData)
library(shinycssloaders)
library(visdat)
library(recipes)
library(ggplot2)
library(plotly)
library(reshape2)
library(aplpack)
library(dbscan)
library(caret)
library(e1071)

# set the seed to make   partition reproducible
set.seed(7)

options(
  spinner.color = "#39e6f7",
  spinner.type = 7,
  spinner.size = 1
)

data("SaratogaHouses")
cols <- colnames(SaratogaHouses)
cols_num_all <- cols[1:10]
cols_num <- cols[2:10]
cols_cat <- cols[11:16]

# 70% of the sample size
smp_size <- floor(0.7 * nrow(SaratogaHouses))

# Train and Test Split
train_ind <- sample(seq_len(nrow(SaratogaHouses)), size = smp_size)
df_t <- SaratogaHouses
tr <- SaratogaHouses[train_ind,]
te <- SaratogaHouses[-train_ind,]

# Performing Yeo Johnson transform for Normalization
# Setting variable price as outcome and remaining numerics as predictors
yj_estimates <-
  recipe(as.formula(paste("price ~ ", paste(cols_num, collapse = " + "))), data = tr) %>%
  # Power transformation step
  step_YeoJohnson(all_numeric()) %>%
  # Feeds training data
  prep(data = tr)
# The trained process is run of test set
yj_t <- bake(yj_estimates, te)

# Mahalanobis distance
rec_mh <-
  recipe(as.formula(paste("price ~ ", paste(cols_num, collapse = " + "))), data = tr) %>%
  #step_naomit(everything()) %>%  # Not needed because of lack in missing data
  step_nzv(all_predictors()) %>%  # Removes near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # Remove predictors that are linear combinations of other predictors
  step_YeoJohnson(all_predictors()) %>% # Power transformation step
  prep(data = tr) # Feeds training data
pr_mh <- bake(rec_mh, te) # The trained process is run of test set

varMat <- var(pr_mh) # calculate the covariance matrix
colM <- colMeans(pr_mh) # calculate variable means
md2 <- mahalanobis(x = pr_mh, center = colM, cov = varMat)
# Threshold of 97.5% gives optimum output. A higher threshold skips some outliers
threshold <- qchisq(p = 0.975, df = ncol(pr_mh))
# Fetching row numbers of outlier observations
rows_mh <- c()
for (i in 1:length(md2)) {
  if (md2[i] > threshold) {
    rows_mh = c(rows_mh, i)
  }
}
# Detected outliers from mahalanobis distance method
te_mh <- te[rows_mh, ]

# Cook's Distance
# Creating a linear model with price as outcome and remaining numeric variables as predictors
lmod <-
  glm(formula = as.formula(paste("price ~ ", paste(cols_num, collapse = " + "))),
      data = pr_mh,
      family = gaussian)
# Calculating Cook's distance
dc <- cooks.distance(lmod)
thresh <-
  4 * mean(dc)  # Standard way of selecting threshold. 4 times mean value
# Data for plotting
dfcd <- data.frame(dc, id = 1:length(dc) / length(dc))
# Fetching row numbers of outlier observations
rows_ck <- c()
for (i in 1:length(dc)) {
  if (dc[i] > thresh) {
    rows_ck = c(rows_ck, i)
  }
}
# Detected outliers by Cook's distance
te_ck <- te[rows_ck, ]

# LOF
# Selecting all numeric columns
pr_lof <- te[cols_num_all]
# Size of neighbourhood - 5
d <- dbscan::lof(pr_lof, k = 5)
pr_lof$distance <- d
pr_lof <- pr_lof[order(d, decreasing = TRUE), ]
# Distance 1.75 gives optimum outliers
pr_lof <- pr_lof[pr_lof$distance > 1.75, ]
# Saving row names of outlier obs
rows_lof <- rownames(pr_lof)

# DB Scan
rec_db <-
  recipe(as.formula(paste("price ~ ", paste(cols_num, collapse = " + "))), data = tr) %>%
  step_naomit(everything()) %>%  # Not needed because of lack in missing data
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  prep(data = tr)
pr_db <- bake(rec_db, te)

# From Elbow method value of eps was found to be 25000
clustered <- dbscan::dbscan(pr_db, eps = 25000, minPts = 4)
# Fetching outlier observations
pr_db_te <- te[clustered$cluster == 0, ]
# Saving row names of outlier obs
rows_db <- rownames(pr_db_te)

# Outliers from 5 point summary after YJ transform
b_df = yj_t
rows_five <- c()
for (k in 1:length(cols_num_all)) {
  dfk <- data.frame(y = b_df[[cols_num_all[k]]])
  fnum = (fivenum(dfk$y))
  # Calculating end point of whiskers
  low = fnum[2] - 1.5 * (fnum[4] - fnum[2])
  high = fnum[4] + 1.5 * (fnum[4] - fnum[2])
  for (i in 1:nrow(dfk)) {
    # Checking if the value is novel
    if (dfk$y[i] > high | dfk$y[i] < low) {
      rows_five = c(rows_five, i)
    }
  }
}
# Saving outlier rows from 5-point summary
out_five = unique(rows_five)

# Z Score
rows_z <- c()
for (k in 1:length(cols_num_all)) {
  dfk <- data.frame(y = df_t[[cols_num_all[k]]])
  dfk <- scale(dfk)
  for (i in 1:length(dfk)) {
    # Z-score of point outside (-3,3) is considered as outlier
    if (dfk[i] > 3 | dfk[i] < -3) {
      rows_z = c(rows_z, i)
    }
  }
}
# Fetching row numbers and observations of outlier observations
rows_z = unique(rows_z)

# Outliers from 5 point summary for Raw Data
b_df = df_t
rows_5_raw <- c()
for (k in 1:length(cols_num_all)) {
  dfk <- data.frame(y = b_df[[cols_num_all[k]]])
  fnum = (fivenum(dfk$y))
  # Calculating end point of whiskers
  low = fnum[2] - 1.5 * (fnum[4] - fnum[2])
  high = fnum[4] + 1.5 * (fnum[4] - fnum[2])
  for (i in 1:nrow(dfk)) {
    # Checking if the value is novel
    if (dfk$y[i] > high | dfk$y[i] < low) {
      rows_5_raw = c(rows_5_raw, i)
    }
  }
}
# Saving outlier rows from 5-point summary
rows_5_raw = unique(rows_5_raw)

# Support Vector Machines
# Detecting outliers in training set
rows_tr <- c()
# Using Z -score method to find outliers in training observations
for (k in 1:length(cols_num_all)) {
  dfk <- data.frame(y = tr[[cols_num_all[k]]])
  dfk <- scale(dfk)
  for (i in 1:length(dfk)) {
    if (dfk[i] > 3 | dfk[i] < -3) {
      rows_tr = c(rows_tr, i)
    }
  }
}
rows_tr = unique(rows_tr)
# nu calculation
nu_val = length(rows_tr) / (length(tr$price) - length(rows_tr))
# Trains 1-class SVM with numeric training observations
model <-
  e1071::svm(
    tr[cols_num_all],
    y = NULL,
    type = 'one-classification',
    nu = nu_val,
    scale = TRUE,
    kernel = "radial"
  )
# Predicting outliers on test numeric variables
good <- predict(model, te[cols_num_all])
svm_out <- te[!good, ]
# Saving rownames of outliers
rows_svm <- rownames(svm_out)

# Robust Methods
getMethods <- function() {
  mi <- caret::getModelInfo()
  Label <- vector(mode = "character", length = length(mi))
  Tags <- vector(mode = "character", length = length(mi))
  Regression <- vector(mode = "logical", length = length(mi))
  Classification <- vector(mode = "logical", length = length(mi))
  for (row in 1:length(mi)) {
    Label[row] <- mi[[row]]$label
    Tags[row] <- paste(collapse = ", ", mi[[row]]$tags)
    Regression[row] <- "Regression" %in% mi[[row]]$type
    Classification[row] <- "Classification" %in% mi[[row]]$type
  }
  data.frame(
    Model = names(mi),
    Tags,
    Regression,
    Classification,
    stringsAsFactors = FALSE
  )
}
methods <- getMethods()
rb_methods <-
  methods[grepl(methods$Tags, pattern = ".*Robust Model.*", ignore.case = TRUE),]

# Integration of outlier observations - Some methods provide row numbers while other give row names
# Row indices
rows_out_int <- out_five
#rows_out_int <- intersect(rows_out_int, rows_z)
rows_out_int <- intersect(rows_out_int, rows_mh)
rows_out_int <- intersect(rows_out_int, rows_ck)
# Outlier observation intersection - Part 1
te_int_out <- te[rows_out_int, ]
# Row Names
rows_out_ch <- rows_db
rows_out_ch <- intersect(rows_out_ch, rows_lof)
rows_out_ch <- intersect(rows_out_ch, rows_svm)
# Outlier observation intersection - Part 2
te_ch_out <- te[rows_out_ch, ]
# Intersection of outlier observations
te_out_final = inner_join(te_int_out, te_ch_out)

# Outlier collection from Bag Plot Highly expensive
# 
# rows_bag_raw <- c()
# for (k in 'price') {
#   for (l in 'age'){
#     df_med = compute.bagplot(df_t[k], df_t[l])["pxy.outlier"]
#     len_bag = length(df_med$pxy.outlier)
#       for (i in 1:length(len_bag/2)){
#         get_r <- unique(which(df_t[[k]] == df_med$pxy.outlier[i]),which(df_t[[l]] == df_med$pxy.outlier[i+len_bag/2]))
#         #print(str(get_r))
#         print(get_r)
#         rows_bag_raw <- c(unlist(get_r, use.names=FALSE), rows_bag_raw)
#       }
#   }
# }
# rows_bag_raw = unique(rows_bag_raw)
# print(rows_bag_raw)


# Intersection of outliers in Raw Data Z-Score and Five Point Summary
rows_out_raw <- intersect(rows_5_raw, rows_z)
    df_t_out <- df_t[rows_out_raw, ]

