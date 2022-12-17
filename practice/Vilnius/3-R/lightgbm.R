# install.packages("lightgbm", repos = "https://cran.r-project.org")
library(lightgbm)
library(pROC)

train <- read.csv("../../../project/1-data/train_data.csv")

train_x = train[, c(-1,-2)]
train_x <- as.matrix(train_x)
train_y = train$y

dtrain = lgb.Dataset(train_x, label = train_y)

params = list(
  objective="binary",
  boosting="gbdt",
  num_leaves=100,
  max_depth=20
)

model = lgb.train(
  params = params,
  data = dtrain

)

pred_y = predict(model, train_x)

tree_imp = lgb.importance(model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 5L, measure = "Gain")


roc_obj <- roc(train$y, pred_y)
auc(roc_obj)
plot(roc_obj)
