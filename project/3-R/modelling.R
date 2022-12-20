library(h2o)
library(tidyverse)
library(dplyr)
h2o.init()

df <- h2o.importFile("C:/Users/admin/Downloads/Drive Data/full_train_data.csv")
df
class(df)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id", "C1"))
df$y <- as.factor(df$y)
summary(df)


splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%

### GBM

gbm_params1 <- list(max_depth = c(8, 15),
                    sample_rate = c(0.7, 1.0))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 50,
                      seed = 1234,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

model_gbm <- h2o.getModel("gbm_grid1_model_6")

model_gbm

h2o.performance(model_gbm, newdata = valid)

h2o.performance(model_gbm, newdata = test)

h2o.saveModel(model_gbm, "C:/Users/admin/Downloads/Drive Data/model", filename = "gmb_grid2")

model <- h2o.loadModel("C:/Users/admin/Downloads/Drive Data/model/gmb_grid2")

data <- read.csv("C:/Users/admin/Downloads/Drive Data/test_data.csv")
data <- data %>% mutate_all(~na_if(., "NA"))
test_data <- as.h2o(data)

h2o.varimp_plot(model)
predictions <- h2o.predict(model, test_data)

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/admin/Downloads/Drive Data/predictions2.csv")