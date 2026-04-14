#### predicting surface fluxes using random forest #####

# load library 
library(ranger)

# load dataset
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity = features
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m = Y


# combine
data = slow_profile_data%>%
  left_join(Eco_data_30m, by = "datetime")

# split in training (70%) and test (30%)
sample <- sample(c(TRUE,FALSE), nrow(data), 
                 replace=TRUE, prob=c(0.5,0.5))

# creating training dataset
train_data  <- data[sample, ]

# creating testing dataset
test_data  <- data[!sample, ]

# predictor variables
#data$Ta_24m
#data$Ta_55m
#data$Ta_100m
#data$WS
#data$SW_IN_1_1_1



rf_model = ranger(
  formula = H_Wm2 ~ Ta_24m + Ta_55m + Ta_100m + WS + SW_IN_1_1_1, 
  data = train_data, 
  importance = "permutation", 
  scale.permutation.importance = TRUE, 
  mtry = 3
)


# predict
preds = predict(rf_model, data = test_data)

# scatter plot
model = lm(preds$predictions~test_data$H_Wm2)

plot(x = test_data$H_Wm2, 
     y = preds$predictions, las= 1, 
     xlab = "H observed [Wm2]", 
     ylab = "H RF [Wm2]", 
     xlim = c(-300, 850), 
     ylim = c(-300, 850))
grid()
abline(a = 0, b = 1, col = "black", lwd = 2, lty = 2)
abline(a = 0, b = 1, col = "white", lwd = 2, lty = 3)

abline(model, col = "red", lwd = 2)


# predict whole week or so

day = 15000
idx <- day:(day + 336)

#prediction for a week
preds_week = predict(rf_model, data = data[idx, ])

plot(
  x = data[idx, "datetime"], 
  y = preds_week$predictions, 
  type = "l", 
  col = "purple", 
  las = 1, 
  xlab = "", 
  ylab = "H [W/m²]"
)

lines(
  x = data$datetime[idx], 
  y = data$H_Wm2[idx], 
  col = "darkgreen"
)

abline(h = 0, col = "grey")



#### do the same with a GLM

# load dataset
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity = features
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m = Y


# combine
data = slow_profile_data%>%
  left_join(Eco_data_30m, by = "datetime")%>%
  select(Ta_24m, Ta_100m, Ta_55m, WS, SW_IN_1_1_1, datetime, H_Wm2)%>%
  drop_na()

# split in training (70%) and test (30%)
sample <- sample(c(TRUE,FALSE), nrow(data), 
                 replace=TRUE, prob=c(0.5,0.5))

# creating training dataset
train_data  <- data[sample, ]

# creating testing dataset
test_data  <- data[!sample, ]

# predictor variables
#data$Ta_24m
#data$Ta_55m
#data$Ta_100m
#data$WS
#data$SW_IN_1_1_1



glm_model = lm(
  formula = H_Wm2 ~ poly(Ta_24m, 3) * poly(Ta_55m, 3) * poly(Ta_100m, 3) * poly(WS, 3) * poly(SW_IN_1_1_1, 3), 
  data = train_data
)


# predict
preds = predict(glm_model, newdata = test_data)

valid <- abs(preds) < 1000

preds <- preds[valid]
test_data_H   <- test_data$H_Wm2[valid]

# scatter plot
model = lm(preds~test_data_H)

plot(x = test_data_H, 
     y = preds, las= 1, 
     xlab = "H observed [Wm2]", 
     ylab = "H RF [Wm2]", 
     xlim = c(-350, 800), 
     ylim = c(-350, 800))
grid()
abline(a = 0, b = 1, col = "black", lwd = 2, lty = 2)
abline(a = 0, b = 1, col = "white", lwd = 2, lty = 3)

abline(model, col = "red", lwd = 2)


# predict whole week or so

day = 5000
idx <- day:(day + 336)

#prediction for a week
preds_week = predict(glm_model, newdata = data[idx, ])

plot(
  x = data[idx, "datetime"], 
  y = preds_week, 
  type = "l", 
  col = "purple", 
  las = 1, 
  xlab = "", 
  ylab = "H [W/m²]"
)

lines(
  x = data$datetime[idx], 
  y = data$H_Wm2[idx], 
  col = "darkgreen"
)

abline(h = 0, col = "grey")



