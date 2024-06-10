library(dplyr)
library(readr)

# What does the forecast look like if we base it in MSSTC through May 21?

env_file <- read_csv("../data/data/environment/environment.csv")
wip_msstc <- read_csv("../data/data/derived/msstc/ncep.reanalysis_may_daily.csv")
cpue <- read_csv("../data/data/cpue/cpue.csv")

msstc_21 <- wip_msstc |>
  group_by(year) |>
  filter(day <= 21) |>
  summarize(msstc_21 = mean(skt))

combined <- env_file |>
  left_join(msstc_21) |>
  left_join(cpue)

model_orig <- lm(mdj ~ msstc, subset(combined, year < 2024))
summary(model_orig)

# amatc + msstc_21
model_exp <- lm(fifdj ~ amatc + msstc_21, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)
model_exp <- lm(qdj ~ amatc + msstc_21, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)
model_exp <- lm(mdj ~amatc +  msstc_21, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)

#amatc
model_exp <- lm(fifdj ~ amatc, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)
model_exp <- lm(qdj ~ amatc, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)
model_exp <- lm(mdj ~amatc, subset(combined, year < 2024))
predict(model_exp, newdata = subset(combined, year == 2024), se.fit = TRUE)
###


forecast_year <- 2024
hindcast_window <- 15 # last n years
hindcast_years <- seq(forecast_year - hindcast_window, forecast_year - 1)
round_method <- floor # Floor predictions

models <- c(
  "fifdj ~ amatc",
  "fifdj ~ msstc",
  "fifdj ~ pice",
  "fifdj ~ amatc + msstc",
  "fifdj ~ amatc + pice",
  "fifdj ~ msstc + pice",
  "fifdj ~ amatc + msstc + pice",
  "fifdj ~ amatc + msstc_21"
)

hindcast_year <- function(data, model, forecast_year) {
  training_data <- data[data$year < forecast_year, ]
  training_model <- lm(formula(model), training_data)

  new_data <- data[data$year == forecast_year, ]
  prediction <- predict(training_model, newdata = new_data, se.fit = TRUE)
  prediction_fit <- round_method(prediction$fit[[1]])
  prediction_interval <- prediction_fit + c(-3.5, 3.5) * qnorm(0.975) *
    prediction$se.fit[[1]]

  # Extract response
  response_var = dimnames(attr(terms(as.formula(model)), "factors"))[[1]][1]
  actual <- new_data[[response_var]]

  in_interval <- actual >= round_method(prediction_interval[1]) &&
    actual <= round_method(prediction_interval[2])

  data.frame(
    "formula" = model,
    "year" = forecast_year,
    "predicted" = (prediction_fit),
    "observed" = actual,
    "diff" = prediction_fit - actual,
    "predict_se" = prediction$se.fit[[1]],
    "in_interval" = in_interval,
    "int_lower" = prediction_interval[1],
    "int_upper" = prediction_interval[2],
    "int_width" = prediction_interval[2] -
      prediction_interval[1]
  )
}

hindcast_model <- function(data, model, years, summarize = TRUE) {
  result <- lapply(years, function(year) {
    hindcast_year(data, model, year)
  })

  model_result <- do.call(rbind, result)

  if (!summarize) {
    return(model_result)
  }

  data.frame(
    model = model,
    "MAPE" = round(mean(abs(model_result$predicted - model_result$observed)), 2),
    "SDMAPE" = round(sd(abs(model_result$predicted - model_result$observed)), 2),
    "width" = round(mean(model_result$int_width), 2),
    "p.in" = round(sum(model_result$in_interval) / length(model_result$in_interval), 2),
    "absmax" = max(abs(model_result$predicted - model_result$observed)),
    "meanbias" = round(mean(model_result$predicted - model_result$observed), 2)
  )
}

hindcast_models <- function(data, models, years) {
  result <- lapply(models, function(model) {
    hindcast_model(data, model, years)
  })

  do.call(rbind, result)
}

model_selection_result <- hindcast_models(combined, models, hindcast_years)
model_selection_result
