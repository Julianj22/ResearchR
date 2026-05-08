

#' @title Simple Linear Regression
#' @description Creates a simple linear regression, or SLR, model.
#' Best to use when your predictor AND outcome variable are quantitative.
#' @param data This is the data set containing the variables you want use.
#' @param x Predictor variable.
#' @param y Outcome variable.
#' @return Some summary data related to the regression:
#' regression coefficient, p-value, Adjusted R-Squared.
#' @export

simple_linear_regression <- function(data, x, y) {

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  if (!is.numeric(data[[x_name]]) ||
      !is.numeric(data[[y_name]])) {
    stop("Wait a minute... your x and y variables must be numeric!")
  }

  form <- as.formula(paste(y_name, "~", x_name))
  model_sum <- summary(lm(form, data = data))

  slope <- model_sum$coefficients[2, 1]
  p_val <- model_sum$coefficients[2, 4]
  adj_r2 <- model_sum$adj.r.squared

  cat("\n--- Regression Results ---\n")
  cat("Model: ", y_name, "~", x_name, "\n")
  cat("Slope: ", round(slope, 4), "\n")
  cat("P-value: ", round(p_val, 5), "\n")
  cat("Adj. R-Squared: ", round(adj_r2, 4), "\n")
  cat("--------------------------\n")

  return(invisible(list(
    slope = slope,
    p_value = p_val,
    adj_r_squared = adj_r2
  )))
}



#' @title Logistic Regression
#' @description Creates a logistic regression model.
#' Best to use when your predictor variable is quantitative,
#' and your outcome variable is binary.
#' @param data This is the data set containing the variables you want use.
#' @param x Predictor variable.
#' @param y Outcome variable.
#' @return Some summary data related to the regression:
#' regression coefficient, p-value, Adjusted R-Squared.
#' @export

logistic_regression <- function(data, x, y) {

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  if (!is.numeric(data[[x_name]])) {
    stop("Wait a minute... your x variable must be numeric!")
  }

  if (length(unique(data[[y_name]])) != 2) {
    stop("Stop... your y variable must be binary!")
  }

  form <- as.formula(paste(y_name, "~", x_name))
  model_sum <- summary(glm(form, data = data, family = binomial))

  slope <- model_sum$coefficients[2, 1]
  p_val <- model_sum$coefficients[2, 4]

  # glm objects do not have adjusted R-squared
  adj_r2 <- NA

  cat("\n--- Regression Results ---\n")
  cat("Model: ", y_name, "~", x_name, "\n")
  cat("Slope: ", round(slope, 4), "\n")
  cat("P-value: ", round(p_val, 5), "\n")
  cat("Adj. R-Squared: ", adj_r2, "\n")
  cat("--------------------------\n")

  return(invisible(list(
    slope = slope,
    p_value = p_val,
    adj_r_squared = adj_r2
  )))
}



#' @title T-Test
#' @description Conducts a Welch's T-Test.
#' Best to use when your predictor variable is binary,
#' and your outcome variable is quantitative.
#' @param data This is the data set containing the variables you want use.
#' @param x Predictor variable.
#' @param y Outcome variable.
#' @return Some summary data related to the test:
#' means of each group, p-value, t-statistic.
#' @export

t_test <- function(data, x, y) {

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  if (!is.numeric(data[[y_name]])) {
    stop("Hey now... your y variable must be quantitative!")
  }

  if (length(unique(data[[x_name]])) != 2) {
    stop("Wait... your x variable must be binary!")
  }

  form <- as.formula(paste(y_name, "~", x_name))
  model_sum <- t.test(form, data = data)

  p_val <- model_sum$p.value
  t_stat <- model_sum$statistic
  estimate <- model_sum$estimate

  cat("\n--- Regression Results ---\n")
  cat("Model: ", y_name, "~", x_name, "\n")
  cat("T-Statistic: ", round(t_stat, 4), "\n")
  cat("P-value: ", round(p_val, 5), "\n")
  cat("Means: ", round(estimate, 4), "\n")
  cat("--------------------------\n")

  return(invisible(list(
    t_stat = t_stat,
    p_value = p_val,
    estimate = estimate
  )))
}



#' @title Analysis of Variance /ANOVA/ Test
#' @description Conducts a Welch's ANOVA.
#' Best to use when your predictor variable is categorical,
#' and your outcome variable is quantitative.
#' @param data This is the data set containing the variables you want use.
#' @param x Predictor variable.
#' @param y Outcome variable.
#' @return Some summary data related to the test:
#' means of each group, p-value, t-statistic.
#' @export

anova_test <- function(data, x, y) {

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  if (length(unique(data[[x_name]])) < 3) {
    stop("Wait... your x variable must be categorical and have more than 2 groups!")
  }

  if (!is.numeric(data[[y_name]])) {
    stop("Hey now... your y variable must be quantitative!")
  }

  form <- as.formula(paste(y_name, "~", x_name))
  model_sum <- oneway.test(form, data = data)

  f_stat <- model_sum$statistic
  p_val <- model_sum$p.value

  cat("\n--- ANOVA Results ---\n")
  cat("Model: ", y_name, "~", x_name, "\n")
  cat("F-Statistic: ", round(f_stat, 4), "\n")
  cat("P-value: ", round(p_val, 5), "\n")
  cat("--------------------------\n")

  return(invisible(list(
    p_value = p_val,
    f_stat = f_stat
  )))
}
