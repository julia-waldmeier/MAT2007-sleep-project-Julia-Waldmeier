cat("Julia Waldmeier\n")

# MAT2007 project, extra analysis: multiple regression of reported sleep on
# country, gender, age group and commute at the same time.
#
# sleep_analysis.R compares the groups of one variable at a time. Here all four
# variables are in one model, so every difference is measured while the other
# three variables are kept fixed (for example: men against women of the same
# country, age group and commute).
#
# Type source("regression_analysis.R") in the R terminal, with MAT2007-Project
# as the working directory. Only base R is used (lm, anova).

output_folder <- "output"

# Reuse the data settings and the loading and cleaning functions of the main
# analysis, so both scripts use exactly the same data. Only these definitions
# are run, not the whole main script.
reused_names <- c("data_file_path", "data_url", "country_names",
                  "load_sleep_data", "clean_sleep_data", "summarise_by_group",
                  "plot_resolution", "colour_main", "colour_other",
                  "colour_main_dark", "colour_other_dark", "colour_grid",
                  "colour_text", "uncertainty_label", "draw_error_bars",
                  "draw_legend", "open_plot_file")
for (expression in parse("sleep_analysis.R")) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.name(expression[[2]]) &&
      as.character(expression[[2]]) %in% reused_names) {
    eval(expression)
  }
}

sleep_data <- load_sleep_data(data_file_path, data_url)
sleep_data <- clean_sleep_data(sleep_data, country_names)
# All people, for the one-variable-at-a-time differences in the plot (the same
# data as in sleep_analysis.R)
all_people <- sleep_data

# The model needs every variable, so people without a commute answer are left
# out here
sleep_data <- sleep_data[!is.na(sleep_data$commute), ]
cat("People in the regression:", nrow(sleep_data), "\n")

# Each coefficient is a difference against a reference group. The references
# are the groups that report the most sleep in sleep_analysis.R, so every
# coefficient reads as "how much less sleep than the group that sleeps most".
sleep_data$country_name <- relevel(factor(sleep_data$country_name), "Albania")
sleep_data$gender <- relevel(factor(sleep_data$gender), "Women")
sleep_data$age_group <- relevel(factor(sleep_data$age_group), "19-31")
sleep_data$commute <- relevel(factor(sleep_data$commute), "< 30 min")

sleep_model <- lm(sleep ~ country_name + gender + age_group + commute,
                  data = sleep_data)
model_summary <- summary(sleep_model)
coefficient_table <- coef(model_summary)

# One coefficient as a difference in minutes, with its uncertainty (standard
# error) and its significance (difference / uncertainty)
coefficient_row <- function(coefficient_name, label) {
  if (!coefficient_name %in% rownames(coefficient_table)) {
    stop("no coefficient called ", coefficient_name)
  }
  estimate <- coefficient_table[coefficient_name, "Estimate"]
  uncertainty <- coefficient_table[coefficient_name, "Std. Error"]
  return(data.frame(comparison = label,
                    difference_minutes = round(60 * estimate, 1),
                    uncertainty_minutes = round(60 * uncertainty, 1),
                    significance = round(estimate / uncertainty, 1),
                    stringsAsFactors = FALSE))
}

group_results <- rbind(
  coefficient_row("genderMen", "Men - Women"),
  coefficient_row("age_group45-57", "Ages 45-57 - ages 19-31"),
  coefficient_row("commute> 60 min", "Commute > 60 min - < 30 min"))
cat("\nDifferences with the other variables kept fixed",
    "(minutes, negative = less sleep):\n")
print(group_results, row.names = FALSE)

lowest_countries <- c("Japan", "Philippines", "Israel", "Singapore",
                      "Indonesia", "Malaysia", "Puerto Rico", "Thailand",
                      "Saudi Arabia")
country_results <- do.call(rbind, lapply(lowest_countries, function(country) {
  coefficient_row(paste0("country_name", country),
                  paste(country, "- Albania"))
}))
cat("\nThe nine lowest countries against Albania, same age, gender and",
    "commute mix:\n")
print(country_results, row.names = FALSE)

# How much of the differences between people the four variables explain
# together, and how much each one adds when the other three are already in the
# model (the drop in explained variation when it is left out)
total_variation <- sum((sleep_data$sleep - mean(sleep_data$sleep))^2)
variable_tests <- drop1(sleep_model, test = "F")
variable_shares <- data.frame(
  variable = rownames(variable_tests)[-1],
  share_of_variation_percent = round(
    100 * variable_tests[-1, "Sum of Sq"] / total_variation, 2),
  F_value = round(variable_tests[-1, "F value"], 1),
  stringsAsFactors = FALSE)

cat(sprintf("\nAll four variables together explain %.1f%% of the differences",
            100 * model_summary$r.squared),
    "in reported sleep between people.\n")
cat("Share explained by each variable on its own, beyond the other three:\n")
print(variable_shares, row.names = FALSE)

# The same differences calculated one variable at a time, as in
# sleep_analysis.R: difference of the two group means, with the uncertainties
# added in quadrature
separate_difference <- function(values, groups, group_a, group_b) {
  group_summary <- summarise_by_group(values, groups)
  row_a <- group_summary[group_summary$group == group_a, ]
  row_b <- group_summary[group_summary$group == group_b, ]
  if (nrow(row_a) != 1 || nrow(row_b) != 1) {
    stop("group_a and group_b must each match exactly one group")
  }
  return(c(difference_minutes = 60 * (row_a$mean - row_b$mean),
           uncertainty_minutes = 60 * sqrt(row_a$uncertainty^2 +
                                             row_b$uncertainty^2)))
}

has_commute <- !is.na(all_people$commute)
separate_results <- rbind(
  separate_difference(all_people$sleep, all_people$gender, "Men", "Women"),
  separate_difference(all_people$sleep, all_people$age_group, "45-57",
                      "19-31"),
  separate_difference(all_people$sleep[has_commute],
                      all_people$commute[has_commute], "> 60 min",
                      "< 30 min"),
  do.call(rbind, lapply(lowest_countries, function(country) {
    separate_difference(all_people$sleep, all_people$country_name, country,
                        "Albania")
  })))

# Both versions of every difference in one plot: one row per comparison, the
# one-variable-at-a-time difference (blue) above the regression difference
# (orange), each with its uncertainty
plot_regression_comparison <- function(separate_results, model_results,
                                       file_name) {
  if (nrow(separate_results) != nrow(model_results)) {
    stop("separate_results and model_results must have the same rows")
  }
  num_rows <- nrow(model_results)
  # Top row first, with a gap between the group rows and the country rows
  y_positions <- rev(seq_len(num_rows)) + ifelse(seq_len(num_rows) <= 3, 1, 0)
  offset <- 0.17
  separate_low <- separate_results[, "difference_minutes"] -
    separate_results[, "uncertainty_minutes"]
  separate_high <- separate_results[, "difference_minutes"] +
    separate_results[, "uncertainty_minutes"]
  model_low <- model_results$difference_minutes -
    model_results$uncertainty_minutes
  model_high <- model_results$difference_minutes +
    model_results$uncertainty_minutes

  open_plot_file(file_name, 1800, 1500)
  par(mar = c(4.5, 13, 3, 1))
  plot(NA, xlim = c(min(separate_low, model_low) - 2, 5),
       ylim = c(0.4, max(y_positions) + 0.6), yaxt = "n",
       xlab = "Difference in average reported sleep (minutes)", ylab = "",
       main = "Differences one variable at a time and in the regression")
  grid(nx = NULL, ny = NA, col = colour_grid, lty = 1)
  abline(v = 0, col = colour_text, lty = 2)
  draw_error_bars(separate_low, y_positions + offset, separate_high,
                  y_positions + offset, colour_main_dark)
  draw_error_bars(model_low, y_positions - offset, model_high,
                  y_positions - offset, colour_other_dark)
  points(separate_results[, "difference_minutes"], y_positions + offset,
         pch = 19, cex = 1.4, col = colour_main)
  points(model_results$difference_minutes, y_positions - offset,
         pch = 19, cex = 1.4, col = colour_other)
  axis(2, at = y_positions, labels = model_results$comparison, tick = FALSE,
       cex.axis = 0.8)
  draw_legend("bottomright",
              legend = c("One variable at a time",
                         "Regression (other variables kept fixed)",
                         uncertainty_label),
              col = c(colour_main, colour_other, colour_main_dark),
              pch = c(19, 19, NA), pt.cex = 1.4, error_bar_entry = 3,
              error_bar_horizontal = TRUE)
  invisible(dev.off())
}

plot_regression_comparison(separate_results,
                           rbind(group_results, country_results),
                           "fig9_regression_comparison.png")

write.csv(rbind(group_results, country_results),
          file.path(output_folder, "regression_differences.csv"),
          row.names = FALSE)
cat("\nThe differences are saved in", file.path(output_folder,
                                                 "regression_differences.csv"),
    "and the plot in", file.path(output_folder,
                                 "fig9_regression_comparison.png"), "\n")
