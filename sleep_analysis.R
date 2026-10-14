cat("Julia Waldmeier\n")

# MAT2007 project: In which countries do people report the least sleep, and
# which groups report the least (by gender, age and length of commute)?
#
# Type source("sleep_analysis.R") in the R terminal, with MAT2007-Project as
# the working directory. This runs the whole analysis in order: it loads and
# cleans the data, prints the results and saves the plots in the output folder.
#
# Dataset: self-reported sleep duration of 730,187 people in 63 countries,
# collected through the mobile game Sea Hero Quest and published with
# Coutrot et al. (2022), Nature Communications 13, 7697.
# If the data file is missing, it is downloaded from the Open Science Framework.

data_file_path <- "data/clean_demog_data.csv"
data_url <- paste0("https://osf.io/download/cqjgs/",
                   "?view_only=92c1aa56abfc4d0f99501c9f260fa324")
output_folder <- "output"
home_country <- "Netherlands"   # always shown on the country plot
num_countries_shown <- 10       # lowest and highest countries on the plot
num_subsamples <- 10            # for the sub-sampling cross-check
set.seed(2007)                  # same random sub-samples in every run
plot_resolution <- 220          # higher values give larger text in the plots

# Colours used in all plots
colour_main  <- "#2a78d6"   # blue
colour_other <- "#eb6834"   # orange
colour_main_dark  <- "#12365f"   # dark blue, error bars on blue points
colour_other_dark <- "#8a3312"   # dark orange, error bars on orange points
uncertainty_label <- "Statistical uncertainty (\u00b1 1 standard error)"
colour_grid  <- "#e4e3df"
colour_text  <- "#52514e"

# The dataset stores countries as 2-letter codes, these are the full names
country_names <- c(
  AE = "United Arab Emirates", AL = "Albania", AR = "Argentina",
  AT = "Austria", AU = "Australia", BE = "Belgium", BR = "Brazil",
  CA = "Canada", CH = "Switzerland", CL = "Chile", CN = "China",
  CO = "Colombia", CR = "Costa Rica", CY = "Cyprus", CZ = "Czechia",
  DE = "Germany", DK = "Denmark", EC = "Ecuador", EG = "Egypt",
  ES = "Spain", FI = "Finland", FR = "France", GB = "United Kingdom",
  GR = "Greece", HK = "Hong Kong", HR = "Croatia", HU = "Hungary",
  ID = "Indonesia", IE = "Ireland", IL = "Israel", IN = "India",
  IR = "Iran", IT = "Italy", JP = "Japan", KR = "South Korea",
  LB = "Lebanon", MK = "North Macedonia", MX = "Mexico", MY = "Malaysia",
  NL = "Netherlands", NO = "Norway", NZ = "New Zealand", PE = "Peru",
  PH = "Philippines", PL = "Poland", PR = "Puerto Rico", PT = "Portugal",
  RO = "Romania", RS = "Serbia", RU = "Russia", SA = "Saudi Arabia",
  SE = "Sweden", SG = "Singapore", SK = "Slovakia", TH = "Thailand",
  TR = "Turkey", TW = "Taiwan", UA = "Ukraine", US = "United States",
  UY = "Uruguay", VE = "Venezuela", VN = "Vietnam", ZA = "South Africa"
)

# Step 1: load and clean the data

load_sleep_data <- function(input_file_path, download_url) {
  if (!is.character(input_file_path) || length(input_file_path) != 1) {
    stop("input_file_path must be a single string")
  }
  if (!file.exists(input_file_path)) {
    cat("Data file not found, downloading it (29 MB)...\n")
    dir.create(dirname(input_file_path), showWarnings = FALSE,
               recursive = TRUE)
    download.file(download_url, input_file_path, mode = "wb")
  }

  # na.strings = "" so that empty cells become NA, and nothing else does
  sleep_data <- read.csv(input_file_path, na.strings = "",
                         stringsAsFactors = FALSE)

  expected_columns <- c("age", "gender", "country", "sleep", "commute",
                        "education", "homeenv")
  if (!all(expected_columns %in% names(sleep_data))) {
    stop("the data file does not have the expected columns")
  }
  if (nrow(sleep_data) == 0) {
    stop("the data file contains no rows")
  }

  return(sleep_data)
}

clean_sleep_data <- function(sleep_data, country_names) {
  if (!is.data.frame(sleep_data)) {
    stop("sleep_data must be a data frame")
  }
  num_rows_before <- nrow(sleep_data)

  # Every analysis needs sleep, age, gender and country, so drop rows without
  # them. Commute is only needed for one plot, so missing values stay as NA.
  is_complete <- !is.na(sleep_data$sleep) & !is.na(sleep_data$age) &
    !is.na(sleep_data$gender) & !is.na(sleep_data$country)
  sleep_data <- sleep_data[is_complete, ]

  if (!is.numeric(sleep_data$sleep) || !is.numeric(sleep_data$age)) {
    stop("sleep and age must be numeric")
  }
  if (any(sleep_data$sleep < 0 | sleep_data$sleep > 24)) {
    stop("sleep contains values outside 0 to 24 hours")
  }

  unknown_codes <- setdiff(unique(sleep_data$country), names(country_names))
  if (length(unknown_codes) > 0) {
    stop("no country name for: ", paste(unknown_codes, collapse = ", "))
  }
  sleep_data$country_name <- unname(country_names[sleep_data$country])

  # Readable labels, in the order they should appear on the plots
  sleep_data$gender <- factor(sleep_data$gender, levels = c("f", "m"),
                              labels = c("Women", "Men"))
  sleep_data$commute <- factor(sleep_data$commute,
                               levels = c("less-30-mins", "30-mins-to-1-hour",
                                          "hour-plus"),
                               labels = c("< 30 min", "30-60 min", "> 60 min"))
  # Four age groups of equal width (13 years each)
  sleep_data$age_group <- cut(sleep_data$age,
                              breaks = c(18, 31, 44, 57, 70),
                              labels = c("19-31", "32-44", "45-57", "58-70"))

  cat("Rows removed in cleaning:", num_rows_before - nrow(sleep_data), "\n")
  return(sleep_data)
}

print_overview <- function(sleep_data) {
  cat("People:", nrow(sleep_data), "\n")
  cat("Countries:", length(unique(sleep_data$country)), "\n")
  cat("Age range:", min(sleep_data$age), "to", max(sleep_data$age), "\n")
  cat("Sleep range:", min(sleep_data$sleep), "to", max(sleep_data$sleep),
      "hours\n")
  cat("Missing commute answers:", sum(is.na(sleep_data$commute)), "\n")

  people_per_country <- table(sleep_data$country_name)
  cat("Fewest people in a country:", min(people_per_country),
      "(", names(which.min(people_per_country)), ")\n")
  cat("Most people in a country:", max(people_per_country),
      "(", names(which.max(people_per_country)), ")\n")
}

# Step 2: averages and uncertainties

# Mean sleep per group with its statistical uncertainty, the standard error
# of the mean: standard deviation / sqrt(number of people)
summarise_by_group <- function(values, groups) {
  if (!is.numeric(values) || length(values) != length(groups)) {
    stop("values must be numeric and as long as groups")
  }
  num_people <- tapply(values, groups, length)
  group_means <- tapply(values, groups, mean)
  group_sds <- tapply(values, groups, sd)

  group_summary <- data.frame(group = names(num_people),
                              num_people = as.vector(num_people),
                              mean = as.vector(group_means),
                              sd = as.vector(group_sds),
                              stringsAsFactors = FALSE)
  group_summary <- group_summary[!is.na(group_summary$num_people), ]
  if (any(group_summary$num_people < 2)) {
    stop("need at least 2 people per group to calculate an uncertainty")
  }
  group_summary$uncertainty <- group_summary$sd / sqrt(group_summary$num_people)
  rownames(group_summary) <- NULL

  return(group_summary)
}

# Cross-check of the uncertainty with the sub-sampling technique: split the
# values randomly into sub-samples and take the mean of each. The spread of
# those means shows how much a mean of one sub-sample varies. The mean of all
# values uses num_subsamples times more people, so its uncertainty is
# sd(sub-sample means) / sqrt(num_subsamples).
subsample_uncertainty <- function(values, num_subsamples) {
  if (!is.numeric(values) || anyNA(values)) {
    stop("values must be numeric without NA")
  }
  if (num_subsamples < 2 || length(values) < 2 * num_subsamples) {
    stop("need at least 2 sub-samples with at least 2 values each")
  }
  shuffled_values <- sample(values)
  subsample_labels <- rep(seq_len(num_subsamples),
                          length.out = length(values))
  subsample_means <- tapply(shuffled_values, subsample_labels, mean)

  return(sd(subsample_means) / sqrt(num_subsamples))
}

print_subsampling_check <- function(values, label, num_subsamples) {
  formula_uncertainty <- sd(values) / sqrt(length(values))
  cat(sprintf("  %-22s formula %.4f hours, sub-sampling %.4f hours\n", label,
              formula_uncertainty,
              subsample_uncertainty(values, num_subsamples)))
}

# Difference between two groups. The groups are independent, so the
# uncertainties are added in quadrature: sqrt(sigma_a^2 + sigma_b^2)
print_difference <- function(group_summary, group_a, group_b) {
  row_a <- group_summary[group_summary$group == group_a, ]
  row_b <- group_summary[group_summary$group == group_b, ]
  if (nrow(row_a) != 1 || nrow(row_b) != 1) {
    stop("group_a and group_b must each match exactly one group")
  }
  difference <- row_a$mean - row_b$mean
  difference_uncertainty <- sqrt(row_a$uncertainty^2 + row_b$uncertainty^2)

  # Significance: how many uncertainties (sigma) the difference is from zero
  significance <- abs(difference) / difference_uncertainty

  cat(sprintf("%s - %s: %.3f +- %.3f hours (%.1f +- %.1f minutes), %.1f sigma\n",
              group_a, group_b, difference, difference_uncertainty,
              60 * difference, 60 * difference_uncertainty, significance))
  return(invisible(c(difference = difference,
                     uncertainty = difference_uncertainty,
                     significance = significance)))
}

# Sub-sampling check of the uncertainty of a difference between two groups.
# The formula sqrt(sigma_a^2 + sigma_b^2) assumes the two group means are
# uncorrelated. Here the people of both groups are split randomly into
# num_subsamples sub-samples, and the difference is taken inside every
# sub-sample. The spread of those differences shows how much the difference
# really varies, without assuming anything about correlation:
#   uncertainty = sd(differences) / sqrt(num_subsamples)
# The correlation between the two groups' sub-sample means is also returned:
# close to 0 means the two means are uncorrelated. A single split of 10 values
# is noisy, so the split is repeated num_repeats times and averaged.
check_difference_by_subsampling <- function(values, groups, group_a, group_b,
                                            num_subsamples, num_repeats) {
  if (!is.numeric(values) || length(values) != length(groups)) {
    stop("values must be numeric and as long as groups")
  }
  is_compared <- !is.na(groups) & groups %in% c(group_a, group_b)
  values <- values[is_compared]
  in_group_a <- groups[is_compared] == group_a
  if (sum(in_group_a) < 2 * num_subsamples ||
      sum(!in_group_a) < 2 * num_subsamples) {
    stop("each group needs at least 2 people per sub-sample")
  }

  difference <- mean(values[in_group_a]) - mean(values[!in_group_a])
  formula_uncertainty <- sqrt(var(values[in_group_a]) / sum(in_group_a) +
                                var(values[!in_group_a]) / sum(!in_group_a))

  subsample_uncertainties <- numeric(num_repeats)
  correlations <- numeric(num_repeats)
  for (repeat_index in seq_len(num_repeats)) {
    subsample_labels <- sample(rep(seq_len(num_subsamples),
                                   length.out = length(values)))
    means_a <- tapply(values[in_group_a], subsample_labels[in_group_a], mean)
    means_b <- tapply(values[!in_group_a], subsample_labels[!in_group_a], mean)
    subsample_uncertainties[repeat_index] <- sd(means_a - means_b) /
      sqrt(num_subsamples)
    correlations[repeat_index] <- cor(means_a, means_b)
  }

  return(data.frame(comparison = paste(group_a, "-", group_b),
                    difference_minutes = round(60 * difference, 1),
                    formula_minutes = round(60 * formula_uncertainty, 2),
                    subsampling_minutes = round(
                      60 * mean(subsample_uncertainties), 2),
                    ratio = round(mean(subsample_uncertainties) /
                                    formula_uncertainty, 2),
                    mean_correlation = round(mean(correlations), 2),
                    stringsAsFactors = FALSE))
}

# Effect size of a difference between two groups: Hedges' g, the difference
# of the means divided by the pooled standard deviation of the individuals in
# both groups (with the small-sample correction). Unlike the significance, it
# does not grow with the number of people. Rule of thumb: 0.2 is small, 0.5
# medium and 0.8 large.
hedges_g <- function(values, groups, group_a, group_b) {
  if (!is.numeric(values) || length(values) != length(groups)) {
    stop("values must be numeric and as long as groups")
  }
  values_a <- values[!is.na(groups) & groups == group_a]
  values_b <- values[!is.na(groups) & groups == group_b]
  num_a <- length(values_a)
  num_b <- length(values_b)
  if (num_a < 2 || num_b < 2) {
    stop("each group needs at least 2 people")
  }
  pooled_sd <- sqrt(((num_a - 1) * var(values_a) +
                       (num_b - 1) * var(values_b)) / (num_a + num_b - 2))
  correction <- 1 - 3 / (4 * (num_a + num_b) - 9)
  return(correction * (mean(values_a) - mean(values_b)) / pooled_sd)
}

# The same difference, calculated separately inside every country.
# group_column is the name of the column that holds the two groups.
compare_within_countries <- function(sleep_data, group_column, group_a,
                                     group_b) {
  if (!group_column %in% names(sleep_data)) {
    stop("group_column must be the name of a column in sleep_data")
  }
  all_countries <- sort(unique(sleep_data$country_name))
  num_countries <- length(all_countries)
  differences <- numeric(num_countries)
  uncertainties <- numeric(num_countries)

  for (country_index in seq_len(num_countries)) {
    # Only the people of this country who are in one of the two groups
    is_compared <- sleep_data$country_name == all_countries[country_index] &
      sleep_data[[group_column]] %in% c(group_a, group_b)
    country_data <- sleep_data[is_compared, ]
    group_summary <- summarise_by_group(country_data$sleep,
                                        country_data[[group_column]])
    row_a <- group_summary[group_summary$group == group_a, ]
    row_b <- group_summary[group_summary$group == group_b, ]
    if (nrow(row_a) != 1 || nrow(row_b) != 1) {
      stop("a group is missing in ", all_countries[country_index])
    }
    differences[country_index] <- row_a$mean - row_b$mean
    uncertainties[country_index] <- sqrt(row_a$uncertainty^2 +
                                           row_b$uncertainty^2)
  }

  return(data.frame(country = all_countries,
                    difference_minutes = 60 * differences,
                    uncertainty_minutes = 60 * uncertainties,
                    significance = differences / uncertainties,
                    stringsAsFactors = FALSE))
}

print_within_countries <- function(country_differences, title,
                                   highlight_country) {
  num_countries <- nrow(country_differences)
  highlight_row <- country_differences[
    country_differences$country == highlight_country, ]

  cat("  Inside each country (", title, "):\n", sep = "")
  cat(sprintf("    same direction in %d of %d countries, %d of them by more than 2 sigma\n",
              sum(country_differences$difference_minutes > 0), num_countries,
              sum(country_differences$significance > 2)))
  cat(sprintf("    opposite direction by more than 2 sigma: %d countries\n",
              sum(country_differences$significance < -2)))
  cat(sprintf("    median difference: %.1f minutes\n",
              median(country_differences$difference_minutes)))
  cat(sprintf("    %s: %.1f +- %.1f minutes\n", highlight_country,
              highlight_row$difference_minutes,
              highlight_row$uncertainty_minutes))
}

print_summary <- function(group_summary, title) {
  cat("\n", title, "\n", sep = "")
  for (row_index in seq_len(nrow(group_summary))) {
    cat(sprintf("  %-22s %.3f +- %.3f hours  (%d people)\n",
                group_summary$group[row_index], group_summary$mean[row_index],
                group_summary$uncertainty[row_index],
                group_summary$num_people[row_index]))
  }
}

# Step 3: plots

draw_error_bars <- function(x_low, y_low, x_high, y_high, colour) {
  # arrows() cannot draw a bar of zero length, so skip those
  has_length <- (x_high - x_low) > 0 | (y_high - y_low) > 0
  suppressWarnings(
    arrows(x_low[has_length], y_low[has_length],
           x_high[has_length], y_high[has_length],
           angle = 90, code = 3, length = 0.03, col = colour, lwd = 1.5)
  )
}

# Legend in a white box, so that it stays readable on top of grid lines.
# error_bar_entry is the number of the legend entry that explains the error
# bars: a small error bar is drawn as its symbol, with the same shape and
# direction as the error bars in the plot.
draw_legend <- function(position, ..., error_bar_entry = NULL,
                        error_bar_colour = colour_main_dark,
                        error_bar_horizontal = FALSE) {
  legend_info <- legend(position, ..., bg = "white", box.col = colour_grid,
                        text.col = colour_text, cex = 0.8, inset = 0.02)
  if (!is.null(error_bar_entry)) {
    # The symbols sit halfway between the box edge and the start of the text
    symbol_x <- (legend_info$rect$left +
                   legend_info$text$x[error_bar_entry]) / 2
    symbol_y <- legend_info$text$y[error_bar_entry]
    if (error_bar_horizontal) {
      half_length <- 0.7 * strwidth("M", cex = 0.8)
      draw_error_bars(symbol_x - half_length, symbol_y,
                      symbol_x + half_length, symbol_y, error_bar_colour)
    } else {
      half_length <- 0.55 * strheight("M", cex = 0.8)
      draw_error_bars(symbol_x, symbol_y - half_length,
                      symbol_x, symbol_y + half_length, error_bar_colour)
    }
  }
}

open_plot_file <- function(file_name, width, height) {
  png(file.path(output_folder, file_name), width = width, height = height,
      res = plot_resolution)
  par(las = 1, bty = "l", col.axis = colour_text, col.lab = colour_text,
      fg = colour_text, cex.main = 1, font.main = 1)
}

# Histogram of how many people reported each number of hours
plot_sleep_distribution <- function(sleep_data, file_name) {
  mean_sleep <- mean(sleep_data$sleep)

  open_plot_file(file_name, 1600, 1100)
  par(mar = c(4.5, 5, 3, 1))
  sleep_histogram <- hist(sleep_data$sleep,
                          breaks = seq(min(sleep_data$sleep) - 0.5,
                                       max(sleep_data$sleep) + 0.5, by = 1),
                          plot = FALSE)
  plot(sleep_histogram, col = colour_main, border = "white", yaxt = "n",
       ylim = c(0, 1.12 * max(sleep_histogram$counts)),
       main = sprintf("Reported sleep per night of %s people in %d countries",
                      format(nrow(sleep_data), big.mark = ","),
                      length(unique(sleep_data$country))),
       xlab = "Reported sleep per night (hours)",
       ylab = "Number of people (thousands)")
  count_ticks <- pretty(c(0, max(sleep_histogram$counts)))
  axis(2, at = count_ticks, labels = count_ticks / 1000)
  # The mean line stops at the top of its bar, to keep the label above it clear
  mean_bar <- which.min(abs(sleep_histogram$mids - mean_sleep))
  segments(mean_sleep, 0, mean_sleep, sleep_histogram$counts[mean_bar],
           lty = 2, lwd = 2, col = colour_other)
  # Share of all people above each bar
  text(sleep_histogram$mids, sleep_histogram$counts,
       sprintf("%.0f%%", 100 * sleep_histogram$counts / nrow(sleep_data)),
       pos = 3, cex = 0.8, col = colour_text)
  draw_legend("topright",
              legend = c("People who gave this answer",
                         sprintf("Mean of all people (%.2f hours)", mean_sleep)),
              col = c(colour_main, colour_other), pch = c(15, NA),
              lty = c(NA, 2), lwd = c(NA, 2), pt.cex = 1.5)
  invisible(dev.off())
}

# Lowest and highest countries, plus highlight_country if it is not NULL.
# A gap is left wherever countries in between are not shown. If num_shown is
# at least half the number of countries, all countries are shown.
# for_report = TRUE uses a short title and leaves out the legend, because the
# figure caption in the report explains the symbols.
plot_sleep_by_country <- function(country_summary, overall_mean, num_shown,
                                  highlight_country, file_name,
                                  plot_height = 1700, for_report = FALSE) {
  if (!is.numeric(num_shown) || num_shown < 1) {
    stop("num_shown must be a positive number")
  }
  country_summary <- country_summary[order(country_summary$mean), ]
  num_countries <- nrow(country_summary)
  country_summary$rank <- seq_len(num_countries)

  is_shown <- country_summary$rank <= num_shown |
    country_summary$rank > num_countries - num_shown |
    country_summary$group %in% highlight_country
  shown <- country_summary[is_shown, ]

  # y position: one step per country, one extra step where ranks are skipped
  rank_jumps <- c(0, diff(shown$rank) > 1)
  y_positions <- seq_len(nrow(shown)) + cumsum(rank_jumps)
  is_highlight <- shown$group %in% highlight_country
  point_colours <- ifelse(is_highlight, colour_other, colour_main)
  bar_colours <- ifelse(is_highlight, colour_other_dark, colour_main_dark)

  if (nrow(shown) == num_countries) {
    plot_title <- sprintf("All %d countries, from the least to the most sleep",
                          num_countries)
  } else {
    plot_title <- sprintf("The %d lowest and %d highest of %d countries",
                          num_shown, num_shown, num_countries)
    if (!is.null(highlight_country)) {
      plot_title <- paste0(plot_title, ", and the ", highlight_country)
    }
  }

  x_label <- "Average reported sleep per night (hours)"
  if (for_report) {
    plot_title <- ""
    x_label <- "Reported sleep (hours)"
  }

  open_plot_file(file_name, 1600, plot_height)
  par(mar = c(4.5, 9.5, ifelse(for_report, 5, 3), 1))
  plot(shown$mean, y_positions, type = "n", yaxt = "n", ylab = "",
       xlim = range(c(shown$mean - shown$uncertainty,
                      shown$mean + shown$uncertainty)) + c(-0.02, 0.02),
       xlab = x_label, main = plot_title)
  if (for_report) {
    # Title on two lines, right-aligned so that it fits above the narrow plot
    mtext(sprintf("The %d countries with the lowest and", num_shown),
          side = 3, line = 3.4, adj = 1, col = "black")
    mtext(sprintf("the %d with the highest average", num_shown),
          side = 3, line = 2.2, adj = 1, col = "black")
    mtext(ifelse(is.null(highlight_country), "reported sleep",
                 paste("reported sleep, and the", highlight_country)),
          side = 3, line = 1, adj = 1, col = "black")
  }
  abline(h = y_positions, col = colour_grid)
  abline(v = overall_mean, lty = 2, col = colour_text)
  points(shown$mean, y_positions, pch = 19, col = point_colours, cex = 1.3)
  # Error bars are drawn on top, so that small ones stay visible
  draw_error_bars(shown$mean - shown$uncertainty, y_positions,
                  shown$mean + shown$uncertainty, y_positions, bar_colours)
  axis(2, at = y_positions, labels = shown$group, tick = FALSE, cex.axis = 0.85)
  if (!for_report) {
    # The highlighted country only gets a legend entry when there is one
    has_highlight <- c(TRUE, !is.null(highlight_country), TRUE, TRUE)
    draw_legend("bottomright",
                legend = c("Mean of one country",
                           paste("Mean of the", highlight_country),
                           uncertainty_label,
                           "Mean of all people")[has_highlight],
                col = c(colour_main, colour_other, colour_main_dark,
                        colour_text)[has_highlight],
                pch = c(19, 19, NA, NA)[has_highlight],
                lty = c(NA, NA, NA, 2)[has_highlight],
                lwd = c(NA, NA, NA, 1)[has_highlight], pt.cex = 1.3,
                error_bar_entry = sum(has_highlight[1:3]),
                error_bar_horizontal = TRUE)
  }
  invisible(dev.off())
}

# Average sleep at every age, separately for women and men
plot_sleep_by_age <- function(sleep_data, file_name, for_report = FALSE) {
  gender_levels <- levels(sleep_data$gender)
  gender_colours <- c(colour_main, colour_other)
  gender_bar_colours <- c(colour_main_dark, colour_other_dark)
  age_summaries <- list()
  for (gender_index in seq_along(gender_levels)) {
    is_gender <- sleep_data$gender == gender_levels[gender_index]
    age_summary <- summarise_by_group(sleep_data$sleep[is_gender],
                                      sleep_data$age[is_gender])
    age_summary$age <- as.numeric(age_summary$group)
    age_summaries[[gender_index]] <- age_summary[order(age_summary$age), ]
  }
  all_ages <- do.call(rbind, age_summaries)

  plot_title <- "Average reported sleep by age, for women and men"
  y_label <- "Average reported sleep per night (hours)"
  legend_labels <- c(paste("Mean at each age,", tolower(gender_levels)),
                     uncertainty_label)
  if (for_report) {
    plot_title <- "Average reported sleep by age, for women and men"
    y_label <- "Reported sleep per night (hours)"
    legend_labels <- c(gender_levels, "Uncertainty")
  }

  open_plot_file(file_name, 1600, ifelse(for_report, 1100, 1100))
  par(mar = c(4.5, 5, ifelse(for_report, 2.5, 3), 1))
  plot(all_ages$age, all_ages$mean, type = "n",
       ylim = range(c(all_ages$mean - all_ages$uncertainty,
                      all_ages$mean + all_ages$uncertainty)),
       xlab = "Age (years)", ylab = y_label, main = plot_title)
  grid(nx = NA, ny = NULL, col = colour_grid, lty = 1)
  for (gender_index in seq_along(gender_levels)) {
    age_summary <- age_summaries[[gender_index]]
    lines(age_summary$age, age_summary$mean, lwd = 2,
          col = gender_colours[gender_index])
    points(age_summary$age, age_summary$mean, pch = 19, cex = 0.7,
           col = gender_colours[gender_index])
    draw_error_bars(age_summary$age, age_summary$mean - age_summary$uncertainty,
                    age_summary$age, age_summary$mean + age_summary$uncertainty,
                    gender_bar_colours[gender_index])
  }
  draw_legend("topright",
              legend = legend_labels,
              col = c(gender_colours, colour_main_dark),
              pch = c(19, 19, NA), lty = c(1, 1, NA), lwd = c(2, 2, NA),
              error_bar_entry = 3)
  invisible(dev.off())
}

# Average sleep for the three commute lengths
plot_sleep_by_commute <- function(commute_summary, file_name,
                                  for_report = FALSE) {
  x_positions <- seq_len(nrow(commute_summary))
  value_range <- range(c(commute_summary$mean - commute_summary$uncertainty,
                         commute_summary$mean + commute_summary$uncertainty))
  padding <- 0.25 * diff(value_range)
  plot_title <- "Average reported sleep by length of commute"
  y_label <- "Average reported sleep per night (hours)"
  if (for_report) {
    plot_title <- "Average reported sleep by length of daily commute"
    y_label <- "Reported sleep (hours)"
  }

  open_plot_file(file_name, 1600, ifelse(for_report, 850, 1100))
  par(mar = c(4.5, 5, ifelse(for_report, 2.5, 3), 1))
  plot(x_positions, commute_summary$mean, type = "n", xaxt = "n",
       xlim = c(0.5, nrow(commute_summary) + 0.5),
       ylim = c(value_range[1] - padding, value_range[2] + 1.6 * padding),
       xlab = "Daily commute", ylab = y_label, main = plot_title)
  grid(nx = NA, ny = NULL, col = colour_grid, lty = 1)
  points(x_positions, commute_summary$mean, pch = 19, cex = 1.6,
         col = colour_main)
  draw_error_bars(x_positions, commute_summary$mean - commute_summary$uncertainty,
                  x_positions, commute_summary$mean + commute_summary$uncertainty,
                  colour_main_dark)
  text(x_positions, commute_summary$mean,
       sprintf("%.3f \u00b1 %.3f h", commute_summary$mean,
               commute_summary$uncertainty),
       pos = 3, offset = 0.9, col = colour_text, cex = 0.85)
  axis(1, at = x_positions, labels = commute_summary$group)
  if (!for_report) {
    draw_legend("topright",
                legend = c("Mean of one commute group",
                           paste0(uncertainty_label,
                                  ", smaller than the dots")),
                col = c(colour_main, colour_main_dark),
                pch = c(19, NA), pt.cex = 1.6, error_bar_entry = 2)
  }
  invisible(dev.off())
}

# Average sleep for every age group. The highest and lowest groups are
# marked, because the age difference is calculated between those two.
plot_sleep_by_age_group <- function(age_summary, file_name) {
  x_positions <- seq_len(nrow(age_summary))
  highest <- which.max(age_summary$mean)
  lowest <- which.min(age_summary$mean)
  is_marked <- x_positions %in% c(highest, lowest)
  point_colours <- ifelse(is_marked, colour_other, colour_main)
  bar_colours <- ifelse(is_marked, colour_other_dark, colour_main_dark)
  difference <- age_summary$mean[highest] - age_summary$mean[lowest]
  difference_uncertainty <- sqrt(age_summary$uncertainty[highest]^2 +
                                   age_summary$uncertainty[lowest]^2)
  value_range <- range(age_summary$mean)
  padding <- 0.2 * diff(value_range)

  open_plot_file(file_name, 1600, 1100)
  par(mar = c(4.5, 5, 3, 1))
  plot(x_positions, age_summary$mean, type = "n", xaxt = "n",
       xlim = c(0.5, nrow(age_summary) + 2.2),
       ylim = c(value_range[1] - 2.5 * padding, value_range[2] + padding),
       xlab = "Age group (years)",
       ylab = "Average reported sleep per night (hours)",
       main = "Average reported sleep in every age group")
  grid(nx = NA, ny = NULL, col = colour_grid, lty = 1)
  lines(x_positions, age_summary$mean, col = colour_grid, lwd = 2)
  # The difference between the highest and the lowest group
  arrow_x <- nrow(age_summary) + 0.7
  segments(c(highest, lowest), age_summary$mean[c(highest, lowest)],
           arrow_x, age_summary$mean[c(highest, lowest)],
           lty = 2, col = colour_other)
  arrows(arrow_x, age_summary$mean[lowest], arrow_x, age_summary$mean[highest],
         code = 3, length = 0.07, col = colour_other, lwd = 1.5)
  text(arrow_x, mean(age_summary$mean[c(highest, lowest)]),
       sprintf("difference\n%.1f \u00b1 %.1f min", 60 * difference,
               60 * difference_uncertainty),
       pos = 4, col = colour_text, cex = 0.85)
  points(x_positions, age_summary$mean, pch = 19, cex = 1.6,
         col = point_colours)
  draw_error_bars(x_positions, age_summary$mean - age_summary$uncertainty,
                  x_positions, age_summary$mean + age_summary$uncertainty,
                  bar_colours)
  text(x_positions, age_summary$mean, sprintf("%.2f h", age_summary$mean),
       pos = 1, offset = 0.8, col = colour_text, cex = 0.85)
  axis(1, at = x_positions, labels = age_summary$group)
  draw_legend("bottomleft",
              legend = c("Mean of one age group",
                         "Highest and lowest age group", uncertainty_label),
              col = c(colour_main, colour_other, colour_main_dark),
              pch = c(19, 19, NA), pt.cex = 1.6, error_bar_entry = 3)
  invisible(dev.off())
}

# The three group differences inside each of the largest countries.
# comparisons is a list of tables made by compare_within_countries().
# In every panel the countries are sorted from the smallest to the largest
# difference, so each panel has its own order of countries.
plot_differences_within_countries <- function(comparisons, panel_titles,
                                              country_summary, num_shown,
                                              highlight_country, file_name) {
  if (length(comparisons) != length(panel_titles)) {
    stop("need one title for every comparison")
  }
  largest <- country_summary[order(-country_summary$num_people), ]
  shown_countries <- head(largest$group, num_shown)
  y_positions <- seq_len(num_shown)

  open_plot_file(file_name, 2800, 1500)
  par(mfrow = c(1, length(comparisons)), oma = c(0, 0, 3.5, 0),
      mar = c(4.5, 8.5, 3, 1))
  for (panel_index in seq_along(comparisons)) {
    comparison <- comparisons[[panel_index]]
    median_difference <- median(comparison$difference_minutes)
    shown <- comparison[comparison$country %in% shown_countries, ]
    shown <- shown[order(shown$difference_minutes), ]
    is_highlight <- shown$country == highlight_country
    point_colours <- ifelse(is_highlight, colour_other, colour_main)
    bar_colours <- ifelse(is_highlight, colour_other_dark, colour_main_dark)

    plot(shown$difference_minutes, y_positions, type = "n", yaxt = "n",
         ylab = "",
         xlim = range(c(0, shown$difference_minutes - shown$uncertainty_minutes,
                        shown$difference_minutes + shown$uncertainty_minutes)),
         xlab = "Difference in reported sleep (minutes)",
         main = panel_titles[panel_index])
    abline(h = y_positions, col = colour_grid)
    abline(v = 0, lty = 2, col = colour_text)
    abline(v = median_difference, lty = 3, lwd = 2, col = colour_other)
    points(shown$difference_minutes, y_positions, pch = 19, cex = 1.4,
           col = point_colours)
    draw_error_bars(shown$difference_minutes - shown$uncertainty_minutes,
                    y_positions,
                    shown$difference_minutes + shown$uncertainty_minutes,
                    y_positions, bar_colours)
    axis(2, at = y_positions, labels = shown$country, tick = FALSE)
  }
  mtext(sprintf("Differences inside each of the %d countries with the most people", num_shown),
        outer = TRUE, line = 2, col = "black")
  mtext(sprintf("Dots: difference in one country (orange: %s). Bars: %s.",
                highlight_country, tolower(uncertainty_label)),
        outer = TRUE, line = 0.8, cex = 0.7, col = colour_text)
  mtext("Dashed line: no difference. Dotted orange line: median of all 63 countries.",
        outer = TRUE, line = -0.3, cex = 0.7, col = colour_text)
  invisible(dev.off())
}

# Histograms of the three group differences over all countries: how many
# countries have a difference of each size. Bins start at multiples of the
# bin width, so that zero is always the edge of a bin.
plot_difference_histograms <- function(comparisons, panel_titles, file_name) {
  if (length(comparisons) != length(panel_titles)) {
    stop("need one title for every comparison")
  }
  open_plot_file(file_name, 2800, 1100)
  par(mfrow = c(1, length(comparisons)), oma = c(0, 0, 3.5, 0),
      mar = c(4.5, 4.5, 3, 1))
  for (panel_index in seq_along(comparisons)) {
    differences <- comparisons[[panel_index]]$difference_minutes
    num_countries <- length(differences)
    median_difference <- median(differences)
    bin_width <- diff(pretty(range(differences), n = 12))[1]
    bin_edges <- seq(floor(min(differences) / bin_width) * bin_width,
                     ceiling(max(differences) / bin_width) * bin_width,
                     by = bin_width)
    difference_histogram <- hist(differences, breaks = bin_edges, plot = FALSE)

    plot(difference_histogram, col = colour_main, border = "white",
         ylim = c(0, 1.25 * max(difference_histogram$counts)),
         xlab = "Difference in reported sleep (minutes)",
         ylab = "Number of countries", main = panel_titles[panel_index])
    abline(v = 0, lty = 2, col = colour_text)
    abline(v = median_difference, lty = 3, lwd = 2, col = colour_other)
    draw_legend("topright",
                legend = c(sprintf("Above zero: %d of %d countries",
                                   sum(differences > 0), num_countries),
                           sprintf("Median: %.1f minutes", median_difference),
                           "No difference"),
                col = c(colour_main, colour_other, colour_text),
                pch = c(15, NA, NA), lty = c(NA, 3, 2), lwd = c(NA, 2, 1),
                pt.cex = 1.5)
  }
  mtext("Differences inside each country, for all countries", outer = TRUE,
        line = 2, col = "black")
  mtext("Each country is counted once, in the bar that contains its difference.",
        outer = TRUE, line = 0.6, cex = 0.7, col = colour_text)
  invisible(dev.off())
}

# Run the analysis

sleep_data <- load_sleep_data(data_file_path, data_url)
sleep_data <- clean_sleep_data(sleep_data, country_names)
print_overview(sleep_data)
dir.create(output_folder, showWarnings = FALSE)

overall_mean <- mean(sleep_data$sleep)
overall_uncertainty <- sd(sleep_data$sleep) / sqrt(nrow(sleep_data))
cat(sprintf("\nAverage sleep of all people: %.3f +- %.3f hours\n",
            overall_mean, overall_uncertainty))

# Question 1: in which countries do people report the least sleep?
country_summary <- summarise_by_group(sleep_data$sleep, sleep_data$country_name)
country_summary <- country_summary[order(country_summary$mean), ]
num_countries <- nrow(country_summary)
print_summary(head(country_summary, num_countries_shown),
              "Countries with the least sleep")
print_summary(tail(country_summary, num_countries_shown),
              "Countries with the most sleep")
cat(sprintf("\n%s is number %d of %d countries, counted from the least sleep\n",
            home_country, which(country_summary$group == home_country),
            num_countries))
cat("Difference between the highest and lowest country:\n  ")
print_difference(country_summary, country_summary$group[num_countries],
                 country_summary$group[1])
# Cross-check: the uncertainty from the formula against sub-sampling
cat("\nUncertainty of the mean, formula against", num_subsamples,
    "sub-samples\n")
print_subsampling_check(sleep_data$sleep, "All people", num_subsamples)
for (check_country in c(country_summary$group[1],
                        country_summary$group[num_countries], home_country)) {
  is_check_country <- sleep_data$country_name == check_country
  print_subsampling_check(sleep_data$sleep[is_check_country], check_country,
                          num_subsamples)
}

# The same cross-check for every country, saved in the country table
country_summary$subsample_uncertainty <- numeric(num_countries)
for (country_index in seq_len(num_countries)) {
  is_country <- sleep_data$country_name == country_summary$group[country_index]
  country_summary$subsample_uncertainty[country_index] <-
    subsample_uncertainty(sleep_data$sleep[is_country], num_subsamples)
}
uncertainty_ratios <- country_summary$subsample_uncertainty /
  country_summary$uncertainty
cat(sprintf("  All %d countries: sub-sampling / formula is %.2f on average (from %.2f to %.2f)\n",
            num_countries, mean(uncertainty_ratios), min(uncertainty_ratios),
            max(uncertainty_ratios)))

# Is the lowest country really the lowest? Compare it with the next ones
cat("\nLowest country against the next lowest countries:\n")
for (country_index in 2:6) {
  cat("  ")
  print_difference(country_summary, country_summary$group[country_index],
                   country_summary$group[1])
}
cat("Highest country against the second highest:\n  ")
print_difference(country_summary, country_summary$group[num_countries],
                 country_summary$group[num_countries - 1])

# How far apart are the countries in the middle, which are not on the plot?
middle_means <- country_summary$mean[(num_countries_shown + 1):
                                       (num_countries - num_countries_shown)]
cat(sprintf("\nThe %d countries in the middle of the ranking span %.1f minutes (%.2f to %.2f hours)\n",
            length(middle_means), 60 * diff(range(middle_means)),
            min(middle_means), max(middle_means)))
write.csv(country_summary, file.path(output_folder, "sleep_by_country.csv"),
          row.names = FALSE)

# Question 2: which groups report the least sleep?
gender_summary <- summarise_by_group(sleep_data$sleep, sleep_data$gender)
print_summary(gender_summary, "Sleep by gender")
cat("  ")
print_difference(gender_summary, "Women", "Men")
gender_within <- compare_within_countries(sleep_data, "gender", "Women", "Men")
print_within_countries(gender_within, "Women - Men", home_country)

age_summary <- summarise_by_group(sleep_data$sleep, sleep_data$age_group)
print_summary(age_summary, "Sleep by age group")
lowest_age_group <- age_summary$group[which.min(age_summary$mean)]
cat("  ")
print_difference(age_summary, age_summary$group[1], lowest_age_group)
age_within <- compare_within_countries(sleep_data, "age_group",
                                       age_summary$group[1], lowest_age_group)
print_within_countries(age_within, "youngest - lowest age group", home_country)
# Mean sleep of all people (women and men together) at single ages
age_means <- tapply(sleep_data$sleep, sleep_data$age, mean)
cat(sprintf("  Mean sleep at age 19: %.2f, at age 50: %.2f, at age 70: %.2f hours\n",
            age_means["19"], age_means["50"], age_means["70"]))
cat("  Age with the lowest mean sleep:", names(which.min(age_means)), "\n") 

has_commute <- !is.na(sleep_data$commute)
commute_summary <- summarise_by_group(sleep_data$sleep[has_commute],
                                      sleep_data$commute[has_commute])
commute_summary <- commute_summary[match(levels(sleep_data$commute),
                                         commute_summary$group), ]
print_summary(commute_summary, "Sleep by length of commute")
cat("  ")
print_difference(commute_summary, "< 30 min", "> 60 min")
commute_within <- compare_within_countries(sleep_data, "commute", "< 30 min",
                                           "> 60 min")
print_within_countries(commute_within, "< 30 min - > 60 min", home_country)

# Sub-sampling check of the formula for the uncertainty of a difference, for
# every comparison in the results: the highest country against each country
# that is tied with the lowest one (less than 3 sigma away), and the gender,
# age and commute differences
lowest_country <- country_summary[1, ]
sigma_from_lowest <- (country_summary$mean - lowest_country$mean) /
  sqrt(country_summary$uncertainty^2 + lowest_country$uncertainty^2)
lowest_countries <- country_summary$group[sigma_from_lowest < 3]
cat("\nCountries less than 3 sigma from the lowest country:",
    paste(lowest_countries, collapse = ", "), "\n")

highest_country <- country_summary$group[num_countries]
# Every comparison as: column with the groups, group a, group b
compared_groups <- c(
  lapply(lowest_countries, function(country) {
    list("country_name", highest_country, country)
  }),
  list(list("gender", "Women", "Men"),
       list("age_group", age_summary$group[1], lowest_age_group),
       list("commute", "< 30 min", "> 60 min")))

num_check_repeats <- 50
difference_checks <- do.call(rbind, lapply(compared_groups, function(groups) {
  check_difference_by_subsampling(sleep_data$sleep, sleep_data[[groups[[1]]]],
                                  groups[[2]], groups[[3]], num_subsamples,
                                  num_check_repeats)
}))
cat("\nUncertainty of the differences: formula against", num_subsamples,
    "sub-samples (averaged over", num_check_repeats, "random splits)\n")
print(difference_checks, row.names = FALSE)

# Effect size of the same differences, saved in the same table
difference_checks$hedges_g <- round(sapply(compared_groups, function(groups) {
  hedges_g(sleep_data$sleep, sleep_data[[groups[[1]]]], groups[[2]],
           groups[[3]])
}), 2)
cat("\nEffect size (Hedges' g: 0.2 small, 0.5 medium, 0.8 large)\n")
print(difference_checks[, c("comparison", "difference_minutes", "hedges_g")],
      row.names = FALSE)
write.csv(difference_checks,
          file.path(output_folder, "difference_subsampling_check.csv"),
          row.names = FALSE)

plot_differences_within_countries(
  list(gender_within, age_within, commute_within),
  c("Women - men",
    sprintf("Ages %s - ages %s", age_summary$group[1], lowest_age_group),
    "Commute < 30 min - > 60 min"),
  country_summary, 15, home_country, "fig6_differences_within_countries.png"
)
plot_difference_histograms(
  list(gender_within, age_within, commute_within),
  c("Women - men",
    sprintf("Ages %s - ages %s", age_summary$group[1], lowest_age_group),
    "Commute < 30 min - > 60 min"),
  "fig7_difference_histograms.png"
)

# One table with the three differences inside every country
names(gender_within)[-1] <- paste0("gender_", names(gender_within)[-1])
names(age_within)[-1] <- paste0("age_", names(age_within)[-1])
names(commute_within)[-1] <- paste0("commute_", names(commute_within)[-1])
within_table <- merge(merge(gender_within, age_within), commute_within)
write.csv(within_table, file.path(output_folder, "differences_by_country.csv"),
          row.names = FALSE)

plot_sleep_distribution(sleep_data, "fig1_sleep_distribution.png")
# The plots of the lowest and highest countries show only those countries;
# the home country is still marked on the plot of all countries
plot_sleep_by_country(country_summary, overall_mean, num_countries_shown,
                      NULL, "fig2_sleep_by_country.png")
plot_sleep_by_country(country_summary, overall_mean, num_countries,
                      home_country, "fig8_sleep_all_countries.png",
                      plot_height = 3800)
plot_sleep_by_age(sleep_data, "fig3_sleep_by_age.png")
plot_sleep_by_commute(commute_summary, "fig4_sleep_by_commute.png")
plot_sleep_by_age_group(age_summary, "fig5_sleep_by_age_group.png")
# The three figures in the report are drawn in Python (report_figures.py).
# These tables hold everything those figures show: the means and their
# uncertainties, calculated above.
report_country_data <- country_summary[, c("group", "num_people", "mean",
                                           "uncertainty")]
report_country_data$overall_mean <- overall_mean
write.csv(report_country_data,
          file.path(output_folder, "report_data_countries.csv"),
          row.names = FALSE)

report_age_data <- do.call(rbind, lapply(levels(sleep_data$gender),
                                         function(gender) {
  is_gender <- sleep_data$gender == gender
  age_summary <- summarise_by_group(sleep_data$sleep[is_gender],
                                    sleep_data$age[is_gender])
  data.frame(gender = gender, age = as.numeric(age_summary$group),
             mean = age_summary$mean, uncertainty = age_summary$uncertainty)
}))
write.csv(report_age_data[order(report_age_data$gender, report_age_data$age), ],
          file.path(output_folder, "report_data_age_sex.csv"),
          row.names = FALSE)

write.csv(commute_summary[, c("group", "num_people", "mean", "uncertainty")],
          file.path(output_folder, "report_data_commute.csv"),
          row.names = FALSE)

cat("\nPlots and tables are saved in the folder:", output_folder, "\n")
cat("Next, draw the report figures with: python3 report_figures.py\n")
