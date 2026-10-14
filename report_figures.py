"""MAT2007 project: the three figures of the report.

Julia Waldmeier

All numbers are calculated in R by sleep_analysis.R, which saves the means
and their uncertainties in three tables in the output folder. This script
only reads those tables and draws the figures, so run the R script first:
    Rscript sleep_analysis.R
    python3 report_figures.py
Run both from the MAT2007-Project folder.

Needs Python 3 with numpy, pandas and matplotlib.
"""

import os

import matplotlib
matplotlib.use("Agg")   # draw straight to files, no window needed
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
import pandas as pd

print("Julia Waldmeier")

output_folder = "output"
num_countries_shown = 10   # lowest and highest countries on the plot

# Colours, the same as in the plots made by sleep_analysis.R
colour_main = "#2a78d6"        # blue
colour_other = "#eb6834"       # orange
colour_main_dark = "#12365f"   # dark blue, error bars on blue points
colour_other_dark = "#8a3312"  # dark orange, error bars on orange points
colour_grid = "#e4e3df"
colour_text = "#52514e"

plt.rcParams.update({"font.family": "Arial", "font.size": 12,
                     "axes.edgecolor": colour_text,
                     "axes.labelcolor": colour_text,
                     "xtick.color": colour_text, "ytick.color": colour_text})


def load_table(file_name, expected_columns):
    file_path = os.path.join(output_folder, file_name)
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"cannot find {file_path}; run "
                                "Rscript sleep_analysis.R first")
    table = pd.read_csv(file_path)
    if not all(column in table.columns for column in expected_columns):
        raise ValueError(f"{file_name} does not have the expected columns")
    return table


# The sizes and resolutions are fixed so that the figures fill the same space
# in the report. Error bars are drawn on top of the dots (barsabove), so that
# the ones smaller than a dot stay visible.

def save_figure(figure, file_name):
    figure.savefig(os.path.join(output_folder, file_name))
    plt.close(figure)


def style_axes(axes):
    axes.spines[["top", "right"]].set_visible(False)
    axes.grid(axis="y", color=colour_grid, linewidth=1)
    axes.set_axisbelow(True)


# The lowest and highest countries, with a gap between the two groups
def plot_sleep_by_country(country_data, num_shown, file_name):
    if num_shown < 1:
        raise ValueError("num_shown must be a positive number")
    country_data = country_data.sort_values("mean")
    shown = pd.concat([country_data.head(num_shown),
                       country_data.tail(num_shown)])
    # one step per country, one extra step for the countries not shown
    y_positions = np.arange(len(shown)) + (np.arange(len(shown)) >= num_shown)

    figure = plt.figure(figsize=(1600 / 380, 2300 / 380), dpi=380)
    axes = figure.add_axes([0.40, 0.09, 0.58, 0.77])
    axes.hlines(y_positions, 0, 1, transform=axes.get_yaxis_transform(),
                color=colour_grid, linewidth=0.8, zorder=0)
    axes.axvline(country_data["overall_mean"].iloc[0], color=colour_text,
                 linestyle="--", linewidth=0.8)
    axes.errorbar(shown["mean"], y_positions, xerr=shown["uncertainty"],
                  fmt="o", color=colour_main, markersize=4,
                  ecolor=colour_main_dark, elinewidth=1, capsize=1.5, barsabove=True)
    axes.set_yticks(y_positions, shown["group"], fontsize=6)
    axes.tick_params(axis="y", length=0)
    axes.tick_params(axis="x", labelsize=6.5)
    axes.set_ylim(-0.8, y_positions[-1] + 0.8)
    axes.set_xlabel("Reported sleep (hours)", fontsize=7)
    axes.spines[["top", "right"]].set_visible(False)
    figure.text(0.98, 0.975,
                f"The {num_shown} countries with the lowest and\n"
                f"the {num_shown} with the highest average\n"
                "reported sleep", ha="right", va="top", fontsize=7.5,
                linespacing=1.5)
    save_figure(figure, file_name)


# Average sleep at every age, separately for women and men
def plot_sleep_by_age(age_data, file_name):
    figure, axes = plt.subplots(figsize=(1600 / 300, 1100 / 300), dpi=300)
    legend_handles = []
    for gender, colour, bar_colour in (("Women", colour_main, colour_main_dark),
                                       ("Men", colour_other,
                                        colour_other_dark)):
        gender_data = age_data[age_data["gender"] == gender].sort_values("age")
        if len(gender_data) == 0:
            raise ValueError(f"no rows for {gender} in the age table")
        axes.errorbar(gender_data["age"], gender_data["mean"],
                      yerr=gender_data["uncertainty"], fmt="-o",
                      color=colour, linewidth=1.5, markersize=2.5,
                      ecolor=bar_colour, elinewidth=0.8, capsize=1.2, barsabove=True)
        legend_handles.append(Line2D([], [], color=colour, marker="o",
                                     markersize=3, linewidth=1.5,
                                     label=gender))
    legend_handles.append(Line2D([], [], color=colour_main_dark, marker="|",
                                 markersize=6, linestyle="none",
                                 label="Uncertainty"))
    style_axes(axes)
    axes.set_title("Average reported sleep by age, for women and men",
                   fontsize=8)
    axes.set_xlabel("Age (years)", fontsize=7)
    axes.set_ylabel("Reported sleep per night (hours)", fontsize=7)
    axes.tick_params(labelsize=6.5)
    axes.legend(handles=legend_handles, loc="upper right", fontsize=6,
                edgecolor=colour_grid, labelcolor=colour_text)
    figure.tight_layout()
    save_figure(figure, file_name)


# Average sleep for the three commute lengths, in the order of the table
def plot_sleep_by_commute(commute_data, file_name):
    x_positions = np.arange(1, len(commute_data) + 1)
    figure, axes = plt.subplots(figsize=(1600 / 300, 850 / 300), dpi=300)
    axes.errorbar(x_positions, commute_data["mean"],
                  yerr=commute_data["uncertainty"], fmt="o",
                  color=colour_main, markersize=5, ecolor=colour_main_dark,
                  elinewidth=1, capsize=2, barsabove=True)
    for x_position, mean, uncertainty in zip(x_positions, commute_data["mean"],
                                             commute_data["uncertainty"]):
        axes.annotate(f"{mean:.3f} ± {uncertainty:.3f} h",
                      (x_position, mean), textcoords="offset points",
                      xytext=(0, 7), ha="center", fontsize=5.5,
                      color=colour_text)
    lowest, highest = commute_data["mean"].min(), commute_data["mean"].max()
    padding = 0.25 * (highest - lowest)
    axes.set_ylim(lowest - padding, highest + 1.6 * padding)
    axes.set_xlim(0.5, len(commute_data) + 0.5)
    axes.set_xticks(x_positions, commute_data["group"])
    style_axes(axes)
    axes.set_title("Average reported sleep by length of daily commute",
                   fontsize=8)
    axes.set_xlabel("Daily commute", fontsize=7)
    axes.set_ylabel("Reported sleep (hours)", fontsize=7)
    axes.tick_params(labelsize=6.5)
    figure.tight_layout()
    save_figure(figure, file_name)


country_data = load_table("report_data_countries.csv",
                          ["group", "mean", "uncertainty", "overall_mean"])
age_data = load_table("report_data_age_sex.csv",
                      ["gender", "age", "mean", "uncertainty"])
commute_data = load_table("report_data_commute.csv",
                          ["group", "mean", "uncertainty"])

plot_sleep_by_country(country_data, num_countries_shown,
                      "report_fig_sleep_by_country_py.png")
plot_sleep_by_age(age_data, "report_fig_sleep_by_age_py.png")
plot_sleep_by_commute(commute_data, "report_fig_sleep_by_commute_py.png")
print("The three report figures are saved in the folder:", output_folder)
