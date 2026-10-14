# Average reported sleep in 63 countries

MAT2007 Introduction to Programming, final project by Julia Waldmeier.

## Research question

**In which countries do people report the least sleep, and which groups
report the least (by sex, age and length of commute)?**

## Dataset

- **Source:** A. Coutrot et al., "Reported sleep duration reveals segmentation
  of the adult life-course into three phases", *Nature Communications* 13,
  7697 (2022), <https://doi.org/10.1038/s41467-022-34624-8>.
- **Download:** the authors' Open Science Framework repository linked in the
  paper, <https://osf.io/j7sd4/?view_only=92c1aa56abfc4d0f99501c9f260fa324>.
- **File used:** `clean_demog_data.csv` (29 MB). It is not included in this
  repository because it is larger than GitHub allows; `sleep_analysis.R`
  downloads it automatically to `data/clean_demog_data.csv` the first time it
  runs.
- **Contents:** 730,187 people aged 19 to 70 in 63 countries who played the
  mobile game Sea Hero Quest between May 2016 and July 2017 and answered "How
  much sleep do you get on average each night?". One row per person with the
  columns `age`, `gender`, `country`, `sleep` (whole hours, 5 to 10),
  `commute`, `education` and `homeenv`.

### Does the data need cleaning?

No manual cleaning is needed. The file was already cleaned by the authors,
and the script does the rest:

- rows without sleep, age, gender or country are removed (there are none);
- the 239 people without a commute answer are left out of the commute
  comparison and the regression only;
- 2-letter country codes are replaced by country names;
- sex and commute get readable labels, and age is grouped into four bands of
  13 years (19–31, 32–44, 45–57, 58–70).

## Requirements

- R (written and tested with R 4.6.0), with no extra packages: the analysis
  uses base R only.
- Python 3 (written and tested with Python 3.12) with numpy, pandas and
  matplotlib, for the three figures of the report. Install them with:

```bash
pip install numpy pandas matplotlib
```

## How to run

Open a terminal in this folder and run the two steps in this order:

```bash
Rscript sleep_analysis.R
python3 report_figures.py
```

`sleep_analysis.R` does all calculations and prints the results.
`report_figures.py` then reads the tables that R saved and draws the three
figures of the report. The working directory must be this folder. If
`data/clean_demog_data.csv` is missing, the R script downloads it, which needs
an internet connection. Both steps take a few seconds.

The multiple regression is a separate R script:

```bash
Rscript regression_analysis.R
```

## What the scripts do

`sleep_analysis.R`:

1. Loads and cleans the data and prints an overview.
2. Calculates the average sleep and its statistical uncertainty (standard
   deviation / sqrt(N)) per country, sex, age group and commute length.
3. Within each variable, compares the group with the highest average to the
   group with the lowest: the difference, its uncertainty (the two
   uncertainties added in quadrature) and its significance (difference /
   uncertainty, in σ, significant from 3σ).
4. Compares the lowest country with every other country, to find the
   countries that cannot be separated from it (less than 3σ away).
5. Checks both uncertainty formulas with the sub-sampling technique (10 random
   sub-samples), and checks that the two groups of each comparison are
   uncorrelated.
6. Calculates the effect size of every difference (Hedges' g).
7. Repeats the sex, age and commute comparisons inside each of the 63
   countries separately.
8. Saves extra plots and tables in the `output` folder, including the three
   tables used for the report figures.

`regression_analysis.R` fits a multiple linear regression with country, sex,
age group and commute together, so that every difference is measured with
the other variables kept fixed, and plots the differences next to those of
`sleep_analysis.R`.

`report_figures.py` draws the three figures of the report from the tables
saved by `sleep_analysis.R`. It does no calculations of its own.

## Output

All output is saved in the `output` folder. It falls into three groups.

### 1. The figures in the report

These three figures are Figure 1 of the report. They are drawn in Python by
`report_figures.py`, from the tables that `sleep_analysis.R` saves.

| File | Content |
|---|---|
| `output/report_fig_sleep_by_country_py.png` | The 10 lowest and 10 highest countries |
| `output/report_fig_sleep_by_age_py.png` | Average sleep at every age, for women and men |
| `output/report_fig_sleep_by_commute_py.png` | Average sleep by length of commute |
| `output/report_data_countries.csv`, `report_data_age_sex.csv`, `report_data_commute.csv` | The averages and uncertainties shown in these three figures |

### 2. Supporting figures (not in the report)

The report is limited to two pages, so it shows only Figure 1. The R scripts
also make the figures below. They were used to explore the data and to check
the results, and they show in more detail where several numbers in the report
come from.

| File | Content | Supports in the report |
|---|---|---|
| `output/fig1_sleep_distribution.png` | How many people reported each number of hours | The data sample: answers in whole hours from 5 to 10 |
| `output/fig2_sleep_by_country.png` | The 10 lowest and 10 highest countries, with a legend | Earlier R version of the left panel of Figure 1 |
| `output/fig3_sleep_by_age.png` | Average sleep at every age, for women and men, with a legend | Earlier R version of the top right panel of Figure 1 |
| `output/fig4_sleep_by_commute.png` | Average sleep by length of commute, with a legend | Earlier R version of the bottom right panel of Figure 1 |
| `output/fig5_sleep_by_age_group.png` | Average sleep in the four age groups, with the largest difference marked | The 20.6-minute gap between ages 19–31 and 45–57 |
| `output/fig6_differences_within_countries.png` | The sex, age and commute differences inside the 15 largest countries | The check inside each country |
| `output/fig7_difference_histograms.png` | The same differences for all 63 countries | "Same direction in 58, 59 and 61 of 63 countries" |
| `output/fig8_sleep_all_countries.png` | Average sleep in all 63 countries | The ranking, the 8 countries tied with Japan and the 43 middle countries |
| `output/fig9_regression_comparison.png` | The differences one variable at a time next to those of the regression | The multiple regression (commute 14.1 minutes, countries 41 to 53 minutes) |

### 3. Result tables

These tables hold the exact numbers behind the report.

| File | Content |
|---|---|
| `output/sleep_by_country.csv` | Average sleep and uncertainty (formula and sub-sampling) for all 63 countries |
| `output/differences_by_country.csv` | The sex, age and commute differences inside each country |
| `output/difference_subsampling_check.csv` | Sub-sampling check and Hedges' g of every difference |
| `output/regression_differences.csv` | The differences from the multiple regression |

## Main results

| Comparison | Result |
|---|---|
| All people | 7.006 ± 0.001 hours (standard deviation between people 1.06 hours) |
| Highest country (Albania) | 7.54 ± 0.04 hours, 3.9σ above the second highest |
| Lowest country (Japan) | 6.63 ± 0.04 hours, tied (less than 3σ) with 8 other countries |
| Albania − the 9 lowest countries | 45 to 55 minutes (13σ or more), Hedges' g 0.61 to 0.79 |
| Middle 43 countries | within 20 ± 1 minutes of each other (6.85 to 7.18 hours) |
| Ages 19–31 − ages 45–57 | 20.6 ± 0.2 minutes (106σ), g = 0.32 |
| Commute under 30 min − over 60 min | 10.7 ± 0.2 minutes (56σ), g = 0.17 |
| Women − men | 7.9 ± 0.1 minutes (53σ), g = 0.12 |

Inside the individual countries, the three group differences have the same
direction in 59 (age), 61 (commute) and 58 (sex) of the 63 countries, with
median differences of 16.5, 13.2 and 9.1 minutes.

In the multiple regression, the differences are 41 to 53 minutes (countries),
19.7 ± 0.2 minutes (age), 14.1 ± 0.2 minutes (commute) and 7.3 ± 0.1 minutes
(sex). The four variables together explain 4.4% of the differences in
reported sleep between people.

The uncertainties are statistical only. Sleep is self-reported in whole
hours, and the people are players of a mobile game, not a random sample of
each country.

## Conclusion

Among these respondents, the least sleep is reported in Japan and eight other
countries that cannot be statistically separated from it, and the most in
Albania (45 to 55 minutes more). Men, people aged 45–57 and people commuting
more than an hour report the least sleep. All differences are highly
significant, but only the country differences are large compared with the
spread between individuals. The full discussion is in the report:
[`report/MAT2007_report_JW_i6341664-2.pdf`](report/MAT2007_report_JW_i6341664-2.pdf).
