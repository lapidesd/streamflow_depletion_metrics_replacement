## WithdrawalFraction.R
# Figure for SI showing we why used 20% of flow for pumping.

library(tidyverse)

# directory to keep stuff that is too big for GitHub
dir_big_files <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/StreamflowDepletion/DepletionMetrics_LapidesEtAl/data"

# load data
withdrawal_data <- read_csv(file.path(dir_big_files, "withdrawal_fractions.csv"))

# calculate median
median_fraction <- median(withdrawal_data$withdrawal_as_fraction_of_annual_Q)

# plot
ggplot(withdrawal_data, aes(x = withdrawal_as_fraction_of_annual_Q)) +
  geom_histogram(breaks = seq(0, 1, 0.05), fill = "black", color = "white") +
  geom_vline(xintercept = median_fraction, color = "red") +
  scale_x_continuous(name = "Withdrawals as fraction of\nmean annual discharge",
                     breaks = seq(0, 1, 0.2), expand = c(0,0)) +
  scale_y_continuous(name = "Number of gages", expand = expansion(mult = c(0, 0.05))) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(file.path("Figures", "WithdrawalFraction.png"),
       width = 90, height = 90, units = "mm")  
