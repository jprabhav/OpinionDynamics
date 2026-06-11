library(ggplot2)
library(tidyr)

# Parameters

set.seed(42)

n_agents    <- 10
n_steps     <- 10
n_neighbors <- 5

# Initialise agents

opinions <- sample(c(-1, 0, 1), n_agents, replace = TRUE)

# Simulating Shuffling

history <- matrix(nrow = n_steps, ncol = n_agents)

for (t in seq_len(n_steps)) {
  history[t, ] <- opinions
  for (i in seq_len(n_agents)) {
    neighbors   <- sample(seq_len(n_agents), size = n_neighbors, replace = FALSE)
    net_opinion <- sum(opinions[neighbors])
    opinions[i] <- as.integer(net_opinion > 0) - as.integer(net_opinion < 0)
  }
}

# Plotting

plot_df <- data.frame(
  time_step = seq_len(n_steps),
  Pro       = rowSums(history ==  1),
  Anti      = rowSums(history == -1),
  Undecided = rowSums(history ==  0)
)

plot_long <- pivot_longer(plot_df, -time_step, names_to = "opinion", values_to = "count")

print(
  ggplot(plot_long, aes(x = time_step, y = count, colour = opinion)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = c("Pro" = "#2ecc71", "Anti" = "#e74c3c", "Undecided" = "gray50")) +
    labs( title  = "Galam Opinion Dynamics", x = "Time Step", y = "Number of Agents", colour = NULL) +
    theme_minimal(base_size = 13)
)