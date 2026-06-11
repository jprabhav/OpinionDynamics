library(ggplot2)
library(tidyr)

# Parameters

set.seed(42)

n_agents          <- 20
n_steps           <- 30
convergence_parameter <- 0.2

# Initialise agents 

positions        <- runif(n_agents, -1, 1)
salience         <- runif(n_agents)
capability       <- runif(n_agents)
influence_scores <- salience * capability

# Simulating Convergence

history <- matrix(nrow = n_steps, ncol = n_agents)

for (t in seq_len(n_steps)) {
  for (i in seq_len(n_agents)) {
    others       <- setdiff(seq_len(n_agents), i)
    total_weight <- sum(influence_scores[others])
    if (total_weight == 0) next
    weighted_avg <- sum(positions[others] * influence_scores[others]) / total_weight
    positions[i] <- positions[i] + convergence_parameter * (weighted_avg - positions[i])
  }
  history[t, ] <- positions
}

# Plot

history_df           <- as.data.frame(history)
colnames(history_df) <- paste0("Agent_", seq_len(n_agents))
history_df$time_step <- seq_len(n_steps)

history_long <- pivot_longer(history_df, -time_step, names_to  = "agent", values_to = "position")

print(
  ggplot(history_long, aes(x = time_step, y = position, colour = agent)) +
    geom_line(alpha = 0.7, linewidth = 0.9) +
    labs(title = "Convergence to Equilibrium Position", x = "Time Step", y = "Position") +
    theme_minimal(base_size = 13) + theme(legend.position = "none")
)
