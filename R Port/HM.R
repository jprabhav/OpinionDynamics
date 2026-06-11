library(igraph)
library(ggplot2)
library(tidyr)
library(patchwork)

# Parameters

set.seed(42)

n_agents            <- 10
n_steps             <- 5
influencer_strength <- 0.5

# Build Watts-Strogatz network

G <- sample_smallworld(dim = 1, size = n_agents, nei = 3, p = 0.1)

# Initialise regular agents

opinions         <- sample(c(-1, 0, 1), n_agents, replace = TRUE)
salience         <- runif(n_agents)
capability       <- runif(n_agents)
influence_scores <- salience * capability

# Influencers

influencer_opinions   <- c(1, -1)
influencer_influences <- c(1.0, 1.0)

# Simulating

history <- matrix(nrow = n_steps, ncol = n_agents)

for (t in seq_len(n_steps)) {
  history[t, ] <- opinions
  
  # Network-based opinion update (influence-weighted)
  for (i in seq_len(n_agents)) {
    nbrs <- neighbors(G, i)
    if (length(nbrs) == 0) next
    net_influence <- sum(opinions[nbrs] * influence_scores[nbrs])
    opinions[i]   <- as.integer(net_influence > 0) - as.integer(net_influence < 0)
  }
  
  # Influencer broadcast in shuffled order
  
  for (k in sample(2)) {
    inf_opinion   <- influencer_opinions[k]
    inf_influence <- influencer_influences[k]
    targets <- which(opinions != inf_opinion & inf_influence > influence_scores)
    for (i in targets) {
      if (runif(1) < influencer_strength) {
        opinions[i] <- inf_opinion
      }
    }
  }
}

#  Plotting final opinion network

layout_kk <- layout_with_kk(G)

node_df <- data.frame(
  x       = layout_kk[, 1],
  y       = layout_kk[, 2],
  opinion = factor(opinions, levels = c(1, -1, 0), labels = c("Pro", "Anti", "Undecided"))
)

edge_list <- as_edgelist(G)
edge_df <- data.frame(
  x    = layout_kk[edge_list[, 1], 1],
  y    = layout_kk[edge_list[, 1], 2],
  xend = layout_kk[edge_list[, 2], 1],
  yend = layout_kk[edge_list[, 2], 2]
)

p1 <- ggplot() +
  geom_segment(data = edge_df, aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey70", linewidth = 0.5, alpha = 0.5) +
  geom_point(data = node_df, aes(x = x, y = y, colour = opinion), size = 3) +
  scale_colour_manual(values = c("Pro" = "#2ecc71", "Anti" = "#e74c3c", "Undecided" = "gray50")) +
  labs(title = "Final Opinion Network", colour = NULL) +
  theme_void(base_size = 13) + theme(legend.position = "bottom")

# Plotting opinion evolution

plot_df <- data.frame(
  time_step = seq_len(n_steps),
  Pro       = rowSums(history ==  1),
  Anti      = rowSums(history == -1),
  Undecided = rowSums(history ==  0)
)

plot_long <- pivot_longer(plot_df, -time_step, names_to = "opinion", values_to = "count")

p2 <- ggplot(plot_long, aes(x = time_step, y = count, colour = opinion)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("Pro" = "#2ecc71", "Anti" = "#e74c3c", "Undecided" = "gray50")) +
  labs(
    title  = "Public Opinion Over Time",
    x      = "Time Step",
    y      = "Number of Agents",
    colour = NULL
  ) +
  theme_minimal(base_size = 13)

print(p1 + p2)