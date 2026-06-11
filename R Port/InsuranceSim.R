library(igraph)
library(ggplot2)
library(tidyr)
library(patchwork)

# Parameters 
threshold_price                  <- 150
prob_below_threshold_pro         <- 0.5
prob_above_threshold_pro         <- 0.1
prob_below_threshold_undecided   <- 0.2
prob_above_threshold_undecided   <- 0.05
buy_prob_anti                    <- 0
buy_prob_neighbor_multiplier     <- 0.02
prob_renewal_pro                 <- 0.95
prob_renewal_undecided           <- 0
prob_renewal_anti                <- 0.01
renewal_prob_claim_multiplier    <- 0.1
renewal_prob_neighbor_multiplier <- 0.02

# Compute premium
calculate_premium <- function(mu, theta, p) p * mu * (1 + theta)

# Simulation 
run_simulation <- function(n_agents = 100, n_periods = 120, k = 6, p = 0.1,
                           avg_claim_size = 2000, theta = 0.05,
                           claim_probability = 0.02, expense_ratio = 0.20,
                           seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  premium_monthly <- calculate_premium(avg_claim_size, theta, claim_probability)
  annual_premium  <- premium_monthly * 12
  
  # Initialise network
  G <- sample_smallworld(dim = 1, size = n_agents, nei = 3, p = p)
  
  # Define agent states
  
  opinions         <- sample(c(-1L, 0L, 1L), n_agents, replace = TRUE)
  salience         <- runif(n_agents)
  capability       <- runif(n_agents)
  influence_scores <- salience * capability
  has_policy       <- rep(FALSE, n_agents)
  policy_start     <- rep(NA_integer_, n_agents)
  policy_end       <- rep(NA_integer_, n_agents)
  claims_count     <- rep(0L, n_agents)
  
  # Influencers
  
  influencer_opinions   <- c(1L, -1L)
  influencer_influences <- c(1.0, 1.0)
  
  # Metrics
  
  active_policies_vec <- integer(n_periods)
  new_sales_vec       <- integer(n_periods)
  renewals_vec        <- integer(n_periods)
  lapses_vec          <- integer(n_periods)
  premiums_vec        <- numeric(n_periods)
  claims_vec          <- numeric(n_periods)
  expenses_vec        <- numeric(n_periods)
  profit_vec          <- numeric(n_periods)
  opinion_dist        <- matrix(NA_integer_, nrow = n_periods, ncol = 3)
  
  for (t in seq_len(n_periods)) {
    
    # Same as the Hybrid Model
    for (i in seq_len(n_agents)) {
      nbrs <- as.integer(neighbors(G, i))
      if (length(nbrs) == 0) next
      net_influence <- sum(opinions[nbrs] * influence_scores[nbrs])
      opinions[i]   <- as.integer(net_influence > 0) - as.integer(net_influence < 0)
    }
    
    # Influencer broadcast 
    for (k_inf in sample(2)) {
      inf_opinion   <- influencer_opinions[k_inf]
      inf_influence <- influencer_influences[k_inf]
      targets <- which(opinions != inf_opinion & inf_influence > influence_scores)
      for (i in targets) {
        if (runif(1) < 0.2) opinions[i] <- inf_opinion
      }
    }
    
    # Policy decisions 
    new_sales <- 0L
    renewals  <- 0L
    lapses    <- 0L
    
    for (i in seq_len(n_agents)) {
      
      if (has_policy[i] && !is.na(policy_end[i]) && t >= policy_end[i]) {
        
        # Renewal decision
        
        renewal_prob <- if (opinions[i] == 1L) {
          prob_renewal_pro
        } else if (opinions[i] == 0L) {
          prob_renewal_undecided
        } else {
          prob_renewal_anti
        }
        # Uncomment to enable network effects on renewal:
        # nbrs <- as.integer(neighbors(G, i))
        # renewal_prob <- renewal_prob +
        #   renewal_prob_claim_multiplier * claims_count[i] +
        #   renewal_prob_neighbor_multiplier * sum(claims_count[nbrs])
        
        if (runif(1) < renewal_prob) {
          policy_start[i] <- t
          policy_end[i]   <- t + 12L
          renewals        <- renewals + 1L
        } else {
          has_policy[i] <- FALSE
          lapses        <- lapses + 1L
        }
        
      } else if (!has_policy[i]) {
        
        # Purchase decision
        
        buy_prob <- if (opinions[i] == 1L) {
          if (annual_premium < threshold_price) prob_below_threshold_pro else prob_above_threshold_pro
        } else if (opinions[i] == 0L) {
          if (annual_premium < threshold_price) prob_below_threshold_undecided else prob_above_threshold_undecided
        } else {
          buy_prob_anti
        }
        # Uncomment to enable network effects on purchase:
        # nbrs <- as.integer(neighbors(G, i))
        # buy_prob <- buy_prob +
        #   buy_prob_neighbor_multiplier * sum(claims_count[nbrs[has_policy[nbrs]]])
        
        if (runif(1) < buy_prob) {
          has_policy[i]   <- TRUE
          policy_start[i] <- t
          policy_end[i]   <- t + 12L
          new_sales       <- new_sales + 1L
        }
      }
    }
    
    # Accounting
    active_mask       <- has_policy & !is.na(policy_start) & t >= policy_start & t < policy_end
    active            <- sum(active_mask)
    premium_collected <- active * premium_monthly
    expenses          <- premium_collected * expense_ratio
    
    claims_total <- 0
    for (i in which(active_mask)) {
      if (runif(1) < claim_probability) {
        claims_total    <- claims_total + rexp(1, rate = 1 / avg_claim_size)
        claims_count[i] <- claims_count[i] + 1L
      }
    }
    
    profit <- premium_collected - claims_total - expenses
    
    active_policies_vec[t] <- active
    new_sales_vec[t]       <- new_sales
    renewals_vec[t]        <- renewals
    lapses_vec[t]          <- lapses
    premiums_vec[t]        <- premium_collected
    claims_vec[t]          <- claims_total
    expenses_vec[t]        <- expenses
    profit_vec[t]          <- profit
    opinion_dist[t, ]      <- c(sum(opinions == -1L), sum(opinions == 0L), sum(opinions == 1L))
  }
  
  list(
    agents = list(
      opinions     = opinions,
      has_policy   = has_policy,
      claims_count = claims_count
    ),
    G = G,
    metrics = list(
      active_policies = active_policies_vec,
      new_sales       = new_sales_vec,
      renewals        = renewals_vec,
      lapses          = lapses_vec,
      premiums        = premiums_vec,
      claims          = claims_vec,
      expenses        = expenses_vec,
      profit          = profit_vec,
      opinion_dist    = opinion_dist
    )
  )
}

# Network visualisation 
visualize_network <- function(G, agents) {
  layout_kk <- layout_with_kk(G)
  
  node_df <- data.frame(
    x       = layout_kk[, 1],
    y       = layout_kk[, 2],
    opinion = factor(agents$opinions,
                     levels = c(1L, -1L, 0L),
                     labels = c("Pro", "Anti", "Undecided"))
  )
  
  edge_list <- as_edgelist(G)
  edge_df   <- data.frame(
    x    = layout_kk[edge_list[, 1], 1],
    y    = layout_kk[edge_list[, 1], 2],
    xend = layout_kk[edge_list[, 2], 1],
    yend = layout_kk[edge_list[, 2], 2]
  )
  
  print(
    ggplot() +
      geom_segment(data = edge_df,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   colour = "grey70", linewidth = 0.3, alpha = 0.5) +
      geom_point(data = node_df,
                 aes(x = x, y = y, colour = opinion), size = 2.5, alpha = 0.8) +
      scale_colour_manual(
        values = c(Pro = "#2ecc71", Anti = "#e74c3c", Undecided = "gray")
      ) +
      labs(title = "Final Network State", colour = "Agent Opinion") +
      theme_void(base_size = 13) +
      theme(legend.position = "bottom")
  )
}

# Dashboard

create_visualizations <- function(metrics, n_periods, premium, claim_prob, claim_size) {
  
  # Active policies and market sentiment
  max_pol <- max(metrics$active_policies)
  max_op  <- max(metrics$opinion_dist)
  sf      <- if (max_op > 0) max_pol / max_op else 1
  
  p1_df <- data.frame(
    t         = seq_len(n_periods),
    policies  = metrics$active_policies,
    anti      = metrics$opinion_dist[, 1] * sf,
    undecided = metrics$opinion_dist[, 2] * sf,
    pro       = metrics$opinion_dist[, 3] * sf
  )
  
  p1 <- ggplot(p1_df, aes(x = t)) +
    geom_line(aes(y = policies), colour = "steelblue", linewidth = 1.2) +
    geom_line(aes(y = anti),      colour = "#e74c3c",   linewidth = 0.8, alpha = 0.5) +
    geom_line(aes(y = undecided), colour = "gray",  linewidth = 0.8, alpha = 0.7) +
    geom_line(aes(y = pro),       colour = "#2ecc71", linewidth = 0.8, alpha = 0.7) +
    scale_y_continuous(
      name     = "Number of Policies",
      sec.axis = sec_axis(~ . / sf, name = "Number of Agents")
    ) +
    labs(title = "Active Policies & Market Sentiment", x = "Time Period") +
    theme_minimal(base_size = 11) +
    theme(
      axis.title.y.left  = element_text(colour = "steelblue"),
      axis.title.y.right = element_text(colour = "darkgray")
    )
  
  # Revenue vs costs 
  
  p2_df <- data.frame(
    t        = seq_len(n_periods),
    Premiums = metrics$premiums,
    Claims   = metrics$claims,
    Expenses = metrics$expenses
  ) |> pivot_longer(-t, names_to = "series", values_to = "amount")
  
  p2 <- ggplot(p2_df, aes(x = t, y = amount, colour = series)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(Premiums = "#2ecc71", Claims = "#e74c3c", Expenses = "orange")
    ) +
    labs(title = "Revenue vs Costs",
         x = "Time Period", y = "Amount ($)", colour = NULL) +
    theme_minimal(base_size = 11)
  
  # Period profit / loss
  p3_df <- data.frame(
    t      = seq_len(n_periods),
    profit = metrics$profit,
    sign   = ifelse(metrics$profit >= 0, "Profit", "Loss")
  )
  
  p3 <- ggplot(p3_df, aes(x = t, y = profit, fill = sign)) +
    geom_col(alpha = 0.6) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
    scale_fill_manual(values = c(Profit = "#2ecc71", Loss = "#e74c3c"), guide = "none") +
    labs(title = "Period Profit / Loss",
         x = "Time Period", y = "Profit ($)") +
    theme_minimal(base_size = 11)
  
  # Combine plots
  print(
    (p1 | p2 | p3) +
      plot_annotation(
        title = sprintf(
          "Insurance Market Simulation  (Premium = $%.0f/mo,  Claim Prob = %.1f%%)",
          premium, claim_prob * 100
        ),
        theme = theme(plot.title = element_text(size = 13, face = "bold"))
      )
  )
}

# Summary
print_summary <- function(metrics, premium, claim_prob, claim_size, expense_ratio) {
  total_profit   <- sum(metrics$profit)
  total_renewals <- sum(metrics$renewals)
  total_lapses   <- sum(metrics$lapses)
  retention_rate <- if ((total_renewals + total_lapses) > 0)
    total_renewals / (total_renewals + total_lapses) else NA
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("SUMMARY\n")
  cat(strrep("=", 60), "\n\n", sep = "")
  cat(" PARAMETERS:\n")
  cat(sprintf("  Monthly Premium:   $%.2f\n",         premium))
  cat(sprintf("  Claim Probability: %.1f%% per month\n", claim_prob * 100))
  cat(sprintf("  Average Claim:     $%.0f\n",          claim_size))
  cat(sprintf("  Expense Ratio:     %.0f%%\n",          expense_ratio * 100))
  cat("\n RESULTS:\n")
  cat(sprintf("  Avg Active Policies: %.0f\n",          mean(metrics$active_policies)))
  cat(sprintf("  Total Profit:        $%s\n",           format(round(total_profit), big.mark = ",")))
  cat(sprintf("  Status:              %s\n",            if (total_profit > 0) "PROFITABLE" else "UNPROFITABLE"))
  cat(sprintf("  Total Renewals:      %d\n",            total_renewals))
  cat(sprintf("  Total Lapses:        %d\n",            total_lapses))
  if (!is.na(retention_rate))
    cat(sprintf("  Retention Rate:      %.1f%%\n",      retention_rate * 100))
  cat(strrep("=", 60), "\n\n", sep = "")
}

# Running the setup

fix_seed       <- 44
n_agents       <- 100
n_periods      <- 120
avg_claim_size <- 2000
theta          <- 0.5
claim_prob     <- 0.02
expense_ratio  <- 0.1

premium_monthly <- calculate_premium(avg_claim_size, theta, claim_prob)

result <- run_simulation(
  n_agents          = n_agents,
  n_periods         = n_periods,
  avg_claim_size    = avg_claim_size,
  theta             = theta,
  claim_probability = claim_prob,
  expense_ratio     = expense_ratio,
  seed              = fix_seed
)

visualize_network(result$G, result$agents)
create_visualizations(result$metrics, n_periods, premium_monthly, claim_prob, avg_claim_size)
print_summary(result$metrics, premium_monthly, claim_prob, avg_claim_size, expense_ratio)