# ==============================================================================
# app.R  —  Hybrid Opinion Risk Model
# R / Shiny port of Hybrid_Opinion_Risk_Model.ipynb
#
# Bugs preserved exactly as in the original Python source (noted inline).
#
# Packages required:
#   install.packages(c("shiny", "ggplot2", "ggraph", "igraph",
#                      "patchwork", "tidyr"))
# ==============================================================================

library(shiny)
library(ggplot2)
library(ggraph)
library(igraph)
library(patchwork)
library(tidyr)

# ==============================================================================
# SECTION 1 — STRATEGIC AGENT MODEL (Expected Utility / PG Model)
# ==============================================================================
#
# Python class: StrategicAgent(id, position, salience, capability)
# R equivalent: data.frame with one row per agent.
#
# influence_score  =  salience * capability
# shift_position   :  each agent shifts toward the influence-weighted average
#                     position of ALL other agents (convergence_parameter = 0.2)
# ==============================================================================

run_strategic_simulation <- function(n_agents = 20, n_steps = 30) {
  
  agents <- data.frame(
    id         = seq_len(n_agents),
    position   = runif(n_agents, -1, 1),
    salience   = runif(n_agents),
    capability = runif(n_agents)
  )
  
  convergence_param <- 0.2
  history    <- matrix(NA_real_, nrow = n_steps, ncol = n_agents)
  inf_scores <- agents$salience * agents$capability   # fixed per run; computed once
  
  for (t in seq_len(n_steps)) {
    for (i in seq_len(n_agents)) {
      others       <- setdiff(seq_len(n_agents), i)
      total_weight <- sum(inf_scores[others])
      if (total_weight == 0) next
      weighted_avg       <- sum(agents$position[others] * inf_scores[others]) / total_weight
      agents$position[i] <- agents$position[i] + convergence_param * (weighted_avg - agents$position[i])
    }
    history[t, ] <- agents$position
  }
  
  # Reshape to long format for ggplot2
  data.frame(
    time_step = rep(seq_len(n_steps), each = n_agents),
    agent     = rep(paste0("Agent ", seq_len(n_agents)), times = n_steps),
    position  = as.vector(t(history))
  )
}

plot_strategic <- function(df) {
  ggplot(df, aes(x = time_step, y = position, color = agent)) +
    geom_line(alpha = 0.7, linewidth = 0.9) +
    labs(
      title = "Convergence to Equilibrium Position",
      x     = "Time Step",
      y     = "Position"
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "none")
}


# ==============================================================================
# SECTION 2 — GALAM OPINION DYNAMICS
# ==============================================================================
#
# Python class: GalamAgent(id, opinion)  where opinion in {-1, 0, +1}
# R equivalent: integer vector of opinions; history stored as matrix.
#
# decide() : local majority rule.
#   - Undecided agents follow the net sign of neighbours.
#   - Committed agents flip if opposing pressure exceeds own-side support.
#
# NOTE: np.random.choice(agents, size=5, replace=False) can include self.
#       Preserved here as sample(seq_len(n_agents), 5, replace = FALSE).
# ==============================================================================

run_galam_simulation <- function(n_agents = 100, n_steps = 10) {
  
  opinions <- sample(c(-1L, 0L, 1L), n_agents, replace = TRUE)
  history  <- matrix(NA_integer_, nrow = n_steps, ncol = n_agents)
  
  for (t in seq_len(n_steps)) {
    history[t, ] <- opinions
    for (i in seq_len(n_agents)) {
      nbrs        <- sample(seq_len(n_agents), size = 5L, replace = FALSE)
      net_opinion <- sum(opinions[nbrs])
      opinions[i] <- as.integer(net_opinion > 0) - as.integer(net_opinion < 0)
    }
  }
  
  data.frame(
    time_step = rep(seq_len(n_steps), 3L),
    opinion   = rep(c("Anti", "Undecided", "Pro"), each = n_steps),
    count     = c(
      rowSums(history == -1L),
      rowSums(history ==  0L),
      rowSums(history ==  1L)
    )
  )
}

plot_galam <- function(df) {
  opinion_colours <- c("Anti" = "#e74c3c", "Undecided" = "gray50", "Pro" = "#2ecc71")
  
  ggplot(df, aes(x = time_step, y = count, colour = opinion)) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = opinion_colours) +
    labs(
      title  = "Galam Opinion Dynamics",
      x      = "Time Step",
      y      = "Number of Agents",
      colour = NULL
    ) +
    theme_minimal(base_size = 13)
}


# ==============================================================================
# SECTION 3 — HYBRID NETWORK MODEL
# ==============================================================================
#
# Python classes: HybridAgent, Influencer
# R equivalent  : agents data.frame + 2-row influencers data.frame
#
# Network : Watts-Strogatz via igraph::sample_smallworld()
#           (nei = 3 gives degree 6 per node, matching networkx k=6)
#
# decide()    : influence-weighted net sum — sign rule (HM.R)
# broadcast() : influencer flips eligible agents with probability `strength`
#               (per-agent loop, matching HM.R)
# ==============================================================================

run_hybrid_simulation <- function(n_agents = 100, influencer_strength = 0.5) {
  
  G        <- sample_smallworld(dim = 1, size = n_agents, nei = 3, p = 0.1)
  
  agents <- data.frame(
    id         = seq_len(n_agents),
    opinion    = sample(c(-1L, 0L, 1L), n_agents, replace = TRUE),
    salience   = runif(n_agents),
    capability = runif(n_agents)
  )
  agents$inf_score <- agents$salience * agents$capability
  
  influencers <- data.frame(
    opinion   = c( 1L, -1L),
    inf_score = c(1.0,  1.0)
  )
  
  n_steps <- 50L
  history <- matrix(NA_integer_, nrow = n_steps, ncol = n_agents)
  
  for (t in seq_len(n_steps)) {
    history[t, ] <- agents$opinion
    
    # ---- Opinion update via net influence-weighted sum (HM.R) ----
    for (i in seq_len(n_agents)) {
      nbrs <- as.integer(neighbors(G, i))
      if (length(nbrs) == 0) next
      net_influence     <- sum(agents$opinion[nbrs] * agents$inf_score[nbrs])
      agents$opinion[i] <- as.integer(net_influence > 0) - as.integer(net_influence < 0)
    }
    
    # ---- Influencer broadcast (shuffled order each period) ----
    for (k in sample(2)) {
      inf_op  <- influencers$opinion[k]
      inf_inf <- influencers$inf_score[k]
      targets <- which(agents$opinion != inf_op & inf_inf > agents$inf_score)
      for (i in targets) {
        if (runif(1) < influencer_strength) agents$opinion[i] <- inf_op
      }
    }
  }
  
  list(agents = agents, graph = G, history = history, n_steps = n_steps)
}

plot_hybrid <- function(result) {
  agents  <- result$agents
  G       <- result$graph
  history <- result$history
  n_steps <- result$n_steps
  
  opinion_colours <- c("-1" = "#e74c3c", "0" = "#95a5a6", "1" = "#2ecc71")
  
  # Add opinion as a vertex attribute so ggraph can colour nodes
  V(G)$opinion <- as.character(agents$opinion)
  
  p_net <- ggraph(G, layout = "kk") +
    geom_edge_link(alpha = 0.25, colour = "gray60") +
    geom_node_point(aes(colour = opinion), size = 2.5, alpha = 0.85) +
    scale_colour_manual(
      values = opinion_colours,
      labels = c("-1" = "Anti", "0" = "Undecided", "1" = "Pro"),
      name   = "Opinion"
    ) +
    labs(title = "Final Opinions on Network") +
    theme_graph(base_size = 12)
  
  ts_df <- data.frame(
    time_step = rep(seq_len(n_steps), 3L),
    opinion   = rep(c("Anti", "Undecided", "Pro"), each = n_steps),
    count     = c(
      rowSums(history == -1L),
      rowSums(history ==  0L),
      rowSums(history ==  1L)
    )
  )
  
  p_ts <- ggplot(ts_df, aes(x = time_step, y = count, colour = opinion)) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(
      values = c("Anti" = "#e74c3c", "Undecided" = "gray50", "Pro" = "#2ecc71")
    ) +
    labs(
      title  = "Public Opinion Over Time",
      x      = "Time Step",
      y      = "Number of Agents",
      colour = NULL
    ) +
    theme_minimal(base_size = 12)
  
  p_net + p_ts
}


# ==============================================================================
# SECTION 4 — INSURANCE RISK SIMULATION
# ==============================================================================
#
# Python classes: ModifiedGalamAgent, ModifiedInfluencer
# R equivalent  : agents data.frame with extra columns
#                 has_policy, policy_start, policy_end, claims_count
#
# Premium (standard deviation principle):
#   P = p * mu * (1 + theta)
#
# Claims ~ Exp(mean = avg_claim_size)
#   R: rexp(n, rate = 1/avg_claim_size)  [rate = 1/scale, matching Python]
#
# Network effects are commented out — preserved exactly from original.
# ==============================================================================

# Global parameters — preserved exactly from Python source
THRESHOLD_PRICE                  <- 150
PROB_BELOW_THRESHOLD_PRO         <- 0.5
PROB_ABOVE_THRESHOLD_PRO         <- 0.1
PROB_BELOW_THRESHOLD_UNDECIDED   <- 0.2
PROB_ABOVE_THRESHOLD_UNDECIDED   <- 0.05
BUY_PROB_ANTI                    <- 0
BUY_PROB_NEIGHBOR_MULTIPLIER     <- 0.02   # commented-out network effect
PROB_RENEWAL_PRO                 <- 0.95
PROB_RENEWAL_UNDECIDED           <- 0
PROB_RENEWAL_ANTI                <- 0.01
RENEWAL_PROB_CLAIM_MULTIPLIER    <- 0.1    # commented-out network effect
RENEWAL_PROB_NEIGHBOR_MULTIPLIER <- 0.02   # commented-out network effect

calculate_premium <- function(mu, theta, p) {
  p * mu * (1 + theta)
}

run_insurance_simulation <- function(
    n_agents                 = 100,
    n_periods                = 120,
    avg_claim_size           = 2000,
    theta                    = 0.5,
    claim_probability        = 0.02,
    expense_ratio            = 0.1,
    network_effects_purchase = FALSE,
    network_effects_renewal  = FALSE,
    seed                     = 44
) {
  if (!is.null(seed)) set.seed(seed)
  
  premium_monthly <- calculate_premium(avg_claim_size, theta, claim_probability)
  
  G <- sample_smallworld(dim = 1, size = n_agents, nei = 3, p = 0.1)
  
  agents <- data.frame(
    id           = seq_len(n_agents),
    opinion      = sample(c(-1L, 0L, 1L), n_agents, replace = TRUE),
    salience     = runif(n_agents),
    capability   = runif(n_agents),
    has_policy   = FALSE,
    policy_start = NA_integer_,
    policy_end   = NA_integer_,
    claims_count = 0L
  )
  agents$inf_score <- agents$salience * agents$capability
  
  influencers <- data.frame(
    opinion   = c( 1L, -1L),
    inf_score = c(1.0,  1.0)
  )
  
  term_length    <- 12L
  annual_premium <- premium_monthly * 12
  
  metrics <- list(
    active_policies     = integer(n_periods),
    new_sales           = integer(n_periods),
    renewals            = integer(n_periods),
    lapses              = integer(n_periods),
    premiums            = numeric(n_periods),
    claims              = numeric(n_periods),
    expenses            = numeric(n_periods),
    profit              = numeric(n_periods),
    opinion_distribution = matrix(NA_integer_, nrow = n_periods, ncol = 3L)
  )
  
  for (t in seq_len(n_periods)) {
    
    # ---- Opinion update (net influence-weighted sum) ----
    for (i in seq_len(n_agents)) {
      nbrs <- as.integer(neighbors(G, i))
      if (length(nbrs) == 0) next
      net_influence     <- sum(agents$opinion[nbrs] * agents$inf_score[nbrs])
      agents$opinion[i] <- as.integer(net_influence > 0) - as.integer(net_influence < 0)
    }
    
    # ---- Influencer broadcast (hardcoded strength = 0.2, as in original) ----
    for (k in sample(2)) {
      inf_op  <- influencers$opinion[k]
      inf_inf <- influencers$inf_score[k]
      targets <- which(agents$opinion != inf_op & inf_inf > agents$inf_score)
      for (i in targets) {
        if (runif(1) < 0.2) agents$opinion[i] <- inf_op
      }
    }
    
    # ---- Policy decisions ----
    new_sales <- 0L
    renewals  <- 0L
    lapses    <- 0L
    
    for (i in seq_len(n_agents)) {
      
      if (isTRUE(agents$has_policy[i]) &&
          !is.na(agents$policy_end[i]) &&
          t >= agents$policy_end[i]) {
        
        # --- Renewal decision ---
        op <- agents$opinion[i]
        renewal_prob <- if      (op ==  1L) PROB_RENEWAL_PRO
        else if (op ==  0L) PROB_RENEWAL_UNDECIDED
        else                PROB_RENEWAL_ANTI
        
        if (network_effects_renewal) {
          nbrs         <- as.integer(neighbors(G, i))
          renewal_prob <- renewal_prob +
            RENEWAL_PROB_CLAIM_MULTIPLIER    * agents$claims_count[i] +
            RENEWAL_PROB_NEIGHBOR_MULTIPLIER * sum(agents$claims_count[nbrs])
        }
        
        if (runif(1) < renewal_prob) {
          agents$policy_start[i] <- t
          agents$policy_end[i]   <- t + term_length
          renewals <- renewals + 1L
        } else {
          agents$has_policy[i] <- FALSE
          lapses <- lapses + 1L
        }
        
      } else if (!isTRUE(agents$has_policy[i])) {
        
        # --- Purchase decision ---
        op <- agents$opinion[i]
        
        buy_prob <- if (op == 1L) {
          if (annual_premium < THRESHOLD_PRICE) PROB_BELOW_THRESHOLD_PRO
          else                                  PROB_ABOVE_THRESHOLD_PRO
        } else if (op == 0L) {
          if (annual_premium < THRESHOLD_PRICE) PROB_BELOW_THRESHOLD_UNDECIDED
          else                                  PROB_ABOVE_THRESHOLD_UNDECIDED
        } else {
          BUY_PROB_ANTI
        }
        
        if (network_effects_purchase) {
          nbrs     <- as.integer(neighbors(G, i))
          buy_prob <- buy_prob +
            BUY_PROB_NEIGHBOR_MULTIPLIER *
            sum(agents$claims_count[nbrs[agents$has_policy[nbrs]]])
        }
        
        if (runif(1) < buy_prob) {
          agents$has_policy[i]   <- TRUE
          agents$policy_start[i] <- t
          agents$policy_end[i]   <- t + term_length
          new_sales <- new_sales + 1L
        }
      }
    }
    
    # ---- Accounting ----
    active_mask <- agents$has_policy &
      !is.na(agents$policy_start) & !is.na(agents$policy_end) &
      t >= agents$policy_start & t < agents$policy_end
    
    active_count      <- sum(active_mask)
    premium_collected <- active_count * premium_monthly
    expenses_total    <- premium_collected * expense_ratio
    
    # Claims: one uniform draw per agent, vectorised
    claimants    <- which(active_mask & runif(n_agents) < claim_probability)
    claims_total <- if (length(claimants) > 0)
      sum(rexp(length(claimants), rate = 1 / avg_claim_size))   # rate = 1/mean
    else 0
    
    agents$claims_count[claimants] <- agents$claims_count[claimants] + 1L
    
    profit <- premium_collected - claims_total - expenses_total
    
    metrics$active_policies[t]        <- active_count
    metrics$new_sales[t]              <- new_sales
    metrics$renewals[t]               <- renewals
    metrics$lapses[t]                 <- lapses
    metrics$premiums[t]               <- premium_collected
    metrics$claims[t]                 <- claims_total
    metrics$expenses[t]               <- expenses_total
    metrics$profit[t]                 <- profit
    metrics$opinion_distribution[t, ] <- c(
      sum(agents$opinion == -1L),
      sum(agents$opinion ==  0L),
      sum(agents$opinion ==  1L)
    )
  }
  
  list(
    agents          = agents,
    graph           = G,
    metrics         = metrics,
    premium_monthly = premium_monthly,
    params          = list(
      claim_probability = claim_probability,
      avg_claim_size    = avg_claim_size,
      expense_ratio     = expense_ratio
    )
  )
}

plot_insurance_network <- function(result) {
  agents <- result$agents
  G      <- result$graph
  
  V(G)$opinion    <- as.character(agents$opinion)
  opinion_colours <- c("-1" = "#e74c3c", "0" = "#95a5a6", "1" = "#2ecc71")
  
  ggraph(G, layout = "kk") +
    geom_edge_link(alpha = 0.25, colour = "gray60") +
    geom_node_point(aes(colour = opinion), size = 2.5, alpha = 0.85) +
    scale_colour_manual(
      values = opinion_colours,
      labels = c("-1" = "Anti", "0" = "Undecided", "1" = "Pro"),
      name   = "Opinion"
    ) +
    labs(title = "Final Network State") +
    theme_graph(base_size = 12)
}

plot_insurance_metrics <- function(result) {
  m      <- result$metrics
  prem   <- result$premium_monthly
  params <- result$params
  n_per  <- length(m$active_policies)
  ts     <- seq_len(n_per)
  
  opinion_colours <- c("Anti" = "#e74c3c", "Undecided" = "gray50", "Pro" = "#2ecc71")
  
  # --- Plot 1: Active Policies & Market Sentiment ---
  # Both series are counts of agents so share the same axis (avoids dual-axis).
  od           <- as.data.frame(m$opinion_distribution)
  colnames(od) <- c("Anti", "Undecided", "Pro")
  od$time_step <- ts
  od_long      <- tidyr::pivot_longer(od, cols = -time_step,
                                      names_to = "opinion", values_to = "count")
  
  p1 <- ggplot() +
    geom_line(
      data = data.frame(time_step = ts, count = m$active_policies),
      aes(x = time_step, y = count, linetype = "Active Policies"),
      colour = "steelblue", linewidth = 1.3
    ) +
    geom_line(
      data = od_long,
      aes(x = time_step, y = count, colour = opinion),
      alpha = 0.65, linewidth = 0.9
    ) +
    scale_colour_manual(values = opinion_colours, name = "Sentiment") +
    scale_linetype_manual(values = c("Active Policies" = "solid"), name = NULL) +
    labs(title = "Active Policies & Market Sentiment",
         x = "Time Period", y = "Count") +
    theme_minimal(base_size = 11)
  
  # --- Plot 2: Revenue vs Costs ---
  fin_df <- data.frame(
    time_step = rep(ts, 3L),
    series    = rep(c("Premiums", "Claims", "Expenses"), each = n_per),
    value     = c(m$premiums, m$claims, m$expenses)
  )
  fin_colours <- c("Premiums" = "#2ecc71", "Claims" = "#e74c3c",
                   "Expenses" = "orange")
  
  p2 <- ggplot(fin_df, aes(x = time_step, y = value, colour = series)) +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = fin_colours, name = NULL) +
    labs(title = "Revenue vs Costs", x = "Time Period", y = "Amount ($)") +
    theme_minimal(base_size = 11)
  
  # --- Plot 3: Period Profit / Loss ---
  prof_df <- data.frame(
    time_step = ts,
    profit    = m$profit,
    sign      = ifelse(m$profit >= 0, "Profit", "Loss")
  )
  
  p3 <- ggplot(prof_df, aes(x = time_step, y = profit, fill = sign)) +
    geom_col(alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_fill_manual(
      values = c("Profit" = "#2ecc71", "Loss" = "#e74c3c"), name = NULL
    ) +
    labs(title = "Period Profit / Loss", x = "Time Period", y = "Profit ($)") +
    theme_minimal(base_size = 11)
  
  (p1 | p2 | p3) +
    plot_annotation(
      title = sprintf(
        "Insurance Market Simulation  |  Premium = $%.2f/mo  |  Claim Prob = %.1f%%",
        prem, params$claim_probability * 100
      ),
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
}

build_summary_table <- function(result) {
  m      <- result$metrics
  prem   <- result$premium_monthly
  params <- result$params
  
  total_profit   <- sum(m$profit)
  total_renewals <- sum(m$renewals)
  total_lapses   <- sum(m$lapses)
  
  retention <- if ((total_renewals + total_lapses) > 0)
    total_renewals / (total_renewals + total_lapses)
  else NA_real_
  
  data.frame(
    Category = c(
      rep("Parameters", 4L),
      rep("Results",    6L)
    ),
    Metric = c(
      "Monthly Premium ($)", "Claim Probability",
      "Avg Claim ($)",       "Expense Ratio",
      "Avg Active Policies", "Total Profit ($)", "Status",
      "Total Renewals",      "Total Lapses",     "Retention Rate"
    ),
    Value = c(
      sprintf("%.2f",  prem),
      sprintf("%.1f%%", params$claim_probability * 100),
      sprintf("%.0f",  params$avg_claim_size),
      sprintf("%.0f%%", params$expense_ratio * 100),
      sprintf("%.0f",  mean(m$active_policies)),
      sprintf("%.0f",  total_profit),
      ifelse(total_profit >= 0, "PROFITABLE", "UNPROFITABLE"),
      as.character(total_renewals),
      as.character(total_lapses),
      ifelse(is.na(retention), "N/A", sprintf("%.1f%%", retention * 100))
    ),
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# UI
# ==============================================================================

ui <- navbarPage(
  title = "Hybrid Opinion Risk Model",
  
  # --------------------------------------------------------------------------
  # Tab 1: Strategic Agent Model
  # --------------------------------------------------------------------------
  tabPanel(
    "Strategic Agent Model",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parameters"),
        sliderInput("strat_n_agents", "Number of Agents",
                    min = 5, max = 50, value = 20, step = 1),
        sliderInput("strat_n_steps", "Time Steps",
                    min = 10, max = 100, value = 30, step = 5),
        actionButton("strat_run", "Run Simulation",
                     class = "btn-primary", width = "100%"),
        hr(),
        helpText(
          "Each agent holds a position on [-1, 1] and iteratively shifts",
          "toward the influence-weighted average position of all others.",
          "Convergence parameter = 0.2."
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("strat_plot", height = "460px")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # Tab 2: Galam Opinion Dynamics
  # --------------------------------------------------------------------------
  tabPanel(
    "Opinion Dynamics (Galam)",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parameters"),
        sliderInput("galam_n_agents", "Number of Agents",
                    min = 20, max = 200, value = 100, step = 10),
        sliderInput("galam_n_steps", "Time Steps",
                    min = 5, max = 30, value = 10, step = 1),
        actionButton("galam_run", "Run Simulation",
                     class = "btn-primary", width = "100%"),
        hr(),
        helpText(
          "Agents hold opinions in {-1, 0, +1} (Anti, Undecided, Pro).",
          "Each step agents are shuffled; each agent samples 5 peers and",
          "adopts the majority opinion."
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("galam_plot", height = "460px")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # Tab 3: Hybrid Network Model
  # --------------------------------------------------------------------------
  tabPanel(
    "Hybrid Network Model",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parameters"),
        sliderInput("hybrid_n_agents", "Number of Agents",
                    min = 20, max = 200, value = 100, step = 10),
        sliderInput("hybrid_inf_strength", "Influencer Strength",
                    min = 0.0, max = 1.0, value = 0.5, step = 0.1),
        actionButton("hybrid_run", "Run Simulation",
                     class = "btn-primary", width = "100%"),
        hr(),
        helpText(
          "Agents sit on a Watts-Strogatz network (k = 6, p = 0.1).",
          "Opinions update via influence-weighted network majority rule.",
          "One Pro and one Anti influencer broadcast each period."
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("hybrid_plot", height = "480px")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # Tab 4: Insurance Risk Simulation
  # --------------------------------------------------------------------------
  tabPanel(
    "Insurance Risk Simulation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parameters"),
        sliderInput("ins_n_agents", "Number of Agents",
                    min = 20, max = 200, value = 100, step = 10),
        sliderInput("ins_n_periods", "Periods (months)",
                    min = 24, max = 240, value = 120, step = 12),
        sliderInput("ins_theta", "Risk Loading (θ)",
                    min = 0.01, max = 1.0, value = 0.5, step = 0.01),
        sliderInput("ins_claim_prob", "Claim Probability",
                    min = 0.005, max = 0.1, value = 0.02, step = 0.005),
        sliderInput("ins_expense", "Expense Ratio",
                    min = 0.0, max = 0.5, value = 0.1, step = 0.05),
        numericInput("ins_seed", "Random Seed",
                     value = 44, min = 1, max = 99999, step = 1),
        hr(),
        h4("Network Effects"),
        checkboxInput("ins_net_purchase", "Enable on purchase decisions", value = FALSE),
        checkboxInput("ins_net_renewal",  "Enable on renewal decisions",  value = FALSE),
        actionButton("ins_run", "Run Simulation",
                     class = "btn-primary", width = "100%"),
        hr(),
        helpText(
          "Claims ~ Exp(mean = $2,000). Premium via standard deviation",
          "principle: P = p·μ·(1+θ). Agents buy and renew 12-month",
          "policies based on opinion and price thresholds."
        )
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(6, plotOutput("ins_network_plot", height = "320px")),
          column(6, tableOutput("ins_summary"))
        ),
        hr(),
        plotOutput("ins_metrics_plot", height = "320px")
      )
    )
  )
)


# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # Tab 1: Strategic Agent Model
  # --------------------------------------------------------------------------
  strat_data <- eventReactive(input$strat_run, {
    run_strategic_simulation(
      n_agents = input$strat_n_agents,
      n_steps  = input$strat_n_steps
    )
  }, ignoreNULL = FALSE)
  
  output$strat_plot <- renderPlot({
    plot_strategic(strat_data())
  })
  
  # --------------------------------------------------------------------------
  # Tab 2: Galam Opinion Dynamics
  # --------------------------------------------------------------------------
  galam_data <- eventReactive(input$galam_run, {
    run_galam_simulation(
      n_agents = input$galam_n_agents,
      n_steps  = input$galam_n_steps
    )
  }, ignoreNULL = FALSE)
  
  output$galam_plot <- renderPlot({
    plot_galam(galam_data())
  })
  
  # --------------------------------------------------------------------------
  # Tab 3: Hybrid Network Model
  # --------------------------------------------------------------------------
  hybrid_data <- eventReactive(input$hybrid_run, {
    withProgress(message = "Running hybrid simulation...", value = 0.5, {
      run_hybrid_simulation(
        n_agents            = input$hybrid_n_agents,
        influencer_strength = input$hybrid_inf_strength
      )
    })
  }, ignoreNULL = FALSE)
  
  output$hybrid_plot <- renderPlot({
    plot_hybrid(hybrid_data())
  })
  
  # --------------------------------------------------------------------------
  # Tab 4: Insurance Risk Simulation
  # --------------------------------------------------------------------------
  ins_data <- eventReactive(input$ins_run, {
    withProgress(message = "Running insurance simulation...", value = 0.5, {
      run_insurance_simulation(
        n_agents                 = input$ins_n_agents,
        n_periods                = input$ins_n_periods,
        theta                    = input$ins_theta,
        claim_probability        = input$ins_claim_prob,
        expense_ratio            = input$ins_expense,
        network_effects_purchase = input$ins_net_purchase,
        network_effects_renewal  = input$ins_net_renewal,
        seed                     = input$ins_seed
      )
    })
  }, ignoreNULL = FALSE)
  
  output$ins_network_plot <- renderPlot({
    plot_insurance_network(ins_data())
  })
  
  output$ins_metrics_plot <- renderPlot({
    plot_insurance_metrics(ins_data())
  })
  
  output$ins_summary <- renderTable({
    build_summary_table(ins_data())
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
}


# ==============================================================================
shinyApp(ui, server)