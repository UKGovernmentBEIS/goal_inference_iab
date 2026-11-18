# Setup ----

pdf(NULL)
options(scipen = 999)

library(afex)
library(lme4)
library(dplyr)
library(emmeans)
library(car)
library(ggplot2)
library(cowplot)

afex::afex_options(sig_symbols = rep("", 4))

# Formatting for figures
# Agent display labels
agent_labels <- c("gpt-4o-2024-08-06" = "GPT-4o",
                  "gpt-5-6-sol"       = "GPT-5.6 Sol",
                  "qwen3-8-2-4t-a95b" = "Qwen 3.8",
                  "sft-8b"            = "SFT Llama-3.1-8B",
                  "base-8b"           = "Base Llama-3.1-8B",
                  "human"             = "Human")
format_agent_fig <- function(x) unname(ifelse(x %in% names(agent_labels), agent_labels[x], x))

scale_y_percent <- function(...)
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"), ...)

local({
  for (reg in list(grDevices::pdfFonts, grDevices::postscriptFonts)) {
    if (!"Arial" %in% names(reg())) reg(Arial = reg()$Helvetica)
  }
})

theme_tom <- function(base_size = 13, base_family = "Arial") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom")
}

# 95% Wilson CI (for descriptives)
wilson_ci <- function(x, n, conf = 0.95) {
  ci <- mapply(function(k, m)
    prop.test(k, m, conf.level = conf, correct = FALSE)$conf.int, x, n)
  data.frame(prop = x / n, lower = ci[1, ], upper = ci[2, ])
}

# Headings for txt log
log_heading <- function(title, analysis = NULL) {
  bar <- strrep("=", 78)
  cat(sprintf("\n\n%s\n%s\n", bar, title))
  if (!is.null(analysis)) cat(sprintf("Analysis: %s\n", analysis))
  cat(bar, "\n", sep = "")
}
log_subheading <- function(title) cat(sprintf("\n----- %s -----\n", title))

# One-sided (greater) binomial test of each cell's success count vs chance (0.5), Holm-adjusted.
binom_chance_test <- function(desc, success_col) {
  desc %>% rowwise() %>%
    mutate(binom_p = binom.test(.data[[success_col]], n, p = 0.5, alternative = "greater")$p.value) %>%
    ungroup() %>%
    mutate(p_adjusted = p.adjust(binom_p, method = "holm"))
}

# save to txt log file
LOG_FILE <- "analysis_consolidated_neurips_log.txt"
.log_con <- file(LOG_FILE, open = "wt")
sink(.log_con, split = TRUE)
cat(sprintf("analysis_consolidated_neurips.R run: %s\n\n", format(Sys.time())))

## Functions ----

# SFT vs its base model, emmeans comparison 
sft_vs_base <- function(emm_obj) {
  lv <- emm_obj@levels$llm_type
  contrast(emm_obj, method = list(
    "sft-8b - base-8b" = as.numeric(lv == "sft-8b") - as.numeric(lv == "base-8b")))
}

# Per-agent success counts (descriptive)
desc_agent <- function(df, dv) {
  d <- df %>% filter(!is.na(.data[[dv]])) %>%
    mutate(agent = ifelse(model_type == "dp", "DP solver", as.character(model_type))) %>%
    group_by(agent) %>%
    summarise(n = n(), k = sum(.data[[dv]]), .groups = "drop") %>%
    arrange(agent == "DP solver")
  bind_cols(d, wilson_ci(d$k, d$n))
}

fig_theme <- theme(plot.title = element_text(size = 17),
                   legend.text = element_text(size = 15),
                   axis.text = element_text(size = 15),
                   axis.text.x = element_text(angle = 0, hjust = 0.5),
                   axis.text.y = element_text(margin = margin(r = 4)),
                   axis.title = element_text(size = 16),
                   axis.title.x = element_text(margin = margin(t = 4)),
                   axis.title.y = element_text(margin = margin(r = 10)),
                   plot.margin = margin(5.5, 14, 5.5, 14),
                   legend.position = "none")

## Load Data (analysis-ready) ----

# One combined df - all agents
trials <- bind_rows(read.csv("results/trials_neurips.csv", stringsAsFactors = FALSE),
                    read.csv("results/dp_trials_neurips.csv", stringsAsFactors = FALSE))
cat(sprintf("Trial rows: %d (%s)\n", nrow(trials),
            paste(sort(unique(trials$model_type)), collapse = ", ")))

## Trial subsets

# RQ1a: My target vs neutral -> should choose 'my target'
sub_mine <- trials %>%
  filter(trial_type == "My target vs neutral",
         card1_type_belief != "both", card2_type_belief != "both") %>%
  mutate(chose_my_target = as.integer(chosen_card_type == "mine_only"))

# Pooled blocking trials, one DV (chose_better = the higher-P(help_opp) card):
# (i)  Opp target vs neutral -> should choose the opp target card (block);
# (ii) Two opp targets  -> should choose the higher-P(help_opp) card;
# prob_difference = the absolute gap in P(helps opponent) between the cards (neutral is always 0)
sub_block <- bind_rows(
  trials %>%
    filter(trial_type == "Opponent target vs neutral",
           card1_type_belief != "both", card2_type_belief != "both") %>%
    mutate(chose_opp_target = as.integer(chosen_card_type == "opp_only"),
           prob_difference = ifelse(card1_type_belief == "opp_only",
                                    card1_prob_helps_opp - card2_prob_helps_opp,
                                    card2_prob_helps_opp - card1_prob_helps_opp),
           chose_better = chose_opp_target),
  trials %>%
    filter(trial_type == "Two opp targets (exclusive)",
           card1_prob_helps_opp != card2_prob_helps_opp) %>%
    mutate(chose_higher_prob = as.integer(chosen_prob > not_chosen_prob),
           prob_difference   = abs(card1_prob_helps_opp - card2_prob_helps_opp),
           chose_better = chose_higher_prob))

# RQ2c: My target vs opponent target -> DV is choice to block (the opp target card).
sub_2c <- trials %>%
  filter(trial_type == "My target vs opponent target") %>%
  mutate(chose_opp_target = as.integer(chosen_card_type == "opp_only"),
         prob_difference = ifelse(card1_type_belief == "opp_only",
                                  card1_prob_helps_opp - card2_prob_helps_opp,
                                  card2_prob_helps_opp - card1_prob_helps_opp),
         my_ctw = as.numeric(my_ctw), opp_ctw = as.numeric(opp_ctw)) %>%
  filter(!is.na(my_ctw), !is.na(opp_ctw))

# Analysis 1: My Target vs Neutral (chance tests) ----

log_heading("MY TARGET VS NEUTRAL")

log_subheading("Descriptives: P(choose my target) by agent + exact binomial tests vs chance (50%)")

desc_mine <- desc_agent(sub_mine, "chose_my_target")
print(desc_mine, n = 100)

# Chance tests on all agent cells
binom_test_mine <- binom_chance_test(desc_mine, "k")
cat("\nExact binomial tests vs 50% (one-sided, Holm):\n")
print(binom_test_mine, n = 100)

# Analysis 2: Blocking (combined) ----

log_heading("BLOCKING: OPP TARGET VS NEUTRAL + TWO OPP TARGETS")

df_block <- sub_block %>%
  mutate(cluster = ifelse(model_type == "dp", unique_game_id, cluster)) %>%
  transmute(model_type, opponent_type, unique_game_id, cluster, trial_type,
            chose_better, prob_difference) %>%
  mutate(
    llm_type = factor(model_type),
    opponent_type = factor(opponent_type),
    trial_type = factor(trial_type),
    cluster = factor(cluster),
    prob_difference_c = prob_difference - mean(prob_difference)
  )

dp_block <- sub_block %>% filter(model_type == "dp")

## Analysis 2: Model ----

log_subheading("Mixed-effects logistic regression")

model_block <- mixed(
  chose_better ~ prob_difference_c * llm_type + opponent_type + (1 | cluster),
  data = df_block,
  family = binomial(link = "logit"), method = "LRT",
  control = glmerControl(optimizer = "bobyqa")
)
print(model_block)
model_block_for_emm <- model_block$full_model
print(summary(model_block_for_emm))

## Analysis 2: EMM ----

log_subheading("EMM (at mean prob-difference)")

emm_block <- emmeans(model_block_for_emm, ~ llm_type,
                     type = "response", at = list(prob_difference_c = 0))
print(emm_block)

## Analysis 2: Slopes ----

log_subheading("Slope of prob difference by LLM (95% CI)")

# Change in log-odds of choosing the better card per unit increase in
# prob_difference, averaged over opponent type.
emm_prob_by_llm <- emtrends(model_block_for_emm, ~ llm_type, var = "prob_difference_c")
print(summary(emm_prob_by_llm, infer = TRUE))

## Analysis 2: SFT vs Base contrast ----

log_subheading("Contrast: SFT vs Base")

cat("P(choose better card), odds ratio at mean prob-difference:\n")
print(summary(sft_vs_base(emm_block), infer = TRUE))
cat("\nProb-difference slope, difference in log-odds per unit:\n")
print(summary(sft_vs_base(emm_prob_by_llm), infer = TRUE))

# Analysis 3: Win Rates (LLMs vs Human) ----

## Analysis 3: Data ----

log_heading("WIN RATES: LLMs vs HUMAN")

# games_neurips.csv: one row per completed game, all agents
combined <- read.csv("results/games_neurips.csv", stringsAsFactors = FALSE) %>%
  mutate(agent_type = factor(agent_type), opponent_type = factor(opponent_type),
         cluster = factor(ifelse(agent_type == "Human" & !is.na(participant) & participant != "",
                                 participant, unique_game_id)))
cat(sprintf("Games: %d\n", nrow(combined)))
print(count(combined, agent_type))

## Analysis 3: Human comparison model ----

log_subheading("Logistic regression for human and LLM win rate (DP excluded)")

df_win <- combined %>%
  mutate(agent_type = relevel(factor(as.character(agent_type)), ref = "Human"))
model_win <- glm(P1_win ~ agent_type + opponent_type,
                 data = df_win, family = binomial)
print(summary(model_win))

cat("\nOmnibus Type III ANOVA (LR):\n")
print(Anova(model_win, type = "III", test.statistic = "LR"))

emm_win <- emmeans(model_win, ~ agent_type, type = "response")
cat("\nWin rate by agent (EMM, response scale, averaged over opponent type):\n")
print(summary(emm_win))

con_win <- summary(contrast(emm_win, method = "pairwise", adjust = "tukey"),
                   infer = TRUE)
cat("\nPairwise contrasts (Tukey, odds ratios), all agent pairs:\n")
print(con_win)

## Analysis 3: Plot ----

plot_src <- as.data.frame(summary(emm_win)) %>%
  transmute(agent_type = as.character(agent_type), prop = prob,
            lower = asymp.LCL, upper = asymp.UCL, P1_win_rate = prob)

plot_df <- plot_src %>%
  filter(agent_type != "Human") %>%
  mutate(agent_label = format_agent_fig(agent_type)) %>%
  arrange(desc(prop))

plot_df$agent_label <- factor(plot_df$agent_label, levels = plot_df$agent_label)

model_colors <- c("GPT-5.6 Sol" = "#2a78d6", "Qwen 3.8" = "#eb6834",
                  "GPT-4o" = "#4a3aa7",
                  "SFT Llama-3.1-8B" = "#999933", "Base Llama-3.1-8B" = "#B23A48")

ref_lines <- plot_src %>%
  filter(agent_type == "Human") %>%
  mutate(ref_label = "Human")

p_win <- ggplot(plot_df, aes(x = agent_label, y = prop)) +
  geom_col(aes(fill = agent_label), width = 0.7, alpha = 0.9) +
  scale_x_discrete(labels = function(x) ifelse(nchar(x) > 10, sub(" ", "\n", x), x)) +
  scale_fill_manual(values = model_colors, guide = "none") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.6) +
  geom_hline(data = ref_lines,
             aes(yintercept = P1_win_rate, colour = ref_label),
             linetype = "dotted", linewidth = 0.8) +
  geom_text(data = ref_lines,
            aes(x = Inf, y = P1_win_rate, label = ref_label, colour = ref_label),
            hjust = 1.05, vjust = -0.5, size = 5, show.legend = FALSE) +
  scale_colour_manual(values = c("Human" = "#1b7837")) +
  scale_y_percent(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  labs(title = "Win rate (greedy + oppcost opponents)",
       x = "Agent", y = "Win Rate") +
  theme_tom() + fig_theme

# included as panel C of the combined figure
print(p_win)

# Analysis 4: Blocking vs Progress (My Target vs Opponent Target) ----

## Analysis 4: Data ----

log_heading("BLOCKING VS PROGRESS: MY TARGET VS OPPONENT TARGET",
            "Firth logistic regression of blocking on ΔP(help_opp) and both players' cards-to-win, per model")

# 'progress' = cards-to-win (ctw, distance from winning)
df_2c <- sub_2c %>%
  filter(model_type != "dp") %>%  # DP enters df_2c_model below as its own level
  mutate(
    llm_type = factor(model_type),
    opponent_type = factor(opponent_type),
    my_ctw_c = my_ctw - mean(my_ctw),
    opp_ctw_c = opp_ctw - mean(opp_ctw),
    prob_difference_c = prob_difference - mean(prob_difference)
  )

dp_2c <- sub_2c %>% filter(model_type == "dp")

df_2c_model <- bind_rows(
    df_2c %>% mutate(llm_type = as.character(llm_type),
                     opponent_type = as.character(opponent_type)),
    dp_2c %>%
      mutate(llm_type = "dp_solver",
             my_ctw_c = my_ctw - mean(df_2c$my_ctw),
             opp_ctw_c = opp_ctw - mean(df_2c$opp_ctw),
             prob_difference_c = prob_difference - mean(df_2c$prob_difference))) %>%
  mutate(llm_type = factor(llm_type), opponent_type = factor(opponent_type))

cat(sprintf("N trials: %d\n", nrow(df_2c)))
cat(sprintf("N games: %d\n", n_distinct(df_2c$unique_game_id)))
cat("\nCrosstab (LLM x Opponent):\n")
print(table(df_2c$llm_type, df_2c$opponent_type))

## Analysis 4: Descriptives ----

log_subheading("Descriptives: proportion blocking (Wilson 95% CI)")

desc_2c <- desc_agent(sub_2c, "chose_opp_target")
print(desc_2c)

## Analysis 4: Model ----

log_subheading("BIC-selected logistic regression (Type III LR ANOVA; Firth refit for slopes/EMMs)")

# Model selected by backward-BIC elimination - commented out due to runtime, but can be reproduced.
# Final model hardcoded below
#
# library(buildmer)  # install.packages("buildmer")
# sel_2c <- buildmer(
#   chose_opp_target ~ my_ctw_c * opp_ctw_c * prob_difference_c * llm_type +
#     opponent_type,
#   data = df_2c_model, family = binomial,
#   buildmerControl = buildmerControl(
#     direction = "backward", crit = "BIC",
#     include = ~ llm_type + opponent_type))
# cat(deparse1(formula(sel_2c@model)), "\nBIC:", BIC(sel_2c@model), "\n")
model_2c_ml <- glm(
  chose_opp_target ~ my_ctw_c + opp_ctw_c + prob_difference_c + llm_type +
    opponent_type + my_ctw_c:opp_ctw_c + my_ctw_c:llm_type + opp_ctw_c:llm_type +
    prob_difference_c:llm_type + my_ctw_c:opp_ctw_c:llm_type,
  data = df_2c_model, family = binomial(link = "logit"))

print(summary(model_2c_ml))
print(Anova(model_2c_ml, type = "III", test.statistic = "LR"))

# GPT-4o never chooses the opp-target card, so slopes/EMMs for
# reporting come from a Firth-type bias-reduced refit (brglm2) of the selected model
cat("\nFirth (brglm2) refit of the selected model (slopes/EMMs reported from this fit):\n")
model_2c <- update(model_2c_ml, method = brglm2::brglmFit)
print(summary(model_2c)$coefficients, digits = 3)

## Analysis 4: Slopes ----

log_subheading("Per-model ΔP(help_opp) and cards-to-win slopes (emtrends, 95% CI)")

trend_2c_prob <- emtrends(model_2c, ~ llm_type, var = "prob_difference_c")
trend_2c_my   <- emtrends(model_2c, ~ llm_type, var = "my_ctw_c")
trend_2c_opp  <- emtrends(model_2c, ~ llm_type, var = "opp_ctw_c")
cat("Evidence, ΔP(help_opp) (prob_difference):\n")
print(summary(trend_2c_prob, infer = TRUE))
cat("\nOwn cards-to-win (my_ctw):\n")
print(summary(trend_2c_my, infer = TRUE))
cat("\nOpponent cards-to-win (opp_ctw):\n")
print(summary(trend_2c_opp, infer = TRUE))

## Analysis 4: SFT vs Base contrast ----

log_subheading("Planned contrast: SFT 8B vs Base 8B (ΔP + cards-to-win slopes)")

# Slope differences
cat("ΔP(help_opp) slope difference:\n")
print(summary(sft_vs_base(trend_2c_prob), infer = TRUE))
cat("\nOwn cards-to-win (my_ctw) slope difference:\n")
print(summary(sft_vs_base(trend_2c_my), infer = TRUE))
cat("\nOpponent cards-to-win (opp_ctw) slope difference:\n")
print(summary(sft_vs_base(trend_2c_opp), infer = TRUE))

# Combined Figure ----

dp_label <- "DP (near-optimal)"

agent_levels <- levels(plot_df$agent_label)
pred_levels <- c(agent_levels, "Human", dp_label)
curve_colors <- c(model_colors, setNames("#c93d82", dp_label), "Human" = "#1b7837")
curve_linetypes <- setNames(ifelse(names(curve_colors) == dp_label, "dashed", "solid"),
                            names(curve_colors))

# 95% CI for figure
plogis_ci <- function(fit, se) {
  z <- qnorm(0.975)
  data.frame(prob = plogis(fit), lcl = plogis(fit - z * se), ucl = plogis(fit + z * se))
}

# Panel A: model_block predictions over the observed prob_difference range.
pd_mean <- mean(df_block$prob_difference)
pd_grid <- seq(min(df_block$prob_difference), max(df_block$prob_difference),
               length.out = 60)
curve_pred <- as.data.frame(
  emmeans(model_block_for_emm, ~ prob_difference_c | llm_type,
          at = list(prob_difference_c = pd_grid - pd_mean), type = "response")) %>%
  # DP's display curve comes from its own uncentered Firth fit below
  filter(llm_type != "dp") %>%
  transmute(agent_label = format_agent_fig(as.character(llm_type)),
            prob_difference = prob_difference_c + pd_mean,
            prob, lcl = asymp.LCL, ucl = asymp.UCL)

m_dp_block <- glm(chose_better ~ prob_difference, data = dp_block,
                  family = binomial(link = "logit"), method = brglm2::brglmFit)
nd <- data.frame(prob_difference = pd_grid)
pr <- predict(m_dp_block, newdata = nd, type = "link", se.fit = TRUE)
curve_pred <- bind_rows(curve_pred,
  bind_cols(nd, plogis_ci(pr$fit, pr$se.fit)) %>% mutate(agent_label = dp_label))
curve_pred <- curve_pred %>%
  mutate(agent_label = factor(agent_label, levels = pred_levels))

p_panel_curve <- ggplot(curve_pred, aes(x = prob_difference, y = prob,
                                        colour = agent_label, fill = agent_label,
                                        linetype = agent_label)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  scale_y_percent(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_colour_manual(values = curve_colors) +
  scale_fill_manual(values = curve_colors) +
  scale_linetype_manual(values = curve_linetypes) +
  # x cropped to the supported range: prob_difference = 0 is impossible by
  # construction
  coord_cartesian(xlim = c(min(pd_grid), 1)) +
  labs(title = "Blocking by ΔP(help_opp)",
       x = "ΔP(help_opp)", y = "P(choose better blocking card)",
       colour = NULL, fill = NULL, linetype = NULL) +
  theme_tom() + fig_theme

# Panel B: model_2c predictions 
m_my  <- mean(df_2c$my_ctw)
m_opp <- mean(df_2c$opp_ctw)
rp_grid <- seq(-4, 4, by = 0.25)
prog_pred <- bind_rows(lapply(rp_grid, function(rp) {
  s <- (rp - (m_my - m_opp)) / 2
  as.data.frame(emmeans(model_2c, ~ llm_type, type = "response",
                        at = list(my_ctw_c = s, opp_ctw_c = -s))) %>%
    mutate(rel_progress = rp)
})) %>%
  transmute(agent_label = format_agent_fig(as.character(llm_type)),
            rel_progress, prob, lcl = asymp.LCL, ucl = asymp.UCL)

dp_prog <- dp_2c
m_dp_2c <- glm(chose_opp_target ~ prob_difference + my_ctw + opp_ctw,
               data = dp_prog,
               family = binomial(link = "logit"), method = brglm2::brglmFit)
dm_my  <- mean(dp_prog$my_ctw)
dm_opp <- mean(dp_prog$opp_ctw)
nd <- data.frame(rel_progress = rp_grid) %>%
  mutate(s = (rel_progress - (dm_my - dm_opp)) / 2,
         my_ctw = dm_my + s, opp_ctw = dm_opp - s,
         prob_difference = mean(dp_prog$prob_difference))
pr <- predict(m_dp_2c, newdata = nd, type = "link", se.fit = TRUE)
prog_pred <- bind_rows(prog_pred,
  bind_cols(nd["rel_progress"], plogis_ci(pr$fit, pr$se.fit)) %>%
    mutate(agent_label = dp_label))

prog_pred <- prog_pred %>%
  mutate(agent_label = factor(agent_label, levels = pred_levels))

p_panel_prog <- ggplot(prog_pred, aes(x = rel_progress, y = prob,
                                      colour = agent_label, fill = agent_label,
                                      linetype = agent_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = -2.5, y = 0.97, label = "Ahead", size = 5.3, colour = "grey40") +
  annotate("text", x = 2.5, y = 0.97, label = "Behind", size = 5.3, colour = "grey40") +
  scale_x_continuous(breaks = seq(-4, 4, 2)) +
  scale_y_percent(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_colour_manual(values = curve_colors, guide = "none") +
  scale_fill_manual(values = curve_colors, guide = "none") +
  scale_linetype_manual(values = curve_linetypes, guide = "none") +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(title = "Blocking in 'My vs. Opp Target'",
       x = "Own - opp cards to win",
       y = "P(block opponent)") +
  theme_tom() + fig_theme

p_panel_win <- p_win +
  labs(title = "Win Rate", x = NULL) +
  scale_x_discrete(labels = c("SFT Llama-3.1-8B" = "SFT 8B",
                              "Base Llama-3.1-8B" = "Base 8B"),
                   guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14))

legend_bottom <- get_legend(
  p_panel_curve +
    guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1),
           linetype = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

combined_fig <- plot_grid(
  plot_grid(p_panel_curve, p_panel_prog, p_panel_win,
            nrow = 1, labels = c("A", "B", "C"), align = "h"),
  legend_bottom, ncol = 1, rel_heights = c(1, 0.12))
print(combined_fig)
ggsave("plot_combined_neurips.png", combined_fig, width = 14, height = 4.8,
       dpi = 300, bg = "white")
ggsave("plot_combined_neurips.pdf", combined_fig, width = 14, height = 4.8,
       bg = "white", device = grDevices::cairo_pdf)
cat("Saved: plot_combined_neurips.png + .pdf\n")


sink(); close(.log_con)
cat(sprintf("Console log written to %s\n", LOG_FILE))