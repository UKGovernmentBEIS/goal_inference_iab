# Hidden-goal card game: trial-level dataset and analysis

Analysis and data accompanying our NeurIPS paper on goal
inference and blocking in a two-player hidden-goal card game. It contains the
anonymised trial-level behavioural data for humans, LLMs, fine-tuned local
models, and a near-optimal dynamic-programming (DP) decision model benchmark, together with the
R analysis that produces the statistics and combined figure reported in the
paper.

## Data

All files are in `results/`. Human participants are identified only by
pseudonymous labels (`P001`–`P376`); no
personal information, free-text responses, prompts, or model generations are
included.

### Ethics and consent

The human study passed Responsible Research
Framework (RRF) review. Participants gave informed consent, were compensated
for their time, and consented to the sharing of anonymised data.

### `trials_neurips.csv` — 36,766 rows, one per card-choice trial

| column | description |
|---|---|
| `model_type` | Acting agent: `gpt-4o-2024-08-06`, `gpt-5-6-sol`, `qwen3-8-2-4t-a95b`, `sft-8b` (fine-tuned Llama-3.1-8B), `base-8b` (base Llama-3.1-8B), or `human` |
| `opponent_type` | Scripted opponent policy: `greedy` or `oppcost` |
| `unique_game_id` | Game identifier |
| `cluster` | Clustering unit for mixed models: participant label for humans, game id otherwise |
| `trial_type` | Trial classification (based on what the agent can observe from game history, not ground truth) e.g. `My target vs neutral`, `Opponent target vs neutral`, `Two opp targets (exclusive)` |
| `chosen_card_type`, `card1_type_belief`, `card2_type_belief` | Card classification (as above) `mine_only`, `opp_only`, `both`, `neutral` |
| `chosen_prob`, `not_chosen_prob` | P(card helps the opponent) under the ideal-observer belief model, for the chosen / unchosen card |
| `card1_prob_helps_opp`, `card2_prob_helps_opp` | The same probabilities by card position |
| `my_ctw`, `opp_ctw` | Cards-to-win remaining for the acting player / opponent |

### `games_neurips.csv` — 2,991 rows, one per game

`unique_game_id`, `opponent_type`, `P1_win` (1 = acting player won),
`agent_type`, and `participant` (anonymized label; `NA` for non-human agents).

### `dp_trials_neurips.csv` — 3,995 rows

Same schema as `trials_neurips.csv` (without `cluster`) for the
dynamic-programming solver benchmark (`model_type = "dp"`); value-tied trials
are excluded.

## Reproducing the analysis

```sh
Rscript analysis_consolidated_neurips.R > analysis_consolidated_neurips_log.txt
```

This reads the three CSVs from `results/`, prints the analysis results to the .txt file, and writes the combined figure
(`plot_combined_neurips.png` / `.pdf`).

### Requirements

R 4.6.2 with the following package versions

```r
install.packages("remotes")
remotes::install_version("afex",    "1.5.1")
remotes::install_version("lme4",    "2.1.0")
remotes::install_version("dplyr",   "1.2.1")
remotes::install_version("emmeans", "2.0.4")
remotes::install_version("car",     "3.1.5")
remotes::install_version("ggplot2", "4.0.3")
remotes::install_version("cowplot", "1.2.0")
remotes::install_version("brglm2",  "1.1.1")
```

## License

Code: MIT — see [LICENSE](LICENSE).

Data (the CSVs in `results/`): [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
© Crown copyright (AI Security Institute), 2026.
