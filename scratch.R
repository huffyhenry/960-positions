library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(cmdstanr)

data <- read_parquet("compact_data.parquet") %>%
  select(FEN, Result) %>%
  filter(FEN != "?", Result != "*") %>%
  group_by(FEN, Result) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  pivot_wider(names_from=Result, values_from=n) %>%
  rename(wins=`1-0`, draws=`1/2-1/2`, losses=`0-1`) %>%
  mutate(N=wins+draws+losses)

gc()

model <- cmdstan_model("960-positions.stan")

fit <- model$sample(
  data=list(wins=data$wins, draws=data$draws, losses=data$losses),
  chains=2,
  parallel_chains=2,
  iter_sampling=2000,
  iter_warmup=2000,
  refresh=10
)

fit$cmdstan_diagnose()

s <- fit$summary()

res <- s %>%
  filter(grepl("rates", variable)) %>%
  mutate(coords=str_extract(variable, "[0-9]+,[0-9]+")) %>%
  separate(coords, c("fen_idx", "res_idx"), convert=TRUE) %>% 
  mutate(result=case_when(res_idx == 1 ~ "win", res_idx == 2 ~ "draw", res_idx == 3 ~ "loss")) %>%
  select(-variable, -rhat, -ess_bulk, -ess_tail, -sd, -mad, -res_idx) %>%
  pivot_longer(c(mean, median, q5, q95), names_to="estimand", values_to="estimate") %>%
  # Insert estimates of expected points
  bind_rows(
    filter(s, grepl("xp", variable)) %>%
      select(mean, median, q5, q95) %>%
      mutate(result="xp", fen_idx=seq(1, 960, 1)) %>%
      pivot_longer(c(mean, median, q5, q95), names_to="estimand", values_to="estimate")
  ) %>%
  pivot_wider(names_from=c(estimand, result), values_from=estimate) %>%
  arrange(fen_idx) %>%
  bind_cols(data)

res %>%
  arrange(desc(mean_xp)) %>%
  ggplot(aes(y=reorder(as.factor(fen_idx), mean_xp))) +
  geom_vline(aes(xintercept=0.5104987), linetype="dotted") +
  geom_errorbarh(aes(xmin=q5_xp, xmax=q95_xp), alpha=0.5) +
  geom_point(aes(x=mean_xp)) +
  theme_bw() +
  theme(
    axis.ticks.y=element_blank(), 
    axis.text.y=element_blank()
  ) +
  labs(
    x="Expected points for White w/ 95% CrI", 
    y=NULL, 
    title="Chess960 starting positions"
  )
  