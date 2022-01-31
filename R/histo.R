library(tidyverse)
library(patchwork)

# # First version
# colors <- c(
#   "GER" = "#999999",
#   "CHA" = colorblindr::palette_OkabeIto[2],
#   "REG" = colorblindr::palette_OkabeIto[3]
# )

# Second version
colors <- c(
  "GER" = "#8B0000",
  "CHA" = "#FF8000",
  "UHREG" = "#808A87"
)

df <- read_csv("data/cha_germany.csv") %>%
  rename(age = TSCHQ_q01_age) %>%
  bind_rows(
    read_csv("data/uhreg_germany.csv")
  ) %>%
  mutate(center = ifelse(center == "REG", "UHREG", center))

df

df_cutpoints <-
  tibble(
    cut = c(-Inf, seq(5, 100, 5), Inf),
  ) %>%
  rowid_to_column(var = "cutno")


ger <- read_csv("data/count_data_age_GER.csv") %>%
  pivot_longer(
    everything(), 
    names_to = "age", values_to = "count"
  ) %>%
  map_dfc(as.integer) %>%
  group_by(age) %>%
  expand(count = 1:count) %>%
  ungroup() %>%
  mutate(group = "GER") %>%
  select(group, age)


df_combined <-
  df %>%
  select(group = center, age, perc = percentage_tinnitus) %>%
  mutate(count = perc * 1000) %>%
  group_by(group, age) %>%
  expand(count = 1:count) %>%
  ungroup() %>%
  select(group, age) %>%
  bind_rows(ger) 

df_plot <- df_combined %>%
  mutate(age_cut = cut(age, breaks = df_cutpoints$cut)) %>%
  # mutate(age_cut_no = as.integer(age_cut)) %>%
  add_count(group, name = "group_count") %>%
  group_by(group, age_cut) %>%
  summarize(perc = n() / group_count[1], .groups = "drop")


df_uhreg_ger <- df_plot %>%
  filter(group %in% c("UHREG", "GER"))

p1 <- ggplot(df_uhreg_ger, aes(y = age_cut, fill = group)) +
  scale_x_continuous(
    breaks = seq(-0.15, 0.05, 0.05), 
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +
  scale_y_discrete(labels = paste0("[", c(0, seq(6,96, 5)), ",", seq(5,100,5), "]")) +
  geom_col(aes(x = ifelse(group == "GER", perc, -1*perc)), alpha = 0.6) +
  geom_vline(xintercept = 0, size = 0.25) +
  annotate(
    "text", x = -0.025, y = nlevels(df_uhreg_ger$age_cut) - 2, 
    label = "UHREG tinnitus patients",
    hjust = 1, size = 0.9 * 8/.pt,
    lineheight = 0.875
  ) +
  annotate(
    "text", x = 0.025, y = nlevels(df_uhreg_ger$age_cut) - 2, 
    label = "German\npopulation",
    hjust = 0, size = 0.9 * 8/.pt,
    lineheight = 0.875
  ) +
  labs(x = "Proportion", y = "Age", caption = "(a)") +
  scale_fill_manual(values = colors) +
  guides(fill = "none") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(axis.ticks = element_line()) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold")) #+
  # theme(plot.margin = margin(0.1,0,0.2,0.1,"mm"))
p1
ggsave("figures/age-reg-ger.png", width = 8, height = 6.5, units = "cm", dpi = 600, bg = "white")

df_cha_ger <- df_plot %>%
  filter(group %in% c("CHA", "GER"))

p2 <- ggplot(df_cha_ger, aes(y = age_cut, fill = group)) +
  scale_x_continuous(
    breaks = seq(-0.15, 0.05, 0.05), 
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +
  scale_y_discrete(labels = paste0(c(0, seq(6,96, 5)), "-", seq(5,100,5))) +
  geom_col(aes(x = ifelse(group == "GER", perc, -1*perc)), alpha = 0.6) +
  geom_vline(xintercept = 0, size = 0.25) +
  annotate(
    "text", x = -0.025, y = nlevels(df_uhreg_ger$age_cut) - 2, 
    label = "CHA tinnitus patients",
    hjust = 1, size = 0.9 * 8/.pt,
    lineheight = 0.875
  ) +
  annotate(
    "text", x = 0.025, y = nlevels(df_uhreg_ger$age_cut) - 2, 
    label = "German\npopulation",
    hjust = 0, size = 0.9 * 8/.pt,
    lineheight = 0.875
  ) +
  labs(x = "Proportion", y = "Age", caption = "(b)") +
  scale_fill_manual(values = colors) +
  guides(fill = "none") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(axis.ticks = element_line()) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold")) #+
  # theme(plot.margin = margin(0.1,0,0.2,0.1,"mm"))
p2
ggsave("figures/age-cha-ger.png", width = 8, height = 6.5, units = "cm", dpi = 600, bg = "white")


p3 <- ggplot(df_combined, aes(x = age, color = group)) +
  scale_x_continuous(breaks = seq(0,100,10), expand = c(0,0)) +
  scale_y_continuous(expand = c(0.01,0)) +
  geom_density(adjust = 2, size = 0.5, alpha = 0.6) +
  scale_color_manual(values = colors) +
  guides(color = "none") +
  annotate(
    "text", x = 47, y = 0.032, label = "CHA", 
    color = colors[["CHA"]], hjust = 1,
    size = 0.9 * 8/.pt
  ) +
  annotate(
    "text", x = 62, y = 0.032, label = "UHREG", 
    color = colors[["UHREG"]], hjust = 0,
    size = 0.9 * 8/.pt
  ) +
  annotate(
    "text", x = 12, y = 0.013, label = "Germany", 
    color = colors[["GER"]],
    size = 0.9 * 8/.pt
  ) +
  labs(x = "Age", y = "Kernel density estimate", caption = "(c)") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(axis.ticks.x = element_line()) +
  theme(axis.text.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))
p3
ggsave("figures/age-density.png", width = 16, height = 4, units = "cm", dpi = 600, bg = "white")

(p1 | p2) / p3 + 
  patchwork::plot_layout(height = c(7, 3)) +
  plot_annotation(theme = theme(plot.margin = margin()))
ggsave("figures/age-composite.png", width = 16, height = 10, units = "cm", dpi = 600, bg = "white")
ggsave("figures/age-composite.pdf", width = 16, height = 10, units = "cm", device = cairo_pdf, bg = "white")
