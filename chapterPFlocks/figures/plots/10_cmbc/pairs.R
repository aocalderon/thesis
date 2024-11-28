library(tidyverse)
library(latex2exp)
library(ggpubr)

prefix = "cell187_"
suffix = ""
fields <- c("ts","t","host","tag","n_prime","AppId","x","dataset","epsilon","mu","delta","method","total","n")
pairs <- enframe(read_lines("cmbc_cell187.txt"), value = "line") |>
  filter(str_detect(line, 'INFO')) |>
  filter(str_detect(line, 'Pairs')) |>
  separate(col = line, into = fields, sep = "\\|") |>
  select(dataset, epsilon, n) |>
  mutate(n = as.numeric(n), epsilon = as.numeric(epsilon)) |>
  group_by(dataset, epsilon) |> summarise(n = max(n))

times <- read_tsv("cell187.tsv", col_names = T)

data <- pairs |> inner_join(times, by = join_by(dataset, epsilon))

data_prime <- data |> filter(dataset == paste0(prefix, "500", suffix))
p500 = ggplot(data_prime, aes(x = as.factor(n), y = time, group = method)) +
  geom_line(aes(linetype = method, color = method)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  labs(title="(a) 500", x = "Number of pairs", y = "Time(s)") +
  scale_color_discrete("Method") +
  scale_shape_discrete("Method") +
  guides(linetype = "none") +
  theme_bw()

data_prime <- data |> filter(dataset == paste0(prefix, "2K", suffix))
p1K = ggplot(data_prime, aes(x = as.factor(n), y = time, group = method)) +
  geom_line(aes(linetype = method, color = method)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  labs(title="(b) 1K", x = "Number of pairs", y = "Time(s)") +
  scale_color_discrete("Method") +
  scale_shape_discrete("Method") +
  guides(linetype = "none") +
  theme_bw()

data_prime <- data |> filter(dataset == paste0(prefix, "4K", suffix))
p2K = ggplot(data_prime, aes(x = as.factor(n), y = time, group = method)) +
  geom_line(aes(linetype = method, color = method)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  labs(title="(c) 2K", x = "Number of pairs", y = "Time(s)") +
  scale_color_discrete("Method") +
  scale_shape_discrete("Method") +
  guides(linetype = "none") +
  theme_bw()

data_prime <- data |> filter(dataset == paste0(prefix, "6K", suffix))
p4K = ggplot(data_prime, aes(x = as.factor(n), y = time, group = method)) +
  geom_line(aes(linetype = method, color = method)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  labs(title="(d) 4K", x = "Number of pairs", y = "Time(s)") +
  scale_color_discrete("Method") +
  scale_shape_discrete("Method") +
  guides(linetype = "none") +
  theme_bw()

data_prime <- data |> filter(dataset == paste0(prefix, "8K", suffix))
p6K = ggplot(data_prime, aes(x = as.factor(n), y = time, group = method)) +
  geom_line(aes(linetype = method, color = method)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  labs(title="(e) 6K", x = "Number of pairs", y = "Time(s)") +
  scale_color_discrete("Method") +
  scale_shape_discrete("Method") +
  guides(linetype = "none") +
  theme_bw()

ggarrange(p500, p1K, p2K, p4K, p6K, ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(paste0("cmbc_pairs.pdf"), width = 10, height = 6)
