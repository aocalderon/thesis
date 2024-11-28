library(tidyverse)
library(ggpubr)

W = 8
H = 6

#data_500 <- read_tsv("cmbc_psi_stages.tsv") |> filter(variant != "HASH")
#data_500 |> write_tsv("data_500.tsv", col_names = F)

data_500 <- read_tsv("data_500.tsv", col_names = c("variant", "stage", "epsilon", "time"))

labels = c("Collect","Each","MBC","Cliques","Maximals","Candidates","Centers","Pairs")
p = ggplot(data = data_500, aes(x = variant, y = time, fill = stage)) +
  geom_bar(stat="identity", position="stack") +
  labs(title="(a)", x="Variant", y="Time(s)") +
  theme_bw() + 
  scale_fill_brewer(name = "Stage", palette = "Paired", labels = labels) 
plot(p)
ggsave(paste0("cmbc_500_psi.pdf"), width = W, height = H)

fields <- c("ts","t","host","tag","n_prime","AppId","x","dataset","epsilon","mu","delta","variant","stage","time")
# data_2K <- enframe(read_lines("cmbc_cell187.txt"), value = "line") |>
#   filter(str_detect(line, 'TIME')) |>
#   separate(col = line, into = fields, sep = "\\|") |>
#   filter(dataset == "cell187_4K") |>
#   select(variant, stage, epsilon, time) |>
#   mutate(time = as.numeric(time), epsilon = as.numeric(epsilon), stage = str_trim(stage)) |>
#   filter(epsilon == 20) |>
#   filter(str_trim(stage) != "Total") |>
#   group_by(variant, stage, epsilon) |> summarise(time = mean(time))
# data_2K |> write_tsv("data_2K.tsv", col_names = F)

data_2K <- read_tsv("data_2K.tsv", col_names = c("variant", "stage", "epsilon", "time"))

labels = c("Collect","Each","MBC","Cliques","Maximals","Candidates","Centers","Pairs")
q = ggplot(data = data_2K, aes(x = variant, y = time, fill = stage)) +
  geom_bar(stat="identity", position="stack") +
  labs(title="(b)", x="Variant", y="Time(s)") +
  theme_bw() + 
  scale_fill_brewer(name = "Stage", palette = "Paired", labels = labels) 
plot(q)
ggsave(paste0("cmbc_2K_psi.pdf"), width = W, height = H)

ggarrange(p, q, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(paste0("cmbc2.pdf"), width = 10, height = H)
