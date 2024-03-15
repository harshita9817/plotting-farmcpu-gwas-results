

df=read.csv("Zbluessignals.csv")
data_cum <- df %>%
  group_by(CHROM) %>%
  summarise(max_bp = max(POS)) %>%
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>%
  select(CHROM, bp_add)

gwas_data <- df %>%
  inner_join(data_cum, by = "CHROM") %>%
  mutate(bp_cum = POS + bp_add)
axis_set <- gwas_data %>%
  group_by(CHROM) %>%
  summarize(center = mean(bp_cum))

ggplot(gwas_data, aes(x = bp_cum, y = support)) + 
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd=0.2)+
  geom_point(alpha = 5, size=3) +
  geom_hline(yintercept = 0.1, color = "black", linetype = "dashed", lwd=1)+
  scale_x_continuous(breaks = axis_set$center, n.breaks = 2000,
                     label=c("Chr01", "Chr02", "Chr03","Chr04", "Chr05", "Chr06","Chr07", "Chr08", "Chr09", "Chr10"))
#######################################################################
ggplot(gwas_data, aes(x = bp_cum, y = support)) + 
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd = 0.1) +
  geom_point(aes(color = odd_even_chr), alpha = 0.5, size = 5) +  # Increased size to 5
  scale_color_manual(values = c("Even" = "purple", "Odd" = "darkgreen")) +
  guides(color = FALSE) +  # This line removes the legend
  geom_hline(yintercept = 0.1, color = "black", linetype = "dashed", lwd = 1) +
  scale_x_continuous(breaks = axis_set$center, n.breaks = 2000,
                     labels = c("Chr01", "Chr02", "Chr03", "Chr04", "Chr05", "Chr06", "Chr07", "Chr08", "Chr09", "Chr10"))
