# INCIDENCE
# pocty incidencnich zaznamu graf

ggplot(svl2, aes(x=Pocet.incidencnich.zaznamu)) +
  stat_count(width = 0.5, aes(fill = svl2$B.2), colour="grey20", lwd=0.2) +
  labs(x="cetnost", y = "Pocet dotazniku", title="Zaznamy incidence") + 
  geom_text(stat='count',aes(label=..count..), vjust=0)

# Pocet incidenci podle vekovych kohort
ggplot(svl2, aes(x=Pocet.incidencnich.zaznamu, fill=B.5b, ncol = 2)) +
  stat_count(width = 0.9) +
  facet_grid(~ B.5b) + 
  labs(x="cetnost", y = "Pocet dotazniku", title = "Pocet incidenci podle vekovych kohort") +
  theme(legend.position="none")+ 
  geom_text(stat='count',aes(label=..count..))

