# Figures for Phonological Networks in Development paper

cor_deg_AOP.fig <- ggplot(globalthresholds_AOP,
                        aes(x = AOP, y = degree, colour = data_type)) +
   geom_rect(data = subset(globalthresholds_AOP ,corpus == 'French'), aes(fill = corpus),xmin = -Inf,xmax = Inf,
             ymin = -Inf,ymax = Inf,alpha = 0.08, fill = "gray88") +
  geom_point(shape = 21, size = 1, alpha = 0.5, position = position_jitter(.02)) +
  scale_x_continuous(breaks = seq(from = 10, to = 30, by = 10)) +
  # scale_x_discrete(breaks=NULL) +
  # scale_y_discrete(breaks=NULL) +
  geom_smooth(method=lm,
              se=FALSE, size = 2) +
  #ggtitle("AOP ~ degree, E=.25") +
  ylab("degree (z-score)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.position = "top",
        strip.text = element_text(size=9),
        strip.background = element_rect(colour="black",size=1),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
  facet_wrap(~Speaker, ncol=3)

plot(cor_deg_AOP.fig)

# Data type comparisons

data_type_plot <- ggplot(data = subset(regression_data, age == (AOP-1)), aes(x = age, y = PAT_scaled, colour = data_type)) +
  geom_point(shape = 21, position = position_jitter(.1)) +
  geom_smooth(aes(fill = data_type)) +
  ylab("PAT value (normalised)") +
  xlab("Age (months)") +
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank())
