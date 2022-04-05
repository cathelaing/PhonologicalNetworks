# Figures for Phonological Networks in Development paper


# thresholds.plot <- ggplot(globalthresholds_corr, aes(x = threshold, y = as.numeric(estimate), colour = data_type, fill = data_type)) +
#   geom_point(shape = 21, size = 3, aes(alpha = ifelse(p.value <= .001, .8, .3)), 
#              position = position_jitter(.01)) +
#   geom_line(linetype = "dashed") +
#   ylab("Spearman's rho") +
#   ylim(-.3, .3) +
#   #ggtitle("Lyon (French)") +
#   scale_alpha(guide = 'none') +
#   theme_bw() +
#   facet_wrap(~corpus, ncol = 2)

distance_threshold <- ggplot(global_distance, aes(distance_norm, fill = data_type, colour = data_type)) +
  geom_density(alpha = 0.1, size = 2) +
  geom_vline(xintercept = 0.25, linetype="dotted", 
             size=1.5) +
  xlab("Phonological distance (z-score)") +
  facet_wrap(~corpus, ncol=2) +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = c(.90, .85))

cor_deg_AOP.fig <- ggplot(globalthresholds_AOP, 
                        aes(x = AOP, y = degree_z, colour = data_type)) +
   geom_rect(data = subset(globalthresholds_AOP ,corpus == 'Lyon'), aes(fill = corpus),xmin = -Inf,xmax = Inf,
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

PAT_data_type <- ggplot(subset(regression_data, age == (AOP-1)), aes(x=data_type, y=PAT_val, color = Speaker))+
  geom_point(size = 5, shape = 1, position=position_dodge(-.2)) +
  geom_line(aes(group=Speaker), colour='grey63', position = position_nudge(x = -.05), linetype=6) +
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", aes(fill = Speaker), shape = 24,
               size=1.5, position = position_dodge(-.2), colour = "grey32", show.legend = FALSE) +
  #stat_summary(fun.y = mean, aes(group=Speaker, colour=Speaker), geom='line', size=.8, position = position_dodge(-.2), show.legend = FALSE) +
  #scale_shape_manual(values=c(23,25)) +
  #xlab("Consonant Type") +
  #ylab('% Congruent CPs') +
  #ggtitle("Caregiver Prompt") +
  # scale_colour_discrete(breaks=c("noVMS", "withVMS"),
  #                       labels = c("no-VMS", "with-VMS")) +
  theme_bw(base_size=15) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size=20),
        legend.title = element_blank(),
        legend.justification=c(1,1), legend.position=c(1,.99)) +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape=15))) +
  facet_wrap(~corpus, ncol = 2)

static_preds_all_LN$corpus <-  factor(static_preds_all_LN$corpus, 
                   labels = c("French", "US-English"))


# plot_agg_LN <- ggplot(subset(static_preds_all_LN, predictor %in% c("PAT_scaled", "PAQ_scaled_target", "length_scaled", "freq_scaled")), 
#                       aes(x = predictor, y = Estimate, shape = measure)) +
#   geom_pointrange(aes(ymin = X2.5.., ymax = X97.5.., y = Estimate, col = predictor),
#                   position = position_dodge(width = .4),
#                   size = .8) +
#   facet_wrap(~measure)  +
#   scale_x_discrete(limits = c("PAT_scaled", "PAQ_scaled_target", "length_scaled", "freq_scaled"),
#                    labels = c("PAT", "PAQ", "Length", "Freq")) +
#   coord_flip() +
#   xlab("Predictor") +
#   geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
#   guides(colour="none") +
#   scale_colour_solarized() +
#   theme_bw(
#     #base_size = 18
#     ) +
#   theme(aspect.ratio = 0.7,
#         legend.title = element_blank(),
#         strip.text = element_text(size=9),
#         strip.background = element_rect(colour="black",size=1),
#         strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
#   facet_wrap(~corpus, ncol= 2, scales = "free")

even_indexes<-seq(2, 30, 2)

dat_comp <- ggplot(subset(regression_data, age == AOP-1 & age %in% even_indexes), 
       aes(x = PAT_scaled, y = (as.factor(AOP)), fill = data_type)) +
  stat_density_ridges(#binwidth = 4,
                      alpha = 0.3,
                      aes(linetype = data_type),
                      lwd = 1,
                      quantile_lines = TRUE,
                      quantiles = 2) +
  scale_fill_manual(values = wes_palette(name = "Moonrise2", type = "continuous")) +
  xlab("PAT values (z-score)") +
  ylab("Age of Production") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~corpus, ncol=2, scales = "free")

  

# Small world networks

ER_edit <- globalsmallworlddata %>%
  filter(data_type %in% c("Erdos_Renyi", "target", "actual")) %>%
  mutate(sample = 1)

globalsmallworlddata_ER <- globalsmallworlddata %>%
  mutate(sample = 2) %>%
  bind_rows(ER_edit) %>%
  mutate(real_sim = ifelse(data_type %in% c("actual", "target"), "real", "simulated"),
         act_tar = ifelse(sample == 1, "Actual", "Target"),
         pairing = ifelse(act_tar == "Actual", "Actual vs. Erdos-Renyi", "Target vs. Erdos-Renyi")) %>%
  dplyr::select(-sample)

globalsmallworlddata_WS <- globalsmallworlddata %>%
  mutate(real_sim = ifelse(data_type %in% c("actual", "target"), "real", "simulated"),
         act_tar = ifelse(data_type %in% c("actual", "WS_actual"), "Actual", "Target"),
         pairing = ifelse(data_type %in% c("actual", "WS_actual"), "Actual vs. Watts-Strogatz", NA),
         pairing = ifelse(data_type %in% c("target", "WS_target"), "Target vs. Watts-Strogatz", pairing)) %>%
  bind_rows(globalsmallworlddata_ER)

# pairing_types <- c(
#   "Actual vs. Watts-Strogatz" = "Watts-Strogatz",
#   "Target vs. Watts-Strogatz" = "Watts-Strogatz",
#   "Actual vs. Erdos-Renyi" = "Erdos-Renyi", 
#   "Target vs. Erdos-Renyi" = "Erdos-Renyi",
#   "French" = "French",
#   "English" = "English"
#   )

path_length_all <- ggplot(data = transform(subset(globalsmallworlddata_WS, !is.na(pairing)), 
                                          pairing=factor(pairing,levels=c(
                                            "Actual vs. Watts-Strogatz",
                                            "Target vs. Watts-Strogatz",
                                            "Actual vs. Erdos-Renyi",
                                            "Target vs. Erdos-Renyi"))),
                           aes(x = age, y = path_length, colour = real_sim)) +
  # geom_rect(data = transform(subset(globalsmallworlddata_WS, !is.na(pairing) & act_tar == "Target"), 
  #                                   pairing=factor(pairing,levels=c(
  #                                     "Actual vs. Watts-Strogatz",
  #                                     "Target vs. Watts-Strogatz",
  #                                     "Actual vs. Erdos-Renyi",
  #                                     "Target vs. Erdos-Renyi"))), aes(fill = act_tar),xmin = -Inf,xmax = Inf,
  #           ymin = -Inf,ymax = Inf,alpha = 0.05, fill = "gray88") +
  geom_smooth(size = 2) +
  scale_colour_discrete(labels = c("Real", "Simulated")) + 
  #ylim(0, 2.5) +
  xlab("Age (months)") +
  ylab("Mean Path Length") +
  guides(colour=guide_legend(title="Data type")) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  facet_grid(pairing ~ corpus, scales = "free"#, labeller = as_labeller(pairing_types)
             )

# Clustering coefficient

clust_coef_all <- ggplot(data = transform(subset(globalsmallworlddata_WS, !is.na(pairing)), 
                                           pairing=factor(pairing,levels=c(
                                             "Actual vs. Watts-Strogatz",
                                             "Target vs. Watts-Strogatz",
                                             "Actual vs. Erdos-Renyi",
                                             "Target vs. Erdos-Renyi"))),
                          aes(x = age, y = clust_coef_avg, colour = real_sim)) +
  # geom_rect(data = transform(subset(globalsmallworlddata_WS, !is.na(pairing) & act_tar == "Target"), 
  #                                   pairing=factor(pairing,levels=c(
  #                                     "Actual vs. Watts-Strogatz",
  #                                     "Target vs. Watts-Strogatz",
  #                                     "Actual vs. Erdos-Renyi",
  #                                     "Target vs. Erdos-Renyi"))), aes(fill = act_tar),xmin = -Inf,xmax = Inf,
  #           ymin = -Inf,ymax = Inf,alpha = 0.05, fill = "gray88") +
  geom_smooth(size = 2) +
  scale_colour_discrete(labels = c("Real", "Simulated")) + 
  #ylim(0, 2.5) +
  xlab("Age (months)") +
  ylab("Average Clustering Coefficient") +
  guides(colour=guide_legend(title="Data type")) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  facet_grid(pairing ~ corpus, scales = "free"#, labeller = as_labeller(pairing_types)
  )

