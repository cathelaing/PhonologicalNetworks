

speaker.data.sessions <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  rename("n Sessions" = "n")

speaker.data.sessions.mean <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(mean_sessions = mean(n))

speaker.data.sessions.sd <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  ungroup() %>%
  group_by(corpus) %>%
    #rename("n Sessions" = "n") %>%
  summarise(sd_sessions = sd(n))

speaker.data.overview <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(`Min. age` = min(age)) %>%
  left_join(speaker.data.sessions) %>%
  arrange(corpus)

speaker.data.overview.mean <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(min_age = min(age)) %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(mean_age = mean(min_age))

speaker.data.overview.sd <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(min_age = min(age)) %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(sd_age = sd(min_age))

table.data.summary.mean <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  #rename("Types" = "n") %>%
  #spread(corpus, Types) %>%
  summarise(Types = mean(n, na.rm=T))

table.data.summary.sd <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  #rename("Types" = "n") %>%
  #spread(corpus, Types) %>%
  summarise(Types = sd(n, na.rm=T))

overview.sds <- speaker.data.overview.sd %>%
  left_join(speaker.data.sessions.sd) %>%
  left_join(table.data.summary.sd) %>%
  mutate("Speaker" = "SD") %>%
  rename("Min. age" = "sd_age",
         "n Sessions" = "sd_sessions")

overview.means <- speaker.data.overview.mean %>%
  left_join(speaker.data.sessions.mean) %>%
  left_join(table.data.summary.mean) %>%
    mutate("Speaker" = "Mean") %>%
  rename("Min. age" = "mean_age",
         "n Sessions" = "mean_sessions") %>%
  bind_rows(overview.sds)

### now create means and sds for full corpus

full.data.summary.mean.typ <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(Types = mean(n, na.rm=T)) %>%
  mutate("Speaker" = "Mean")

full.data.summary.sd.typ <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(Types = sd(n, na.rm=T)) %>%
  mutate("Speaker" = "SD")

full.data.summary.mean <- speaker.data.overview %>%
  ungroup() %>%
  summarise(`Min. age` = mean(`Min. age`),
            `n Sessions` = mean(`n Sessions`)) %>%
  mutate("Speaker" = "Mean") %>%
  left_join(full.data.summary.mean.typ)

full.data.summary.sd <- speaker.data.overview %>%
  ungroup() %>%
  summarise(`Min. age` = sd(`Min. age`),
            `n Sessions` = sd(`n Sessions`)) %>%
  mutate("Speaker" = "SD") %>%
  left_join(full.data.summary.sd.typ)

full.data.summary <- full.data.summary.mean %>%
  bind_rows(full.data.summary.sd) %>%
  mutate(Corpus = "All")

## bind all dataframes together

table.data.overview <- regression_data %>%
  group_by(Speaker, corpus) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally() %>% 
  rename("Types" = "n") %>%
  left_join(speaker.data.overview) %>%
  dplyr::select(Speaker, corpus, `Min. age`, `n Sessions`, Types) %>%
  bind_rows(overview.means) %>%
  rename("Corpus" = "corpus") %>%
  mutate(Corpus = fct_recode(Corpus,
                             "French" = "Lyon",
                             "English" = "Providence")) %>%
  arrange(Corpus) %>%
  bind_rows(full.data.summary)
  

table.aop.deg.corr.speaker <- globalthresholds_AOP %>%
  group_by(Speaker, corpus) %>%
  summarize(rho = stats::cor.test(AOP, degree, method = "sp")$estimate,
            pval = stats::cor.test(AOP, degree, method = "sp")$p.value
  ) %>%
  ungroup() %>%
  arrange(corpus) %>%
  rename("Corpus" = "corpus",
         "p" = "pval") %>%
  mutate(p = scales::pvalue(p))


