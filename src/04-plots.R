summary.dt[, .(device_id,avg.vec,sd.vec,steps_bin,peaks.per.sec,avg.period,sd.period,avg.amp,sd.amp)] %>% 
  melt.data.table(id.vars = c("device_id", "steps_bin")) %>%
  ggplot(aes(x = device_id, y = value)) + 
  geom_boxplot() +
  facet_grid(variable ~ steps_bin, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_bw() 
  
scaled.summary[, .(device_id,avg.vec,sd.vec,steps_bin,peaks.per.sec,avg.period,sd.period,avg.amp,sd.amp)] %>% 
  melt.data.table(id.vars = c("device_id", "steps_bin")) %>%
  ggplot(aes(x = device_id, y = value)) + 
  geom_boxplot() +
  facet_grid(variable ~ steps_bin, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_bw() 