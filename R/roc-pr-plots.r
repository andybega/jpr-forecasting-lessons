# ROC/PR plots for calib and test, also forecast map


#   6-month rolling ROC
#   _____________________

df <- rbind(
  cbind(partition="Calib", rocdf(ebma, y, pr_calib, type="roc")),
  cbind(partition="Test 6", rocdf(ebma, y, pr_test6, type="roc"))
)

p1 <- ggplot(data=df, aes(x=tpr, y=fpr, col=partition)) +
  geom_line(show_guide=TRUE, alpha=0.7) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 1)) +
  geom_abline(intercept=0, slope=1, col="gray50", alpha=0.5) +
  labs(x="FPR", y="TPR") +
  scale_color_discrete(name="Partition") +
  theme_bw()
p1

ggsave(filename="graphics/roc-curve.png", plot=p1, width=2.9, height=2, units="in",
       dpi=400, scale=1.5)

#   6-month rolling PR
#   _____________________

df <- rbind(
  cbind(partition="Calib", rocdf(ebma, y, pr_calib, type="pr")),
  cbind(partition="Test 6", rocdf(ebma, y, pr_test6, type="pr"))
)

p2 <- ggplot(data=df, aes(x=rec, y=prec, col=partition)) +
  geom_line(show_guide=TRUE, alpha=0.7) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 1)) +
  labs(x="Recall", y="Precision") +
  scale_color_discrete(name="Partition") +
  theme_bw()
p2

# logged y
library(scales)
#p2 <- ggplot(data=df, aes(x=rec, y=prec, col=partition)) +
  geom_line(show_guide=TRUE, alpha=0.7) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l") +
  labs(x="Recall", y="Precision") +
  scale_color_discrete(name="Partition") +
  theme_bw()
p2

ggsave(filename="graphics/pr-curve.png", plot=p2, width=2.9, height=2, units="in",
       dpi=400, scale=1.5)


#   Forecast map
#   _______________

total <- pr_fcast %>%
  group_by(gwcode) %>%
  summarize(
    date = min(date),
    y = max(y),
    i1 = p_agg(i1),
    i2 = p_agg(i2),
    i3 = p_agg(i3),
    i4 = p_agg(i4),
    i5 = p_agg(i5),
    i6 = p_agg(i6),
    i7 = p_agg(i7),
    ebma = p_agg(ebma)
  ) %>%
  arrange(desc(ebma)) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  select(country, gwcode, ebma)

# Scale by 100 for proper coloring
total$scaled <- total$ebma*100

# Plot
dpi <- 300
par(mar=c(0, 0, 0, 0))
png("graphics/fcast-map.png", width=3*dpi, height=1.26*dpi, pointsize=10)
worldMap("scaled", "gwcode", total, legend.title="Percent")
dev.off()


