library(clustcurv)
library(ggfortify)


head(veteran)

fit <- survfit(Surv(time, status) ~ factor(celltype), data = veteran)
autoplot(fit)
survdiff(Surv(time,status)~factor(celltype), data=veteran)
survminer::pairwise_survdiff(Surv(time, status) ~ celltype,
                                    data = veteran, p.adjust.method = "BH") 

?clustcurv_surv
res <- clustcurv_surv(time = veteran$time, status = veteran$status,
                      fac = veteran$celltype, algorithm = "kmeans",
                      nboot = 100,
                      cluster = TRUE, seed = 29072016)
res

autoplot(res, groups_by_colour = TRUE, xlab = "Time (in days)")










colonCSm <- na.omit(data.frame(time = colonCS$Stime, status = colonCS$event,
                       nodes = colonCS$nodes))
table(colonCSm$nodes)
# deleting people with zero nodes
colonCSm <- colonCSm[-c(which(colonCSm$nodes == 0)), ]
table(colonCSm$nodes)

# grouping people with more than 10 nodes
colonCSm$nodes[colonCSm$nodes >= 10] <- 10
table(colonCSm$nodes)  # 10 levels


model <- survfit(Surv(time, status) ~ factor(nodes), data = colonCSm)
survdiff(Surv(time,status)~factor(nodes), data = colonCSm)


survminer::pairwise_survdiff(Surv(time, status) ~ nodes,
                                    data = colonCSm, p.adjust.method = "BH")



res <- clustcurv_surv(time = colonCSm$time, status = colonCSm$status,
                      fac = colonCSm$nodes, algorithm = "kmeans",
                      nboot = 100, cluster = TRUE, seed = 300716)

autoplot(res, groups_by_colour = FALSE, xlab = "Time (in days)")

autoplot(res, groups_by_colour = TRUE, xlab = "Time (in days)")


res$table
data.frame(res$levels, res$cluster)

