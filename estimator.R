library(tidyverse)
d = read.csv("result.csv", sep = ";", encoding = "UTF-8", header = F)
names(d) <- c("session","name", "time","curr.time","x.coord","y.coord", "hand")
d$time <- as.numeric(d$time)
d$session <- as.character(d$session)
d$x.coord <- as.numeric(d$x.coord)
d$y.coord <- as.numeric(d$y.coord)
d$hand <- factor(d$hand, levels = c("right","left"))

d$tick <- c(NA, diff(d$time))
d$tick[c(0, diff(d$curr.time)) < 0] <- NA # через дифф ищем смену в текущем времени с 0 на 5, там же сглаживаем провалы в тиках при помощи NA

d$time     <- as.integer(d$time/1000)
d$datetime <- as.POSIXct(d$time, origin="1970-01-01")
d$datetime <- format(d$datetime, format="%H:%M:%S")

##########################################################################################
# Интервал между касаниями
ggplot(d, aes(x=factor(curr.time), y=tick)) +
  geom_violin(scale="width") +
  geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
  geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
  stat_summary(fun.y="median", geom="point", shape=20, col="white") +
  ggtitle("Результаты теппинг-теста") +
  labs(x="Время", y="Интервал между касаниями, мс") +
  facet_grid(hand~name) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) +
  scale_fill_brewer() + geom_jitter(alpha = 0.8) +
  theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0, size=16),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(angle=90, vjust=0, size=16))

sprintf("%.2f±%.2f", mean(d$tick[d$hand=="right"], na.rm = T), sd(d$tick[d$hand=="right"], na.rm = T))
sprintf("%.2f±%.2f", mean(d$tick[d$hand=="left"], na.rm = T),  sd(d$tick[d$hand=="right"], na.rm = T))

##########################################################################################
# Количество касаний в секунду
d %>% group_by(curr.time, hand) %>% summarise(n = n()) -> res

ggplot(res, aes(x=factor(curr.time), y=n)) +
  geom_violin(scale="width") +
  geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
  geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
  stat_summary(fun.y="median", geom="point", shape=20, col="white") +
  ggtitle("Результаты теппинг-теста") +
  labs(x="Время", y="Количество касаний в секунду") +
  facet_grid(hand~.) +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) +
  scale_fill_brewer() + geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0, size=16),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(angle=90, vjust=0, size=16))

sprintf("%.2f±%.2f", mean(res$n[res$hand=="right"], na.rm = T), sd(res$n[res$hand=="right"], na.rm = T))
sprintf("%.2f±%.2f", mean(res$n[res$hand=="left"], na.rm = T),  sd(res$n[res$hand=="right"], na.rm = T))
##########################################################################################
# Коэффициент силы нервной системы (КСНС):
d %>% group_by(hand, curr.time) %>% summarise(n = n(), time = mean(time)) -> res
res %>% summarise(KSNS=(sum(n-n[curr.time==0]))/sum(n[curr.time==0]*100))

##########################################################################################
# Коэффициент функциональной асимметрии по работоспособности левой и правой рук:
d %>% group_by(hand, curr.time) %>% summarise(n = n()) %>% ungroup(hand, curr.time) -> res
res %>% summarise(KFa = (sum(n[hand=="right"])-sum(n[hand=="left"]))/(sum(n[hand=="right"])+sum(n[hand=="left"]))*100)

##########################################################################################
ggplot(d, aes(x = x.coord, y = y.coord, color = hand)) + geom_point() + geom_density_2d()












