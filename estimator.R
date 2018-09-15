remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
library(tidyverse)

do.estimation <- function(filename, autofix = TRUE) {
  d.src = read.csv(filename, sep = ";", encoding = "UTF-8", header = F)
  names(d.src) <- c("session","name", "time","curr.time","duration","x.coord","y.coord", "hand")
  for(x in unique(d.src$name)) {
    if(!all(c("right","left") %in% d.src$hand[d.src$name==x]) & autofix == TRUE) {
      if(length(unique(d.src$session[d.src$name==x]))==2 & all(d.src$hand[d.src$name==x]=="right")) {
        d.src$hand[d.src$session==max(d.src$session[d.src$name==x])] <- "left"
        d.src$session[d.src$session==max(d.src$session[d.src$name==x])] <- min(d.src$session[d.src$name==x])
        print(sprintf("Autofixed hands for %s (%s)", x, min(d.src$session[d.src$name==x])))
      }
    }
  }
  d.src$time <- as.numeric(d.src$time)
  d.src$session <- as.character(d.src$session)
  d.src$x.coord <- as.numeric(d.src$x.coord)
  d.src$y.coord <- as.numeric(d.src$y.coord)
  d.src$hand <- factor(d.src$hand, levels = c("right","left"))
  hand.names <- c(right = "Правая рука", left = "Левая рука")
  
  for(x in unique(d.src$session)) {
    d <- d.src[d.src$session==x,]
    d <- d[!d$duration == 0,]
    if(all(c("right","left") %in% unique(d$hand))) {
      d$tick <- c(NA, diff(d$time))
      d$tick[c(0, diff(d$curr.time)) < 0] <- NA # через дифф ищем смену в текущем времени с 0 на 5, там же сглаживаем провалы в тиках при помощи NA
      
      d$time     <- as.integer(d$time/1000)
      d$datetime <- as.POSIXct(d$time, origin="1970-01-01")
      d$datetime <- format(d$datetime, format="%H:%M:%S")
      result <- data.frame(session = d$session[1], name = d$name[1])
      
      ##########################################################################################
      # Интервал между касаниями
      ggplot(d, aes(x=factor(curr.time), y=remove_outliers(tick))) +
        geom_violin(scale="width") +
        geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
        geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
        stat_summary(fun.y="median", geom="point", shape=20, col="white") +
        ggtitle("Результаты теппинг-теста") +
        labs(x="Время", y="Интервал между касаниями, мс") +
        facet_grid(hand~name, labeller = labeller(hand = hand.names)) +
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
      sprintf("%.2f±%.2f", mean(d$tick[d$hand=="left"], na.rm = T),  sd(d$tick[d$hand=="left"], na.rm = T))
      
      result$interval.mean.right <- mean(d$tick[d$hand=="right"], na.rm = T)
      result$interval.mean.left  <- mean(d$tick[d$hand=="left"] , na.rm = T)
      result$interval.sd.right   <- sd(d$tick[d$hand=="right"] , na.rm = T)
      result$interval.sd.left    <- sd(d$tick[d$hand=="left"] , na.rm = T)
      
      ##########################################################################################
      # Продолжительность касания
      
      ggplot(d, aes(x=factor(curr.time), y=remove_outliers(duration))) +
        geom_violin(scale="width") +
        geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
        geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
        stat_summary(fun.y="median", geom="point", shape=20, col="white") +
        ggtitle("Результаты теппинг-теста") +
        labs(x="Время", y="Продолжительность касания, мс") +
        facet_grid(hand~name, labeller = labeller(hand = hand.names)) +
        theme(legend.position="none",
              axis.text=element_text(size=12),
              axis.title=element_text(size=16,face="bold")) +
        scale_fill_brewer() + geom_jitter(alpha = 0.8) +
        theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
              axis.text.x=element_text(angle=90, vjust=0, size=16),
              axis.ticks.x=element_blank(),
              axis.title.y = element_text(face="bold", size=20),
              axis.text.y  = element_text(angle=90, vjust=0, size=16))
      
      
      sprintf("%.2f±%.2f", mean(d$duration[d$hand=="right"], na.rm = T), sd(d$duration[d$hand=="right"], na.rm = T))
      sprintf("%.2f±%.2f", mean(d$duration[d$hand=="left"], na.rm = T),  sd(d$duration[d$hand=="left"], na.rm = T))
      
      result$duration.mean.right <- mean(d$duration[d$hand=="right"], na.rm = T)
      result$duration.mean.left  <- mean(d$duration[d$hand=="left"] , na.rm = T)
      result$duration.sd.right   <- sd(d$duration[d$hand=="right"] , na.rm = T)
      result$duration.sd.left    <- sd(d$duration[d$hand=="left"] , na.rm = T)
      
      ##########################################################################################
      # Скорость касания (отношение интервала между касаниями к их продолжительности)
      
      ggplot(d, aes(x=factor(curr.time), y=remove_outliers(tick/duration))) +
        geom_violin(scale="width") +
        geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
        geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
        stat_summary(fun.y="median", geom="point", shape=20, col="white") +
        ggtitle("Результаты теппинг-теста") +
        labs(x="Время", y="Скорость касания") +
        facet_grid(hand~name, labeller = labeller(hand = hand.names)) +
        theme(legend.position="none",
              axis.text=element_text(size=12),
              axis.title=element_text(size=16,face="bold")) +
        scale_fill_brewer() + geom_jitter(alpha = 0.8) +
        theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
              axis.text.x=element_text(angle=90, vjust=0, size=16),
              axis.ticks.x=element_blank(),
              axis.title.y = element_text(face="bold", size=20),
              axis.text.y  = element_text(angle=90, vjust=0, size=16))
      
      result$speed.mean.right <- mean(d$tick[d$hand=="right"]/d$duration[d$hand=="right"], na.rm = T)
      result$speed.mean.left  <- mean(d$tick[d$hand=="left"]/d$duration[d$hand=="left"], na.rm = T)
      result$speed.sd.right   <- sd(d$tick[d$hand=="right"]/d$duration[d$hand=="right"], na.rm = T)
      result$speed.sd.left    <- sd(d$tick[d$hand=="left"]/d$duration[d$hand=="left"], na.rm = T)
      
      
      sprintf("%.2f±%.2f", result$speed.mean.right, result$speed.sd.right)
      sprintf("%.2f±%.2f", result$speed.mean.left,  result$speed.sd.left)
      
      result$speed.cor.right <- cor.test(d$tick[d$hand=="right"], d$duration[d$hand=="right"], method = "sp")$estimate
      result$speed.cor.left  <- cor.test(d$tick[d$hand=="left"],  d$duration[d$hand=="left"], method = "sp")$estimate
      
      sprintf("%.4f", result$speed.cor.right)
      sprintf("%.4f", result$speed.cor.left)
      
      ##########################################################################################
      # Количество касаний в секунду
      
      d %>% group_by(curr.time, hand, name) %>% summarise(n = n()) -> res
      ggplot(res, aes(x=factor(curr.time), y=n)) +
        geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
        stat_summary(fun.y="median", geom="point", shape=20, col="black") +
        ggtitle("Результаты теппинг-теста") +
        labs(x="Время", y="Количество касаний в секунду") +
        facet_grid(hand~., labeller = labeller(hand = hand.names)) +
        theme(legend.position="none",
              axis.text=element_text(size=12),
              axis.title=element_text(size=16,face="bold")) +
        scale_fill_brewer() +
        theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
              axis.text.x=element_text(angle=90, vjust=0, size=16),
              axis.ticks.x=element_blank(),
              axis.title.y = element_text(face="bold", size=20),
              axis.text.y  = element_text(angle=90, vjust=0, size=16))
      
      sprintf("%.2f±%.2f", mean(res$n[res$hand=="right"], na.rm = T), sd(res$n[res$hand=="right"], na.rm = T))
      sprintf("%.2f±%.2f", mean(res$n[res$hand=="left"], na.rm = T),  sd(res$n[res$hand=="left"], na.rm = T))
      
      result$tap.mean.right <- mean(res$n[res$hand=="right"], na.rm = T)
      result$tap.mean.left  <- mean(res$n[res$hand=="left"] , na.rm = T)
      result$tap.sd.right   <- sd(res$n[res$hand=="right"] , na.rm = T)
      result$tap.sd.left    <- sd(res$n[res$hand=="left"] , na.rm = T)
      
      ##########################################################################################
      # Кумулятивная сумма касаний в секунду
      
      d %>% group_by(curr.time, hand, name) %>% summarise(n = n()) -> res
      res %>% group_by(hand, name) %>% mutate(cum.n = cumsum(n)) -> res
      ggplot(res, aes(x=factor(curr.time), y=cum.n)) +
        geom_violin(scale="width") +
        geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
        geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
        stat_summary(fun.y="median", geom="point", shape=20, col="white") +
        ggtitle("Результаты теппинг-теста") +
        labs(x="Время", y="Кумулятивная сумма касаний в секунду") +
        facet_grid(hand~., labeller = labeller(hand = hand.names)) +
        theme(legend.position="none",
              axis.text=element_text(size=12),
              axis.title=element_text(size=16,face="bold")) +
        scale_fill_brewer() + geom_jitter(alpha = 0.5) +
        theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
              axis.text.x=element_text(angle=90, vjust=0, size=16),
              axis.ticks.x=element_blank(),
              axis.title.y = element_text(face="bold", size=16),
              axis.text.y  = element_text(angle=90, vjust=0, size=16))
      
      smry.r <- summary(lm(cum.n~curr.time, res[res$hand=="right",]))
      smry.l <- summary(lm(cum.n~curr.time, res[res$hand=="left",]))
      
      result$cum.slope.right <- smry.r$coefficients[2]
      result$cum.slope.left  <- smry.l$coefficients[2]
      result$cum.R2.right    <- smry.r$r.squared
      result$cum.R2.left     <- smry.l$r.squared
      
      ##########################################################################################
      # Коэффициент силы нервной системы (КСНС)
      
      d %>% group_by(hand, curr.time) %>% summarise(n = n(), time = mean(time)) -> res
      res %>% summarise(KSNS=(sum(n-n[curr.time==1]))/sum(n[curr.time==1]*100)) -> res
      
      result$KSNS.right <- as.numeric(res$KSNS[res$hand=="right"])
      result$KSNS.left  <- as.numeric(res$KSNS[res$hand=="left"])
      
      sprintf("%.2f", result$KSNS.right)
      sprintf("%.2f", result$KSNS.left)
      
      ##########################################################################################
      # Коэффициент функциональной асимметрии по работоспособности левой и правой рук
      
      d %>% group_by(hand, curr.time, name) %>% summarise(n = n()) %>% ungroup(hand, curr.time) -> res
      res %>% summarise(KFa = (sum(n[hand=="right"])-sum(n[hand=="left"]))/(sum(n[hand=="right"])+sum(n[hand=="left"]))*100) %>% as.numeric -> result$KFa
      
      sprintf("%.2f", result$KFa)
      
      ##########################################################################################
      # Плотность касаний в процессе прохождения теппинг-теста
      
      ggplot(d, aes(x = x.coord, y = y.coord)) +
        geom_point() +
        geom_density_2d() +
        stat_density_2d(aes(fill = stat(level), color = hand), geom = "polygon") +
        geom_point(aes(x = mean(d$x.coord[d$hand=="right"]), y = mean(d$y.coord[d$hand=="right"])), size = 8, color = "red") +
        geom_point(aes(x = mean(d$x.coord[d$hand=="left"]), y = mean(d$y.coord[d$hand=="left"])), size = 8, color = "blue") +
        geom_segment(aes(x = mean(d$x.coord[d$hand=="right"]), xend = mean(d$x.coord[d$hand=="left"]),
                         y = mean(d$y.coord[d$hand=="right"]), yend = mean(d$y.coord[d$hand=="left"])), 
                     arrow = arrow(angle = 15, ends = "both", type = "closed"), size = 2, color = "green") +
        theme(legend.position = "none")
      
      result$dist.mean <- sqrt((mean(d$x.coord[d$hand=="right"])-mean(d$x.coord[d$hand=="left"]))^2 +
                                 (mean(d$y.coord[d$hand=="right"])-mean(d$y.coord[d$hand=="left"]))^2)
      result$dist.sd.right <- sd(sqrt((d$x.coord[d$hand=="right"])^2+(d$y.coord[d$hand=="right"])^2))
      result$dist.sd.left  <- sd(sqrt((d$x.coord[d$hand=="left"])^2+(d$y.coord[d$hand=="left"])^2))
      
      # Результаты
      
      tbl <- t(result)
      tbl <- cbind(names(result), tbl); rownames(tbl) <- NULL
      colnames(tbl) <- c("Показатель", "Численное значение")
      tbl <- knitr::kable(tbl, align = "c")
      kableExtra::kable_styling(tbl, "striped", full_width = T, position = "left")
      
      # radial chart
      res = gather(result[,-c(1:2)])
      res$value.log <- log(res$value)
      ggplot(data=res,  aes(x=key, y=value.log, group = 1)) + 
        geom_point(size=2) +
        geom_line() +
        xlab("") + 
        ylab("") + 
        coord_polar() +
        theme(panel.grid = element_line(colour = "gray80"),
              axis.text = element_text(size = 14, angle = 0))
      
      if(file.exists(sprintf("%sreport.csv", gsub(basename(filename), "", tools::file_path_as_absolute(filename))))) {
        write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))),
                    sep = ";", row.names = F, append = T, col.names = F)
      } else {
        write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))), sep = ";", row.names = F)
      }
    } else {
      print(sprintf("Something wrong with hands in test for %s (%s)", d$name[1], d$session[1]))
    }
  }
}

# usage:
# do.estimation(filename = "result.csv")

fix.hands <- function(filename) {
  d = read.csv(filename, sep = ";", encoding = "UTF-8", header = F)
  names(d) <- c("session","name", "time","curr.time","duration","x.coord","y.coord", "hand")
  for(x in unique(d$name)) {
    if(!all(c("right","left") %in% d$hand[d$name==x])) {
      if(length(unique(d$session[d$name==x]))==2 & all(d$hand[d$name==x]=="right")) {
        d$hand[d$session==max(d$session[d$name==x])] <- "left"
        d$session[d$session==max(d$session[d$name==x])] <- min(d$session[d$name==x])
      }
    }
  }
  write.table(d, sprintf("%s.fixed.csv", tools::file_path_sans_ext(filename)), sep = ";", row.names = F, col.names = F, quote = F, fileEncoding = "UTF-8")
}