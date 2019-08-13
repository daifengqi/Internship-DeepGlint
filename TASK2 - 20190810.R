library(readxl)
library(tidyverse)
library(lubridate)
library(ggthemr)

ggthemr('flat dark')
setwd('D://R/glst_data') 

# 储存所有event.xlsx文件名
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# 模拟数据
sim <- read.csv('构建好的数据3.0.csv', stringsAsFactors = F)
# 计算危险度
wxd <- 1*sim[['打电话']] + 1.5*sim[['睡觉']] + 1*sim[['低头']] + 1.5*sim[['离开']] + 
  3*sim[['离岗']] + 10*sim[['脱岗']] + 8*sim[['主动屏幕关注']] + 2.5*sim[['非准入时间进入']] + 
  8*sim[['非授权人进入']] + 1.5*sim[['进出频率异常']] + 1*sim[['离开超时']] + 
  10*sim[['人员倒地']] + 1*sim[['走动徘徊']]
# 计算健康度
jkd <- 20 - wxd
jkd <- jkd/2
jkd[jkd < 0] <- 0
# check
summary(jkd)

# 作图
tmpdf <- as.data.frame(cbind(rep(1:24,7),sim['情形'],jkd))
colnames(tmpdf) <- c('V1','模拟情形','V3')
setwd('D://R/glst_data/PLOT') 
# (1)
tmpdf %>%
  filter(模拟情形 == '情形1') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（1）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（1）一天数据.png',width=7.4,height=4)

# (2)
tmpdf %>%
  filter(模拟情形 == '情形2') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（2）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（2）一天数据.png',width=7.4,height=4)


# (3)
tmpdf %>%
  filter(模拟情形 == '情形3') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（3）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（3）一天数据.png',width=7.4,height=4)

# (4)
tmpdf %>%
  filter(模拟情形 == '情形4') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（4）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（4）一天数据.png',width=7.4,height=4)

# (1)
tmpdf %>%
  filter(模拟情形 == '情形5') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（5）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（5）一天数据.png',width=7.4,height=4)

# (6)
tmpdf %>%
  filter(模拟情形 == '情形6') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（6）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（6）一天数据.png',width=7.4,height=4)

# (7)
tmpdf %>%
  filter(模拟情形 == '情形7') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = '模拟情形（7）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('模拟情形（7）一天数据.png',width=7.4,height=4)

# (8)
tmpdf %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=模拟情形),size=0.8) + 
  labs(title = '模拟情形（7）',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('七种情形的对比一天数据.png',width=7.4,height=4)


# 数据指标查看
c <- tmpdf %>%
  select(-V1) %>%
  group_by(模拟情形) %>%
  summarise(中位数 = median(V3),
               平均数 = mean(V3),
               最小值 = min(V3),
               最大值 = max(V3))

xlsx::write.xlsx(x = c,file = '汇总.xlsx')
