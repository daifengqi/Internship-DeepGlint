library(readxl)
library(tidyverse)
library(lubridate)

setwd('D://R/glst_data') #
theme_set(theme_bw())

# 储存所有event.xlsx文件名
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# 加钞间一个数据框
# 金库一个数据框

# 时间条
timehour <- strptime('2019-05-01 00:00:00',"%Y-%m-%d %H:%M:%S", tz ='UTC')+3600*0:2207
timeday <- seq.Date(from = as.Date('2019/5/1'), to = as.Date('2019/7/30'), by = 'day')

# #1 金库
coffer_df <- as.data.frame(timeday)
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)
  if('SiteID' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-01')) %>%
      filter(SiteID == 178 |
               SiteID == 179) %>%
      # 清除重复值
      mutate(wday = ymd(str_sub(as.character(EventTime),1,10)),
             wmin = ymd_hm(str_sub(as.character(EventTime),1,16))) %>%
      group_by(wmin) %>%             # 这两行删除了相同分钟的重复值
      filter(row_number() == 1) %>%
      ungroup() %>%
      group_by(wday) %>%
      
      summarise(EventCount = n()) %>%
      select(wday,EventCount)
    colnames(processEV) <- c('timeday', ev_code)
    # Merge the dataframes
    coffer_df <- full_join(coffer_df, processEV)
  }
}
coffer_df[is.na(coffer_df)] <- 0

# 授权人进入(113)
# 违规进出：非授权人进入(114)、非准入时间进入(109)、未同进同出(119)、违规进入、未验证进入
# 异常事件：疑似陌生人(115)、镜头异常(124)、区域入侵(122)、人员过多(120)

# 合计1
weiguiJC <- cbind(coffer_df['114'],coffer_df['109'],coffer_df['119'])
yichangSJ <- cbind(coffer_df['115'],coffer_df['124'],coffer_df['122'],coffer_df['120'])
weigui_sum <- apply(weiguiJC, 1, sum)
yichang_sum <- apply(yichangSJ, 1, sum)

# 合计2
coffer_sum_df <- cbind(coffer_df['timeday'],weigui_sum,yichang_sum)
colnames(coffer_sum_df) <- c('时间','违规进出','异常事件')
# 作图
coffer_ga <- gather(coffer_sum_df, key = '类型', value = 'value', -时间)

# 1）发生事件统计
coffer_ga %>% 
  filter(时间 > as.Date('2019-06-01') &
             时间 < as.Date('2019-07-01')) %>%
  ggplot(aes(x=时间,y=value)) + 
  geom_line(aes(col=类型), size=1) + 
  labs(title = '金库事件发生统计',
       subtitle = '2019.06',
       y = '',
       x = '') + 
  scale_color_manual(labels = c("违规进出",'异常事件'), 
                    values = c("违规进出"="#f8766d","异常事件"="#00ba38"))

ggsave('金库事件发生统计06.png',width=7.4,height=4)

# 2.1）健康度测量
weigui_sum <- apply(cbind(3*coffer_df['114'],2*coffer_df['109'],3*coffer_df['119']),1,sum)/8
yichang_sum <- apply(cbind(3*coffer_df['115'],2*coffer_df['124'],2*coffer_df['122'],2*coffer_df['120']), 1, sum)/9
coffer_sum <- (0.5*weigui_sum + 0.5*yichang_sum)/24
coffer_ga <- gather(coffer_sum_df, key = '类型', value = 'value', -时间)
# 危险度
coffer_ga %>% 
  filter(时间 > as.Date('2019-06-01') &
             时间 < as.Date('2019-07-01')) %>%
  ggplot(aes(x=时间,y=value)) + 
  geom_line(aes(col=类型), size=1) + 
  labs(title = '金库危险度统计',
       subtitle = '2019.06',
       y = '',
       x = '') + 
  scale_color_manual(labels = c("违规进出",'异常事件'), 
                     values = c("违规进出"="#f8766d","异常事件"="#00ba38"))

ggsave('金库危险度统计06.png',width=7.4,height=4)





# 根据图像观察

jkd <- 10-coffer_sum
coffer_jkd <- as.data.frame(cbind(coffer_df['timeday'], jkd))

coffer_jkd <- coffer_jkd %>% 
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01'))

coffer_jkd %>% 
  ggplot(aes(x=timeday, y = jkd)) + 
  geom_line(col='lightblue', size=1) + 
  labs(title = '金库安全度测量',
       subtitle='2019.06',
       x='',
       y='')

ggsave('金库安全度06.png',width=7.4,height=4)

# 2.2）健康度离散化
coffer_jkd <- coffer_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state = if_else(jkd > 9.8,'高','中')) %>%
  mutate(state = if_else(jkd > 9.6,state,'低')) %>%
  mutate(state = factor(state, levels = c('高','中','低')))

coffer_jkd_tb <- as.data.frame(table(coffer_jkd$state))

coffer_jkd %>%
  ggplot(aes(jkd)) +
  geom_histogram(bins=10, fill = "#f8766d") + 
  labs(title = '金库安全度分布',
       subtitle='2019.06',
       x='',
       y='')

ggsave('金库安全度直方图分布06.png',width=7.4,height=4)

# 离散化后的折线图
coffer_jkd <- coffer_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state2 = if_else(jkd > 9.8,3,2)) %>%
  mutate(state2 = if_else(jkd > 9.6,state2,1))


coffer_jkd %>% 
  ggplot(aes(x=timeday, y = state2, col=state)) + 
  geom_point(size=5, show.legend = F) + 
  labs(title = '金库安全度离散化测量',
       subtitle='2019.06',
       x='',
       y='')

ggsave('金库安全度离散化测量06.png',width=7.4,height=4)



# #2 加钞间
bill_df <- as.data.frame(timeday)
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)
  if('SiteID' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-01')) %>%
      filter(str_detect(DeviceName, '加钞间')) %>%
      
      # 清除重复值
      mutate(wday = ymd(str_sub(as.character(EventTime),1,10)),
             wmin = ymd_hm(str_sub(as.character(EventTime),1,16))) %>%
      group_by(wmin) %>%             # 这两行删除了相同分钟的重复值
      filter(row_number() == 1) %>%
      ungroup() %>%
      group_by(wday) %>%
      
      summarise(EventCount = n()) %>%
      select(wday,EventCount)
    colnames(processEV) <- c('timeday', ev_code)
    # Merge the dataframes
    bill_df <- full_join(bill_df, processEV)
  }
}
bill_df[is.na(bill_df)] <- 0

# 区域人数异常(127)、逗留过久(128)、走动徘徊(129)、人员接近(130)、移动过快(131)
# 合计1
weiguiJC <- cbind(bill_df['114'],bill_df['109'],bill_df['117'])
weiguiCZ <- cbind(bill_df['118'],bill_df['121'])
yichangSJ <- cbind(bill_df['115'],bill_df['124'],bill_df['116'])
weiguiJC_sum <- apply(weiguiJC, 1, sum)
weiguiCZ_sum <- apply(weiguiCZ, 1, sum)
yichang_sum <- apply(yichangSJ, 1, sum)

# 合计2
bill_sum_df <- cbind(bill_df['timeday'],weiguiJC_sum,weiguiCZ_sum,yichang_sum)
colnames(bill_sum_df) <- c('时间','违规进出','违规操作','异常事件')


# 作图
bill_ga <- gather(bill_sum_df, key = '类型', value = 'value', -时间)

# 1）发生事件统计
bill_ga %>% 
  filter(时间 > as.Date('2019-06-01') &
             时间 < as.Date('2019-07-01')) %>%
  ggplot(aes(x=时间,y=value)) + 
  geom_line(aes(col=类型), size=1) + 
  labs(title = '加钞间事件发生统计',
       subtitle = '2019.06',
       y = '',
       x = '') +
  scale_color_manual(labels = c("违规操作",'违规进出','异常事件'), 
                    values = c("违规进出"="#f8766d","异常事件"="#00ba38",'违规操作'='#0000FF'))

ggsave('加钞间事件发生统计06.png',width=7.4,height=4)


# 2.1）健康度测量
weiguiJC_sum <- apply(cbind(3*bill_df['114'],2*bill_df['109'],2*bill_df['117']), 1, sum)/7
weiguiCZ_sum <- apply(cbind(3*bill_df['118'],3*bill_df['121']), 1, sum)/6
yichang_sum <- apply(cbind(3*bill_df['115'],2*bill_df['124'],3*bill_df['116']), 1, sum)/8
bill_sum <- (weiguiJC_sum +  weiguiCZ_sum +  yichang_sum)/3
bill_sum <- bill_sum/24
bill_sum_df <- cbind(bill_df['timeday'],weiguiJC_sum,weiguiCZ_sum,yichang_sum)
colnames(bill_sum_df) <- c('时间','违规进出','违规操作','异常事件')
# 危险度
bill_ga <- gather(bill_sum_df, key = '类型', value = 'value', -时间)
bill_ga %>% 
  filter(时间 > as.Date('2019-06-01') &
             时间 < as.Date('2019-07-01')) %>%
  ggplot(aes(x=时间,y=value)) + 
  geom_line(aes(col=类型), size=1) + 
  labs(title = '加钞间危险度统计',
       subtitle = '2019.06',
       y = '',
       x = '') +
  scale_color_manual(labels = c("违规操作",'违规进出','异常事件'), 
                     values = c("违规进出"="#f8766d","异常事件"="#00ba38",'违规操作'='#0000FF'))

ggsave('加钞间危险度统计06.png',width=7.4,height=4)
# 根据图像观察

jkd <- 10-bill_sum
bill_jkd <- as.data.frame(cbind(bill_df['timeday'], jkd))

bill_jkd <- bill_jkd %>% 
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01'))

bill_jkd %>% 
  ggplot(aes(x=timeday, y = jkd)) + 
  geom_line(col='lightblue', size=1) + 
  labs(title = '加钞间安全度测量',
       subtitle='2019.06',
       x='',
       y='')

ggsave('加钞间安全度06.png',width=7.4,height=4)

# 2.2）健康度离散化
bill_jkd <- bill_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state = if_else(jkd > 9.8,'高','中')) %>%
  mutate(state = if_else(jkd > 9.6,state,'低')) %>%
  mutate(state = factor(state, levels = c('高','中','低')))

bill_jkd_tb <- as.data.frame(table(bill_jkd$state))

bill_jkd %>%
  ggplot(aes(jkd)) +
  geom_histogram(bins=10, fill = "#f8766d") + 
  labs(title = '加钞间安全度分布',
       subtitle='2019.06',
       x='',
       y='')

ggsave('加钞间安全度直方图分布06.png',width=7.4,height=4)

# 离散化后的折线图
bill_jkd <- bill_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state2 = if_else(jkd > 9.8,3,2)) %>%
  mutate(state2 = if_else(jkd > 9.6,state2,1))


bill_jkd %>% 
  ggplot(aes(x=timeday, y = state2, col=state)) + 
  geom_point(size=5, show.legend = F) + 
  labs(title = '加钞间安全度离散化测量',
       subtitle='2019.06',
       x='',
       y='')

ggsave('加钞间安全度离散化测量06.png',width=7.4,height=4)



