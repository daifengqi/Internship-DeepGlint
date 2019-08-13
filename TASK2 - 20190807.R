library(readxl)
library(tidyverse)
library(lubridate)
library(ggthemr)

ggthemr('flat dark')
setwd('D://R/glst_data') 

# 储存所有event.xlsx文件名
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# 准备工作 
## 时间条
timehour <- strptime('2019-05-01 00:00:00',"%Y-%m-%d %H:%M:%S", tz ='UTC')+3600*0:2207
## 编码映射字典
dic <- c('102' = '无人','105' = '睡觉','108' = '低头','109'='非准入时间进入',
         '112' = '打电话', '113' = '授权人进入', '114' = '非授权人进入',
         '115' = '疑似陌生人', '116' = '人员倒地', '117' = '单人进入',
         '118' = '单人加钞','119' = '双人未同进同出','120' = '人员过多', 
         '121' = '单人滞留过久',
         '122' = '区域入侵', '124' = '镜头异常','125' = '单人离岗',
         '126' = '双人脱岗', '127' = '区域人数异常','128' = '逗留过久',
         '129' = '走动徘徊', '130' = '人员接近' , '131' = '移动过快')
# 履职事件4个：102 105 108 112，分别是，无人 睡觉 低头 打电话

# 1 视频监控中心
video_df <- as.data.frame(timehour)
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)
  if('DeviceName' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-01')) %>%
      filter(str_detect(DeviceName, '视频监控')) %>%
      # group_by to get EventCount
      mutate(whour = ymd_h(str_sub(as.character(EventTime),1,13))) %>%
      group_by(whour) %>%
      
      summarise(EventCount = n()) %>%
      select(whour,EventCount)
    colnames(processEV) <- c('timehour', dic[ev_code])
    # Merge the dataframes
    video_df <- full_join(video_df, processEV)
  }
}
video_df[is.na(video_df)] <- 0

lvzhi_df <- as.data.frame(timehour)
# 1 履职事件
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)

  if('Department' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-02')) %>%
      filter(str_detect(Department, '视频监控')) %>%
      # group_by to get EventCount
      mutate(whour = ymd_h(str_sub(as.character(EventTime),1,13))) %>%
      group_by(whour) %>%
      
      summarise(EventCount = n(),
                DurationSum = sum(EventDuration)/60) %>%
      select(whour,EventCount,DurationSum)
    colnames(processEV) <- c('timehour', dic[ev_code], paste0(dic[ev_code],'持续时间'))
    # Merge the dataframes
    lvzhi_df <- full_join(lvzhi_df, processEV)
  }
}
lvzhi_df[is.na(lvzhi_df)] <- 0


# ）事件加权
# 违规进出：非准入时间进入、非授权人进入、（进出频率异常、离开超时）
# 其他异常：人员倒地、走动徘徊
# 履职：    打电话、睡觉、低头、离开、单人离岗，双人脱岗
wgjc <- (video_df[['非准入时间进入']]*4 + video_df[['非授权人进入']] *7)/11
qtyc <- (video_df[['人员倒地']]*10 + video_df[['走动徘徊']]*1)/11

lz <- (lvzhi_df[['打电话']]*1 + lvzhi_df[['睡觉']]*2 + lvzhi_df[['低头']]*1 +
         lvzhi_df[['无人']]*2 + video_df[['单人离岗']]*5 + video_df[['双人脱岗']]*10)/21 + 
  (lvzhi_df[['打电话持续时间']]*1 + lvzhi_df[['睡觉持续时间']]*2 + lvzhi_df[['低头持续时间']]*1 +
      lvzhi_df[['无人持续时间']]*2)/60

# 离岗和脱岗没有持续时间

# 1.1）每小时危险度，每小时安全度
wxd <- (wgjc+qtyc+lz)/3
aqd <- 10 - wxd
aqd[aqd < 0] <- 0

andf <- as.data.frame(cbind(video_df['timehour'], aqd))
# a 健康度走势（每小时）绘图
andf %>%
  filter(timehour > as.Date('2019-05-31') &
           timehour < as.Date('2019-07-01')) %>%
  ggplot(aes(x=timehour, y = aqd)) + 
  geom_line(size=0.7)+
  labs(title = '视频监控中心健康度测量',
                   subtitle='2019.06',
                   x='',
                   y='')
ggsave('视频监控中心健康度测量（小时）06.png',width=7.4,height=4)

# a.b危险度统计
coffer_sum_df <- cbind(andf['timehour'],wgjc,qtyc,lz)
colnames(coffer_sum_df) <- c('时间','违规进出','其他异常','履职事件')
coffer_ga <- gather(coffer_sum_df, key = '类型', value = 'value', -时间)

coffer_ga %>% 
  filter(时间 > as.Date('2019-06-01') &
             时间 < as.Date('2019-07-01')) %>%
  ggplot(aes(x=时间,y=value)) + 
  geom_line(aes(col=类型), size=0.5) + 
  labs(title = '视频监控中心危险度统计',
       subtitle = '2019.06',
       y = '',
       x = '')
ggsave('视频监控中心危险度统计（小时）06.png',width=7.4,height=4)


# b 直方图的分布
andf %>%
  filter(timehour > as.Date('2019-05-31') &
           timehour < as.Date('2019-07-01')) %>%
  ggplot(aes(aqd)) +
  geom_histogram(bins=20) + 
  labs(title = '视频监控中心健康度分布',
       subtitle='2019.06',
       x='',
       y='')
ggsave('视频监控中心直方图分布（小时）06.png',width=7.4,height=4)

# 1.2）每小时健康度离散化
andf <- andf %>%
  mutate(state = if_else(aqd > 9,'S','A')) %>%
  mutate(state = if_else(aqd > 8,state,'B')) %>%
  mutate(state = if_else(aqd > 7,state,'C')) %>%
  mutate(state = if_else(aqd > 6,state,'D')) %>%
  mutate(state = factor(state, levels = c('S','A','B','C','D')))

# c 离散化后的散点图
andf <- andf %>%
  mutate(state2 = if_else(aqd > 9,5,4)) %>%
  mutate(state2 = if_else(aqd > 8,state2,3)) %>%
  mutate(state2 = if_else(aqd > 7,state2,2)) %>%
  mutate(state2 = if_else(aqd > 6,state2,1))


andf %>%
  filter(timehour > as.Date('2019-05-31') &
           timehour < as.Date('2019-07-01')) %>%
  ggplot(aes(x=timehour, y = state2, col=state)) + 
  geom_point(size=1.5, show.legend = F) + 
  labs(title = '视频监控中心健康度离散化测量',
       subtitle='2019.06',
       x='',
       y='')
ggsave('视频监控中心离散化测量（小时）06.png',width=7.4,height=4)
andf_tb <- as.data.frame(table(andf$state))



# 2）每天的健康度
andf_day <- andf %>%
  mutate(wday = ymd(str_sub(as.character(timehour),1,10)),
         weight = if_else(aqd>6, 0.3, 0.7)) %>%
  mutate(dayaq_w = weight * aqd) %>%
  group_by(wday) %>%
  summarise(dayaq = sum(dayaq_w)*10/72)



# a 健康度走势（每天）绘图
andf_day %>%
  filter(wday > as.Date('2019-05-31') &
           wday < as.Date('2019-07-01')) %>%
  ggplot(aes(x=wday, y = dayaq)) + 
  geom_line(size=0.7)+
  labs(title = '视频监控中心健康度测量',
       subtitle='2019.06',
       x='',
       y='')
ggsave('视频监控中心健康度（天）06.png',width=7.4,height=4)
# b 直方图的分布
andf_day %>%
  filter(wday > as.Date('2019-05-31') &
           wday < as.Date('2019-07-01')) %>%
  ggplot(aes(dayaq)) +
  geom_histogram(bins=20) + 
  labs(title = '视频监控中心健康度分布',
       subtitle='2019.06',
       x='',
       y='')
ggsave('视频监控中心健康度直方图分布（天）06.png',width=7.4,height=4)
# 1.2）每天健康度离散化
andf_day <- andf_day %>%
  mutate(state = if_else(dayaq > 9,'S','A')) %>%
  mutate(state = if_else(dayaq > 8,state,'B')) %>%
  mutate(state = if_else(dayaq > 7,state,'C')) %>%
  mutate(state = if_else(dayaq > 6,state,'D')) %>%
  mutate(state = factor(state, levels = c('S','A','B','C','D')))

# c 离散化后的散点图
andf_day <- andf_day %>%
  mutate(state2 = if_else(dayaq > 9,5,4)) %>%
  mutate(state2 = if_else(dayaq > 8,state2,3)) %>%
  mutate(state2 = if_else(dayaq > 7,state2,2)) %>%
  mutate(state2 = if_else(dayaq > 6,state2,1))


andf_day %>%
  filter(wday > as.Date('2019-05-31') &
           wday < as.Date('2019-07-01')) %>%
  ggplot(aes(x=wday, y = state2, col=state)) + 
  geom_point(size=2.5, show.legend = F) + 
  labs(title = '视频监控中心健康度离散化测量',
       subtitle='2019.06',
       x='',
       y='')
ggsave('视频监控中心健康度离散化测量（天）06.png',width=7.4,height=4)
andf_tb_day <- as.data.frame(table(andf_day$state))


tmp <- read.csv('包括安全度的构建数据2.0.csv')
length(tmp$aqd)
tmpdf <- as.data.frame(cbind(rep(1:168,8),tmp['情形'],tmp$aqd))
colnames(tmpdf) <- c('V1','模拟情形','V3')


tmpdf %>%
  filter(模拟情形 == '情形1' |
           模拟情形 == '情形2') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=模拟情形), size=0.5) + 
  labs(title = '模拟情形（1 vs 2）',
       subtitle = '一周数据',
       y = '',
       x = '')
ggsave('模拟情形（小时）12.png',width=7.4,height=4)
tmpdf %>%
  filter(模拟情形 == '情形3' |
               模拟情形 == '情形4') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=模拟情形), size=0.5) + 
  labs(title = '模拟情形（3 vs 4）',
       subtitle = '一周数据',
       y = '',
       x = '')
ggsave('模拟情形（小时）34.png',width=7.4,height=4)

tmpdf %>%
  filter(模拟情形 == '情形5' |
               模拟情形 == '情形6') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=模拟情形), size=0.5) + 
  labs(title = '模拟情形（5 vs 6）',
       subtitle = '一周数据',
       y = '',
       x = '')
ggsave('模拟情形（小时）56.png',width=7.4,height=4)

tmpdf %>%
  filter(模拟情形 == '情形7' |
               模拟情形 == '情形8') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=模拟情形), size=0.5) + 
  labs(title = '模拟情形（7 vs 8）',
       subtitle = '一周数据',
       y = '',
       x = '')
ggsave('模拟情形（小时）78.png',width=7.4,height=4)
