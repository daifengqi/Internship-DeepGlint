library(readxl)
library(tidyverse)
library(lubridate)
library(ggthemr)

ggthemr('flat dark')
setwd('D://R/glst_data') 

# ��������event.xlsx�ļ���
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# ׼������ 
## ʱ����
timehour <- strptime('2019-05-01 00:00:00',"%Y-%m-%d %H:%M:%S", tz ='UTC')+3600*0:2207
## ����ӳ���ֵ�
dic <- c('102' = '����','105' = '˯��','108' = '��ͷ','109'='��׼��ʱ�����',
         '112' = '��绰', '113' = '��Ȩ�˽���', '114' = '����Ȩ�˽���',
         '115' = '����İ����', '116' = '��Ա����', '117' = '���˽���',
         '118' = '���˼ӳ�','119' = '˫��δͬ��ͬ��','120' = '��Ա����', 
         '121' = '������������',
         '122' = '��������', '124' = '��ͷ�쳣','125' = '�������',
         '126' = '˫���Ѹ�', '127' = '���������쳣','128' = '��������',
         '129' = '�߶��ǻ�', '130' = '��Ա�ӽ�' , '131' = '�ƶ�����')
# ��ְ�¼�4����102 105 108 112���ֱ��ǣ����� ˯�� ��ͷ ��绰

# 1 ��Ƶ�������
video_df <- as.data.frame(timehour)
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)
  if('DeviceName' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-01')) %>%
      filter(str_detect(DeviceName, '��Ƶ���')) %>%
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
# 1 ��ְ�¼�
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)

  if('Department' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-02')) %>%
      filter(str_detect(Department, '��Ƶ���')) %>%
      # group_by to get EventCount
      mutate(whour = ymd_h(str_sub(as.character(EventTime),1,13))) %>%
      group_by(whour) %>%
      
      summarise(EventCount = n(),
                DurationSum = sum(EventDuration)/60) %>%
      select(whour,EventCount,DurationSum)
    colnames(processEV) <- c('timehour', dic[ev_code], paste0(dic[ev_code],'����ʱ��'))
    # Merge the dataframes
    lvzhi_df <- full_join(lvzhi_df, processEV)
  }
}
lvzhi_df[is.na(lvzhi_df)] <- 0


# ���¼���Ȩ
# Υ���������׼��ʱ����롢����Ȩ�˽��롢������Ƶ���쳣���뿪��ʱ��
# �����쳣����Ա���ء��߶��ǻ�
# ��ְ��    ��绰��˯������ͷ���뿪��������ڣ�˫���Ѹ�
wgjc <- (video_df[['��׼��ʱ�����']]*4 + video_df[['����Ȩ�˽���']] *7)/11
qtyc <- (video_df[['��Ա����']]*10 + video_df[['�߶��ǻ�']]*1)/11

lz <- (lvzhi_df[['��绰']]*1 + lvzhi_df[['˯��']]*2 + lvzhi_df[['��ͷ']]*1 +
         lvzhi_df[['����']]*2 + video_df[['�������']]*5 + video_df[['˫���Ѹ�']]*10)/21 + 
  (lvzhi_df[['��绰����ʱ��']]*1 + lvzhi_df[['˯������ʱ��']]*2 + lvzhi_df[['��ͷ����ʱ��']]*1 +
      lvzhi_df[['���˳���ʱ��']]*2)/60

# ��ں��Ѹ�û�г���ʱ��

# 1.1��ÿСʱΣ�նȣ�ÿСʱ��ȫ��
wxd <- (wgjc+qtyc+lz)/3
aqd <- 10 - wxd
aqd[aqd < 0] <- 0

andf <- as.data.frame(cbind(video_df['timehour'], aqd))
# a ���������ƣ�ÿСʱ����ͼ
andf %>%
  filter(timehour > as.Date('2019-05-31') &
           timehour < as.Date('2019-07-01')) %>%
  ggplot(aes(x=timehour, y = aqd)) + 
  geom_line(size=0.7)+
  labs(title = '��Ƶ������Ľ����Ȳ���',
                   subtitle='2019.06',
                   x='',
                   y='')
ggsave('��Ƶ������Ľ����Ȳ�����Сʱ��06.png',width=7.4,height=4)

# a.bΣ�ն�ͳ��
coffer_sum_df <- cbind(andf['timehour'],wgjc,qtyc,lz)
colnames(coffer_sum_df) <- c('ʱ��','Υ�����','�����쳣','��ְ�¼�')
coffer_ga <- gather(coffer_sum_df, key = '����', value = 'value', -ʱ��)

coffer_ga %>% 
  filter(ʱ�� > as.Date('2019-06-01') &
             ʱ�� < as.Date('2019-07-01')) %>%
  ggplot(aes(x=ʱ��,y=value)) + 
  geom_line(aes(col=����), size=0.5) + 
  labs(title = '��Ƶ�������Σ�ն�ͳ��',
       subtitle = '2019.06',
       y = '',
       x = '')
ggsave('��Ƶ�������Σ�ն�ͳ�ƣ�Сʱ��06.png',width=7.4,height=4)


# b ֱ��ͼ�ķֲ�
andf %>%
  filter(timehour > as.Date('2019-05-31') &
           timehour < as.Date('2019-07-01')) %>%
  ggplot(aes(aqd)) +
  geom_histogram(bins=20) + 
  labs(title = '��Ƶ������Ľ����ȷֲ�',
       subtitle='2019.06',
       x='',
       y='')
ggsave('��Ƶ�������ֱ��ͼ�ֲ���Сʱ��06.png',width=7.4,height=4)

# 1.2��ÿСʱ��������ɢ��
andf <- andf %>%
  mutate(state = if_else(aqd > 9,'S','A')) %>%
  mutate(state = if_else(aqd > 8,state,'B')) %>%
  mutate(state = if_else(aqd > 7,state,'C')) %>%
  mutate(state = if_else(aqd > 6,state,'D')) %>%
  mutate(state = factor(state, levels = c('S','A','B','C','D')))

# c ��ɢ�����ɢ��ͼ
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
  labs(title = '��Ƶ������Ľ�������ɢ������',
       subtitle='2019.06',
       x='',
       y='')
ggsave('��Ƶ���������ɢ��������Сʱ��06.png',width=7.4,height=4)
andf_tb <- as.data.frame(table(andf$state))



# 2��ÿ��Ľ�����
andf_day <- andf %>%
  mutate(wday = ymd(str_sub(as.character(timehour),1,10)),
         weight = if_else(aqd>6, 0.3, 0.7)) %>%
  mutate(dayaq_w = weight * aqd) %>%
  group_by(wday) %>%
  summarise(dayaq = sum(dayaq_w)*10/72)



# a ���������ƣ�ÿ�죩��ͼ
andf_day %>%
  filter(wday > as.Date('2019-05-31') &
           wday < as.Date('2019-07-01')) %>%
  ggplot(aes(x=wday, y = dayaq)) + 
  geom_line(size=0.7)+
  labs(title = '��Ƶ������Ľ����Ȳ���',
       subtitle='2019.06',
       x='',
       y='')
ggsave('��Ƶ������Ľ����ȣ��죩06.png',width=7.4,height=4)
# b ֱ��ͼ�ķֲ�
andf_day %>%
  filter(wday > as.Date('2019-05-31') &
           wday < as.Date('2019-07-01')) %>%
  ggplot(aes(dayaq)) +
  geom_histogram(bins=20) + 
  labs(title = '��Ƶ������Ľ����ȷֲ�',
       subtitle='2019.06',
       x='',
       y='')
ggsave('��Ƶ������Ľ�����ֱ��ͼ�ֲ����죩06.png',width=7.4,height=4)
# 1.2��ÿ�콡������ɢ��
andf_day <- andf_day %>%
  mutate(state = if_else(dayaq > 9,'S','A')) %>%
  mutate(state = if_else(dayaq > 8,state,'B')) %>%
  mutate(state = if_else(dayaq > 7,state,'C')) %>%
  mutate(state = if_else(dayaq > 6,state,'D')) %>%
  mutate(state = factor(state, levels = c('S','A','B','C','D')))

# c ��ɢ�����ɢ��ͼ
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
  labs(title = '��Ƶ������Ľ�������ɢ������',
       subtitle='2019.06',
       x='',
       y='')
ggsave('��Ƶ������Ľ�������ɢ���������죩06.png',width=7.4,height=4)
andf_tb_day <- as.data.frame(table(andf_day$state))


tmp <- read.csv('������ȫ�ȵĹ�������2.0.csv')
length(tmp$aqd)
tmpdf <- as.data.frame(cbind(rep(1:168,8),tmp['����'],tmp$aqd))
colnames(tmpdf) <- c('V1','ģ������','V3')


tmpdf %>%
  filter(ģ������ == '����1' |
           ģ������ == '����2') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=ģ������), size=0.5) + 
  labs(title = 'ģ�����Σ�1 vs 2��',
       subtitle = 'һ������',
       y = '',
       x = '')
ggsave('ģ�����Σ�Сʱ��12.png',width=7.4,height=4)
tmpdf %>%
  filter(ģ������ == '����3' |
               ģ������ == '����4') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=ģ������), size=0.5) + 
  labs(title = 'ģ�����Σ�3 vs 4��',
       subtitle = 'һ������',
       y = '',
       x = '')
ggsave('ģ�����Σ�Сʱ��34.png',width=7.4,height=4)

tmpdf %>%
  filter(ģ������ == '����5' |
               ģ������ == '����6') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=ģ������), size=0.5) + 
  labs(title = 'ģ�����Σ�5 vs 6��',
       subtitle = 'һ������',
       y = '',
       x = '')
ggsave('ģ�����Σ�Сʱ��56.png',width=7.4,height=4)

tmpdf %>%
  filter(ģ������ == '����7' |
               ģ������ == '����8') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=ģ������), size=0.5) + 
  labs(title = 'ģ�����Σ�7 vs 8��',
       subtitle = 'һ������',
       y = '',
       x = '')
ggsave('ģ�����Σ�Сʱ��78.png',width=7.4,height=4)