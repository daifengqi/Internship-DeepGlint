library(readxl)
library(tidyverse)
library(lubridate)

setwd('D://R/glst_data') #
theme_set(theme_bw())

# ��������event.xlsx�ļ���
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# �ӳ���һ�����ݿ�
# ���һ�����ݿ�

# ʱ����
timehour <- strptime('2019-05-01 00:00:00',"%Y-%m-%d %H:%M:%S", tz ='UTC')+3600*0:2207
timeday <- seq.Date(from = as.Date('2019/5/1'), to = as.Date('2019/7/30'), by = 'day')

# #1 ���
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
      # ����ظ�ֵ
      mutate(wday = ymd(str_sub(as.character(EventTime),1,10)),
             wmin = ymd_hm(str_sub(as.character(EventTime),1,16))) %>%
      group_by(wmin) %>%             # ������ɾ������ͬ���ӵ��ظ�ֵ
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

# ��Ȩ�˽���(113)
# Υ�����������Ȩ�˽���(114)����׼��ʱ�����(109)��δͬ��ͬ��(119)��Υ����롢δ��֤����
# �쳣�¼�������İ����(115)����ͷ�쳣(124)����������(122)����Ա����(120)

# �ϼ�1
weiguiJC <- cbind(coffer_df['114'],coffer_df['109'],coffer_df['119'])
yichangSJ <- cbind(coffer_df['115'],coffer_df['124'],coffer_df['122'],coffer_df['120'])
weigui_sum <- apply(weiguiJC, 1, sum)
yichang_sum <- apply(yichangSJ, 1, sum)

# �ϼ�2
coffer_sum_df <- cbind(coffer_df['timeday'],weigui_sum,yichang_sum)
colnames(coffer_sum_df) <- c('ʱ��','Υ�����','�쳣�¼�')
# ��ͼ
coffer_ga <- gather(coffer_sum_df, key = '����', value = 'value', -ʱ��)

# 1�������¼�ͳ��
coffer_ga %>% 
  filter(ʱ�� > as.Date('2019-06-01') &
             ʱ�� < as.Date('2019-07-01')) %>%
  ggplot(aes(x=ʱ��,y=value)) + 
  geom_line(aes(col=����), size=1) + 
  labs(title = '����¼�����ͳ��',
       subtitle = '2019.06',
       y = '',
       x = '') + 
  scale_color_manual(labels = c("Υ�����",'�쳣�¼�'), 
                    values = c("Υ�����"="#f8766d","�쳣�¼�"="#00ba38"))

ggsave('����¼�����ͳ��06.png',width=7.4,height=4)

# 2.1�������Ȳ���
weigui_sum <- apply(cbind(3*coffer_df['114'],2*coffer_df['109'],3*coffer_df['119']),1,sum)/8
yichang_sum <- apply(cbind(3*coffer_df['115'],2*coffer_df['124'],2*coffer_df['122'],2*coffer_df['120']), 1, sum)/9
coffer_sum <- (0.5*weigui_sum + 0.5*yichang_sum)/24
coffer_ga <- gather(coffer_sum_df, key = '����', value = 'value', -ʱ��)
# Σ�ն�
coffer_ga %>% 
  filter(ʱ�� > as.Date('2019-06-01') &
             ʱ�� < as.Date('2019-07-01')) %>%
  ggplot(aes(x=ʱ��,y=value)) + 
  geom_line(aes(col=����), size=1) + 
  labs(title = '���Σ�ն�ͳ��',
       subtitle = '2019.06',
       y = '',
       x = '') + 
  scale_color_manual(labels = c("Υ�����",'�쳣�¼�'), 
                     values = c("Υ�����"="#f8766d","�쳣�¼�"="#00ba38"))

ggsave('���Σ�ն�ͳ��06.png',width=7.4,height=4)





# ����ͼ��۲�

jkd <- 10-coffer_sum
coffer_jkd <- as.data.frame(cbind(coffer_df['timeday'], jkd))

coffer_jkd <- coffer_jkd %>% 
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01'))

coffer_jkd %>% 
  ggplot(aes(x=timeday, y = jkd)) + 
  geom_line(col='lightblue', size=1) + 
  labs(title = '��ⰲȫ�Ȳ���',
       subtitle='2019.06',
       x='',
       y='')

ggsave('��ⰲȫ��06.png',width=7.4,height=4)

# 2.2����������ɢ��
coffer_jkd <- coffer_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state = if_else(jkd > 9.8,'��','��')) %>%
  mutate(state = if_else(jkd > 9.6,state,'��')) %>%
  mutate(state = factor(state, levels = c('��','��','��')))

coffer_jkd_tb <- as.data.frame(table(coffer_jkd$state))

coffer_jkd %>%
  ggplot(aes(jkd)) +
  geom_histogram(bins=10, fill = "#f8766d") + 
  labs(title = '��ⰲȫ�ȷֲ�',
       subtitle='2019.06',
       x='',
       y='')

ggsave('��ⰲȫ��ֱ��ͼ�ֲ�06.png',width=7.4,height=4)

# ��ɢ���������ͼ
coffer_jkd <- coffer_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state2 = if_else(jkd > 9.8,3,2)) %>%
  mutate(state2 = if_else(jkd > 9.6,state2,1))


coffer_jkd %>% 
  ggplot(aes(x=timeday, y = state2, col=state)) + 
  geom_point(size=5, show.legend = F) + 
  labs(title = '��ⰲȫ����ɢ������',
       subtitle='2019.06',
       x='',
       y='')

ggsave('��ⰲȫ����ɢ������06.png',width=7.4,height=4)



# #2 �ӳ���
bill_df <- as.data.frame(timeday)
for(i in seq_along(event_list)){
  ev <- read_xlsx(event_list[i])
  ev_file <- event_list[i]
  ev_code <- str_sub(ev_file, 11, 13)
  if('SiteID' %in% colnames(ev)){
    processEV <- ev %>%
      filter(EventTime > as.POSIXct('2019-05-01')) %>%
      filter(str_detect(DeviceName, '�ӳ���')) %>%
      
      # ����ظ�ֵ
      mutate(wday = ymd(str_sub(as.character(EventTime),1,10)),
             wmin = ymd_hm(str_sub(as.character(EventTime),1,16))) %>%
      group_by(wmin) %>%             # ������ɾ������ͬ���ӵ��ظ�ֵ
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

# ���������쳣(127)����������(128)���߶��ǻ�(129)����Ա�ӽ�(130)���ƶ�����(131)
# �ϼ�1
weiguiJC <- cbind(bill_df['114'],bill_df['109'],bill_df['117'])
weiguiCZ <- cbind(bill_df['118'],bill_df['121'])
yichangSJ <- cbind(bill_df['115'],bill_df['124'],bill_df['116'])
weiguiJC_sum <- apply(weiguiJC, 1, sum)
weiguiCZ_sum <- apply(weiguiCZ, 1, sum)
yichang_sum <- apply(yichangSJ, 1, sum)

# �ϼ�2
bill_sum_df <- cbind(bill_df['timeday'],weiguiJC_sum,weiguiCZ_sum,yichang_sum)
colnames(bill_sum_df) <- c('ʱ��','Υ�����','Υ�����','�쳣�¼�')


# ��ͼ
bill_ga <- gather(bill_sum_df, key = '����', value = 'value', -ʱ��)

# 1�������¼�ͳ��
bill_ga %>% 
  filter(ʱ�� > as.Date('2019-06-01') &
             ʱ�� < as.Date('2019-07-01')) %>%
  ggplot(aes(x=ʱ��,y=value)) + 
  geom_line(aes(col=����), size=1) + 
  labs(title = '�ӳ����¼�����ͳ��',
       subtitle = '2019.06',
       y = '',
       x = '') +
  scale_color_manual(labels = c("Υ�����",'Υ�����','�쳣�¼�'), 
                    values = c("Υ�����"="#f8766d","�쳣�¼�"="#00ba38",'Υ�����'='#0000FF'))

ggsave('�ӳ����¼�����ͳ��06.png',width=7.4,height=4)


# 2.1�������Ȳ���
weiguiJC_sum <- apply(cbind(3*bill_df['114'],2*bill_df['109'],2*bill_df['117']), 1, sum)/7
weiguiCZ_sum <- apply(cbind(3*bill_df['118'],3*bill_df['121']), 1, sum)/6
yichang_sum <- apply(cbind(3*bill_df['115'],2*bill_df['124'],3*bill_df['116']), 1, sum)/8
bill_sum <- (weiguiJC_sum +  weiguiCZ_sum +  yichang_sum)/3
bill_sum <- bill_sum/24
bill_sum_df <- cbind(bill_df['timeday'],weiguiJC_sum,weiguiCZ_sum,yichang_sum)
colnames(bill_sum_df) <- c('ʱ��','Υ�����','Υ�����','�쳣�¼�')
# Σ�ն�
bill_ga <- gather(bill_sum_df, key = '����', value = 'value', -ʱ��)
bill_ga %>% 
  filter(ʱ�� > as.Date('2019-06-01') &
             ʱ�� < as.Date('2019-07-01')) %>%
  ggplot(aes(x=ʱ��,y=value)) + 
  geom_line(aes(col=����), size=1) + 
  labs(title = '�ӳ���Σ�ն�ͳ��',
       subtitle = '2019.06',
       y = '',
       x = '') +
  scale_color_manual(labels = c("Υ�����",'Υ�����','�쳣�¼�'), 
                     values = c("Υ�����"="#f8766d","�쳣�¼�"="#00ba38",'Υ�����'='#0000FF'))

ggsave('�ӳ���Σ�ն�ͳ��06.png',width=7.4,height=4)
# ����ͼ��۲�

jkd <- 10-bill_sum
bill_jkd <- as.data.frame(cbind(bill_df['timeday'], jkd))

bill_jkd <- bill_jkd %>% 
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01'))

bill_jkd %>% 
  ggplot(aes(x=timeday, y = jkd)) + 
  geom_line(col='lightblue', size=1) + 
  labs(title = '�ӳ��䰲ȫ�Ȳ���',
       subtitle='2019.06',
       x='',
       y='')

ggsave('�ӳ��䰲ȫ��06.png',width=7.4,height=4)

# 2.2����������ɢ��
bill_jkd <- bill_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state = if_else(jkd > 9.8,'��','��')) %>%
  mutate(state = if_else(jkd > 9.6,state,'��')) %>%
  mutate(state = factor(state, levels = c('��','��','��')))

bill_jkd_tb <- as.data.frame(table(bill_jkd$state))

bill_jkd %>%
  ggplot(aes(jkd)) +
  geom_histogram(bins=10, fill = "#f8766d") + 
  labs(title = '�ӳ��䰲ȫ�ȷֲ�',
       subtitle='2019.06',
       x='',
       y='')

ggsave('�ӳ��䰲ȫ��ֱ��ͼ�ֲ�06.png',width=7.4,height=4)

# ��ɢ���������ͼ
bill_jkd <- bill_jkd %>%
  filter(timeday > as.Date('2019-05-31') &
           timeday < as.Date('2019-07-01')) %>%
  mutate(state2 = if_else(jkd > 9.8,3,2)) %>%
  mutate(state2 = if_else(jkd > 9.6,state2,1))


bill_jkd %>% 
  ggplot(aes(x=timeday, y = state2, col=state)) + 
  geom_point(size=5, show.legend = F) + 
  labs(title = '�ӳ��䰲ȫ����ɢ������',
       subtitle='2019.06',
       x='',
       y='')

ggsave('�ӳ��䰲ȫ����ɢ������06.png',width=7.4,height=4)


