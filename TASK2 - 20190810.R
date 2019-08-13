library(readxl)
library(tidyverse)
library(lubridate)
library(ggthemr)

ggthemr('flat dark')
setwd('D://R/glst_data') 

# ��������event.xlsx�ļ���
file_list <- list.files()
event_list <- file_list[which(str_detect(file_list,'EventType'))]

# ģ������
sim <- read.csv('�����õ�����3.0.csv', stringsAsFactors = F)
# ����Σ�ն�
wxd <- 1*sim[['��绰']] + 1.5*sim[['˯��']] + 1*sim[['��ͷ']] + 1.5*sim[['�뿪']] + 
  3*sim[['���']] + 10*sim[['�Ѹ�']] + 8*sim[['������Ļ��ע']] + 2.5*sim[['��׼��ʱ�����']] + 
  8*sim[['����Ȩ�˽���']] + 1.5*sim[['����Ƶ���쳣']] + 1*sim[['�뿪��ʱ']] + 
  10*sim[['��Ա����']] + 1*sim[['�߶��ǻ�']]
# ���㽡����
jkd <- 20 - wxd
jkd <- jkd/2
jkd[jkd < 0] <- 0
# check
summary(jkd)

# ��ͼ
tmpdf <- as.data.frame(cbind(rep(1:24,7),sim['����'],jkd))
colnames(tmpdf) <- c('V1','ģ������','V3')
setwd('D://R/glst_data/PLOT') 
# (1)
tmpdf %>%
  filter(ģ������ == '����1') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�1��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�1��һ������.png',width=7.4,height=4)

# (2)
tmpdf %>%
  filter(ģ������ == '����2') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�2��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�2��һ������.png',width=7.4,height=4)


# (3)
tmpdf %>%
  filter(ģ������ == '����3') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�3��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�3��һ������.png',width=7.4,height=4)

# (4)
tmpdf %>%
  filter(ģ������ == '����4') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�4��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�4��һ������.png',width=7.4,height=4)

# (1)
tmpdf %>%
  filter(ģ������ == '����5') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�5��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�5��һ������.png',width=7.4,height=4)

# (6)
tmpdf %>%
  filter(ģ������ == '����6') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�6��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�6��һ������.png',width=7.4,height=4)

# (7)
tmpdf %>%
  filter(ģ������ == '����7') %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(size=0.8) + 
  labs(title = 'ģ�����Σ�7��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('ģ�����Σ�7��һ������.png',width=7.4,height=4)

# (8)
tmpdf %>%
  ggplot(aes(x=V1,y=V3)) + 
  geom_line(aes(col=ģ������),size=0.8) + 
  labs(title = 'ģ�����Σ�7��',
       y = '',
       x = '') + 
  ylim(0,10)
ggsave('�������εĶԱ�һ������.png',width=7.4,height=4)


# ����ָ��鿴
c <- tmpdf %>%
  select(-V1) %>%
  group_by(ģ������) %>%
  summarise(��λ�� = median(V3),
               ƽ���� = mean(V3),
               ��Сֵ = min(V3),
               ���ֵ = max(V3))

xlsx::write.xlsx(x = c,file = '����.xlsx')