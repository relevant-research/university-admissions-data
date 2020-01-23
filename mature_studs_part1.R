### Обробка, аналіз та візуалізація даних про вступну кампанію 2019 ###

# У цьому аналізі ми порівняємо абітурієнтів, які вступали 
# одразу після школи та тих, які завершили середню освіту раніше 2019 року.

# У першій частині туторіалу ми досліджуватимемо дані за допомогою 
# описових статистик та графіків (intro level)

## Потрібні бібліотеки

library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)

# 1. Імпортуємо дані (датасет отриманий на запит на публічну інформацію від Єдиної державної електронної бази з питань освіти)

data <- read.csv("/YOUR_PATH/data.csv", stringsAsFactors = FALSE)

str(data)
glimpse(data)
data %>% head() %>% View()

## найцікавіша" колонка для нас - Рік.завершення.середньої.освіти
unique(data$Рік.завершення.середньої.освіти)

# 2. Кількість вступників за роком завершення середньої освіти ----------------------------------------------------------------------

data %>% 
  select(Рік.завершення.середньої.освіти, Код.абітурієнта) %>% 
  distinct() %>% 
  group_by(Рік.завершення.середньої.освіти) %>% 
  summarise (freq = n()) %>%
  View()

# Очікувано, найбільше вступників на денну форму завершили середню освіту у 2019 році або незадовго до цього

## Давайте поглянемо на графіку (використаємо барчарт)

data %>% 
  filter(Рік.завершення.середньої.освіти != '2019') %>% 
  select(Рік.завершення.середньої.освіти, Код.абітурієнта) %>% 
  distinct() %>% 
  group_by(Рік.завершення.середньої.освіти) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot() +
  geom_segment(aes(x=as.factor(Рік.завершення.середньої.освіти), 
                   xend = as.factor(Рік.завершення.середньої.освіти), 
                   y=0, yend = n), size = 0.5, color = '#00afbe') +
  geom_point(aes(x = as.factor(Рік.завершення.середньої.освіти), y= n), 
             size= 1.5, color = '#00afbe') +
  geom_text(aes(as.factor(Рік.завершення.середньої.освіти), n, label=n), 
            vjust=0.4, hjust = -0.4, size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_y_discrete(expand = c(0.01, 0)) +
  theme(text = element_text(family = "Open Sans"),
        title = element_text(family = "Open Sans", size = 10),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Open Sans", size = 7)) +
  labs(x = "",  y = "",
       title = "Кількість вступників за роком завершення середньої освіти (окрім 2019 р.)",
       caption = 'Дані: ЄДЕБО')

# 3. Популярні галузі, спеціальності та університети ----------------------

# 1) Популярні галузі (за кількістю заявок) 
data %>% 
  group_by(Рік.завершення.середньої.освіти, Галузь.на.яку.подано.заявку) %>%
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(Рік.завершення.середньої.освіти) %>% 
  mutate(rel.freq = paste0(100 * freq/sum(freq))) %>% 
  mutate(rel.freq = round(as.numeric(rel.freq), 2)) %>%
  View()

# 2) Популярні спеціальності (за кількістю зарахованих)  
data %>% 
  filter(Зарахування.на.навчання != 'Не зараховано') %>% 
  group_by(Рік.завершення.середньої.освіти, Спеціальність.на.яку.подано.заявку) %>%
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(Рік.завершення.середньої.освіти) %>% 
  mutate(rel.freq = paste0(100 * freq/sum(freq))) %>% 
  mutate(rel.freq = round(as.numeric(rel.freq), 2)) %>%
  top_n(3, rel.freq) %>% # завдяки цій команді отримаємо топ-3 спеціальності за кількістю зарахованих для кожного року
  View()

# 3 Найпопулярніші ЗВО (за кількістю зарахованих) 
data %>% 
  filter(Зарахування.на.навчання != 'Не зараховано') %>% 
  group_by(Рік.завершення.середньої.освіти, ЗВО.до.якого.подано.заявку) %>%
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(Рік.завершення.середньої.освіти) %>% 
  mutate(rel.freq = paste0(100 * freq/sum(freq))) %>% 
  mutate(rel.freq = round(as.numeric(rel.freq), 2)) %>%
  top_n(1, freq) %>% # завдяки цій команді отримаємо топ-1 ЗВО за кількістю зарахованих для кожного року
  View()

# 4. Зарахування на бюджет та на контракт ------------------------------------

# наступний блок коду згрупує вступників за десятиріччями та порахує для кожної групи кількість зарахованих на бюджет та на контракт

data %>% 
  filter(Зарахування.на.навчання != 'Не зараховано') %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти == 2019, '2019', 'other')) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти < 2019 & Рік.завершення.середньої.освіти >= 2010, '2010s', year)) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти < 2010 & Рік.завершення.середньої.освіти >= 2000, '2000s', year)) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти < 2000 & Рік.завершення.середньої.освіти >= 1990, '1990s', year)) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти < 1990 & Рік.завершення.середньої.освіти >= 1980, '1980s', year)) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти < 1980 & Рік.завершення.середньої.освіти >= 1970, '1970s', year)) %>% 
  group_by(year, Зарахування.на.навчання) %>%
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rel.freq = paste0(100 * freq/sum(freq))) %>% 
  mutate(rel.freq = round(as.numeric(rel.freq), 2)) %>% 
  View()

# 5. Розподіли конкурсних балів вступників -----------------------

## Порівняємо розподіли для тих, які вступали одразу після закінчення школи та тих, які завершили середню освіту до 2019 року

# 1) Спочатку потрібно виокремити бали для кожного предмету ЗНО

data <- data %>% 
  # для цього спершу поєднаємо назви предметів ЗНО та бали за тести в одну колонку
  unite(all_zno, c(Предмет.1, Бал.1, Предмет.2, Бал.2, Предмет.3, Бал.3, Предмет.4, Бал.4), 
        sep = ' ', remove = TRUE) %>% 
  # потім з неї "витягнемо" потрібні бали в нові колонки; кожна колонка - один предмет 
  mutate(ukr=all_zno %>% str_extract("Українська мова та література.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(math=all_zno %>% str_extract("Математика.*") %>% str_extract('[0-9.]+')) %>% 
  mutate(bio=all_zno %>% str_extract("Біологія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(hist=all_zno %>% str_extract("Історія України.*") %>% str_extract('[0-9.]+')) %>%
  mutate(eng=all_zno %>% str_extract("Англійська мова.*") %>% str_extract('[0-9.]+')) %>%
  mutate(geo=all_zno %>% str_extract("Географія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(him=all_zno %>% str_extract("Хімія.*") %>% str_extract('[0-9.]+')) %>%
  mutate(phiz=all_zno %>% str_extract("Фізика.*") %>% str_extract('[0-9.]+')) %>%
  mutate(inoz=all_zno %>% str_extract("Іноземна мова.*") %>% str_extract('[0-9.]+'))

# 2) Перетворюємо колонки з балами у числовий формат
library(magrittr)
cols = c("ukr", "math", "bio", 'hist', 'eng', 'geo', 'him', 'phiz', 'inoz')
data[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))

# 3) Візуалізація

data %>%
  mutate(year = ifelse(Рік.завершення.середньої.освіти == 2019, 'Випускники 2019 року', 'Випускники попередніх років')) %>% 
  select(year, Конкурсний.бал) %>%
  filter(Конкурсний.бал >= 100 & Конкурсний.бал <= 200) %>%
  ggplot(aes(x = Конкурсний.бал, fill = year)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#ff7800", "#00afbe")) +
  theme_minimal() +
  labs(x = '', y = '', title = 'Розподіл значень конкурсних балів', 
       caption = 'Дані: ЄДЕБО') + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        text = element_text(family = "Open Sans"),
        title = element_text(family = "Open Sans", size = 13, hjust = -3)) 

dev.off()

# Нещодавні випускники мають кращі результати ЗНО з математики
data %>%
  mutate(year = ifelse(Рік.завершення.середньої.освіти == 2019, 'Випускники 2019 року', 'Випускники попередніх років')) %>% 
  mutate(year = ifelse(Рік.завершення.середньої.освіти >= 2014 & Рік.завершення.середньої.освіти < 2019, 'Випускники 2014-2018рр.', year)) %>% 
  select(year, math) %>%
  filter(math >= 100 & math <= 200) %>%
  ggplot(aes(x = math, fill = year)) +
  geom_density(alpha = 0.6) + 
  theme_minimal() +
  labs(x = '', y = '',
       title = 'Розподіл балів ЗНО з математики',
       caption = 'Дані: ЄДЕБО') + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        text = element_text(family = "Open Sans"),
        title = element_text(family = "Open Sans", size = 13, hjust = -3)) +
  facet_wrap(~year)


# 6. Відношення медіанного конкурсного балу та року закінчення середньої освіти ----------------------------------------------------------------------

# Скаттерплот
data %>%
  select(c(Код.абітурієнта, Рік.завершення.середньої.освіти, Конкурсний.бал)) %>% 
  distinct() %>% 
  filter(Конкурсний.бал >= 100 & Конкурсний.бал <= 200) %>%
  group_by(Рік.завершення.середньої.освіти) %>% 
  summarise(median_ball = median(Конкурсний.бал)) %>% 
  ggplot(aes(Рік.завершення.середньої.освіти, median_ball)) + 
  geom_point(size = 2, color = '#00afbe') +
  scale_x_continuous(breaks=seq(1972, 2019, 3)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 9.5, family = 'Open Sans', face = 'bold',
                             hjust = -0.5),
        title = element_text(size = 8, family = 'Open Sans')) + 
  labs(title = "Співвідношення медіанного конкурсного балу та року закінчення середньої освіти", 
       x = '', y = 'бал ЗНО', caption = 'Дані: ЄДЕБО')

dev.off()

