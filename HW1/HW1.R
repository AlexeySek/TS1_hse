# install.packages("tsutils")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("dplyr")
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(dplyr)
library(ggfortify)
library(ggplot2)
library(tsutils)
#library(forecast)
set.seed(777)

# ------------------
# ДЗ_1 по курсу АВР_1
# Дата выполнения 25.02.2023
# Студент: Сек Алексей



# ------------------
# Задание 1

# Начнем с 30 наблюдений

# По дефолту константа равна 0, поэтому сначала посчитаем с 0, потом добавим 10
# По итогу получим y_t = 10 + u_t + 3*u_{t-1}
data <- tibble(ma_2_data = 10 + arima.sim(n=30, list(ma=c(3)), sd=2))

# Добавим дату
data$year = 2001:2030
data <- as_tsibble(data, index = year)

# Посмотрим, корректная ли табличка получилась
data

# Посмотрим на график процесса
p <- ggplot(data, aes(x=year, y=ma_2_data)) +
  geom_line() + 
  ggtitle('Траектория процесса для 30 наблюдений') + 
  xlab('year') + 
  ylab('value')
p

# Посмотроим ACF для данного процесса
ACF(data)
acf <- ACF(data, ma_2_data, lag_max=10) %>% autoplot()
acf + 
  ggtitle('ACF для MA(2) - первые 10 значений') + 
  xlab('Lag') + 
  ylab('ACF')


# Посмотроим PACF для данного процесса
PACF(data)
pacf <- PACF(data, ma_2_data, lag_max=10) %>% autoplot()
pacf + 
  ggtitle('PACF для MA(2) - первые 10 значений') + 
  xlab('Lag') + 
  ylab('PACF')



# ------------------

# Теперь посмотрим 300 наблюдений

# По дефолту константа равна 0, поэтому сначала посчитаем с 0, потом добавим 10
# По итогу получим y_t = 10 + u_t + 3*u_{t-1}
data <- tibble(ma_2_data = 10 + arima.sim(n=300, list(ma=c(3)), sd=2))

# Добавим дату
data$year = 2001:2300
data <- as_tsibble(data, index = year)

# Посмотрим, корректная ли табличка получилась
data

# Посмотрим на график процесса
p <- ggplot(data, aes(x=year, y=ma_2_data)) +
  geom_line() + 
  ggtitle('Траектория процесса для 300 наблюдений') + 
  xlab('year') + 
  ylab('value')
p

# Посмотроим ACF для данного процесса
ACF(data)
acf <- ACF(data, ma_2_data, lag_max=10) %>% autoplot()
acf + 
  ggtitle('ACF для MA(2) - первые 10 значений') + 
  xlab('Lag') + 
  ylab('ACF')


# Посмотроим PACF для данного процесса
PACF(data)
pacf <- PACF(data, ma_2_data, lag_max=10) %>% autoplot()
pacf + 
  ggtitle('PACF для MA(2) - первые 10 значений') + 
  xlab('Lag') + 
  ylab('PACF')





# ------------------
# Задание 2

# Начнем с 30 наблюдений
data <- tibble(random_walk_drift = 
                 10 + arima.sim(
                   model=list(order = c(0,1,0)),n=29, mean=1, sd=2
                   )
               )

# Добавим дату
data$year = 2001:2030
data <- as_tsibble(data, index = year)

# Посмотрим, корректная ли табличка получилась
data

# Посмотрим на график процесса
p <- ggplot(data, aes(x=year, y=random_walk_drift)) +
  geom_line() + 
  ggtitle('Траектория процесса для 30 наблюдений') + 
  xlab('year') + 
  ylab('value')
p

# Посмотроим ACF для данного процесса
ACF(data)
acf <- ACF(data, random_walk_drift, lag_max=10) %>% autoplot()
acf + 
  ggtitle('ACF для RW_drift - первые 10 значений') + 
  xlab('Lag') + 
  ylab('ACF')


# Посмотроим PACF для данного процесса
PACF(data)
pacf <- PACF(data, random_walk_drift, lag_max=10) %>% autoplot()
pacf + 
  ggtitle('PACF для RW_drift - первые 10 значений') + 
  xlab('Lag') + 
  ylab('PACF')



# ------------------
# Рассмотрим 300 наблюдений
data <- tibble(random_walk_drift = 
                 10 + arima.sim(
                   model=list(order = c(0,1,0)),n=299, mean=1, sd=2))

# Добавим дату
data$year = 2001:2300
data <- as_tsibble(data, index = year)

# Посмотрим, корректная ли табличка получилась
data

# Посмотрим на график процесса
p <- ggplot(data, aes(x=year, y=random_walk_drift)) +
  geom_line() + 
  ggtitle('Траектория процесса для 300 наблюдений') + 
  xlab('year') + 
  ylab('value')
p

# Посмотроим ACF для данного процесса
ACF(data)
acf <- ACF(data, random_walk_drift, lag_max=10) %>% autoplot()
acf + 
  ggtitle('ACF для RW_drift - первые 10 значений') + 
  xlab('Lag') + 
  ylab('ACF')


# Посмотроим PACF для данного процесса
PACF(data)
pacf <- PACF(data, random_walk_drift, lag_max=10) %>% autoplot()
pacf + 
  ggtitle('PACF для RW_drift - первые 10 значений') + 
  xlab('Lag') + 
  ylab('PACF')





# ------------------
# Задание 3
# Данные по реальной заработной плате: 
# http://sophist.hse.ru/hse/1/tables/WAG_Y.htm

# Загрузим данные
df_wages = import(
  '//Users/alexeysek/Downloads/2. TS_1_online/HW1/wages.csv', dec = ",")
df_wages = as_tsibble(df_wages, index = year)
glimpse(df_wages)


# Посмотрим на график процесса
p <- ggplot(df_wages, aes(x=year, y=real_wage)) +
  geom_line() + 
  ggtitle('Траектория процесса real_wages') + 
  xlab('year') + 
  ylab('real_wage')
p

# Посмотроим ACF для данного процесса
ACF(df_wages)
acf <- ACF(df_wages, real_wage, lag_max=10) %>% autoplot()
acf + 
  ggtitle('ACF для real_wages - первые 10 значений') + 
  xlab('Lag') + 
  ylab('ACF')


# Посмотроим PACF для данного процесса
PACF(df_wages)
pacf <- PACF(df_wages, real_wage, lag_max=10) %>% autoplot()
pacf + 
  ggtitle('PACF для real_wages - первые 10 значений') + 
  xlab('Lag') + 
  ylab('PACF')


# Визуально тренд есть - очень заметно по графику, что он восходящий
# Тренд похоже есть -> процесс не похож на стационарный
# т.к. среднее должно быть постоянным для стационарности


# Оценим ETS(AAN) 
ets_aan = model(
  df_wages, ets_aan = ETS(real_wage ~ error('A') + trend('A') + season('N')))
report(ets_aan)

# Получили следующие оценки параметров модели ETS(AAN):
# alpha = 0.9999 
# beta  = 0.0001000268 
# l_0 = 88.02125
# b_0 = 4.67952
# sigma^2 = 142.4486

fcst <- forecast(ets_aan, h = '2 years', level=c(80))
fcst

# Построим график прогноза с 80% доверительным интервало
autoplot(fcst, level=c(80)) + 
  ggtitle('Прогноз реальной заработной платы + доверительный интервал 80%')

# Почему-то стандартная функция forecast в fpp3 не выдает ДИ
# Такое есть только в https://otexts.com/fpp2/the-forecast-package-in-r.html

# Поэтому ручками посчитал доверительные интервалы
# Параметры брал из fcst - там есть mean и var
print('CI_80% Lower bound - 1 step forward:')
print(236 - 1.282 * sqrt(142))
print('CI_80% Upper bound - 1 step forward:')
print(236 + 1.282 * sqrt(142))      

print('CI_80% Lower bound - 2 steps forward:')
print(241 - 1.282 * sqrt(285))
print('CI_80% Upper bound - 2 steps forward:')
print(241 + 1.282 * sqrt(285))     
      
# Чтобы не было пробела между данными и прогнозом
# Какой-то баг библиотеки, как мне кажется, что появляется этот пробел
df_wages <- df_wages %>% 
  add_row(year=2022, real_wage=236)

autoplot(fcst, df_wages) + 
  ggtitle('Реальная заработная плата + прогноз на два года')





# ------------------
# Задание 4
# Сезонный ряд месячной периодичности
# Цены на яблоки в РФ: 
# https://www.kaggle.com/datasets/kapatsa/apple-prices-in-russian-regions

# Загрузим данные
apple_prices <- import(
  '//Users/alexeysek/Downloads/2. TS_1_online/HW1/apple_prices.csv')
# Преобразуем дату в нормальный формат
apple_prices$date <- yearmonth(ymd(apple_prices$date))
apple_prices <- as_tsibble(apple_prices, index=date)
# Возьмем ряд только по Москве
apple_prices <- apple_prices[ ,c('date', 'Moscow')]
apple_prices


# Построим графики STL разложений с разной силой сглаживания сезонности
model(apple_prices,
  STL(Moscow ~ trend() + season(), robust = TRUE)) |> components() |> 
  autoplot() + ggtitle('Дефолтные параметры')

model(apple_prices,
      STL(Moscow ~ trend() + season(window = 1), robust = TRUE)) |> 
  components() |> autoplot() + ggtitle('window = 1')

model(apple_prices,
      STL(Moscow ~ trend() + season(window = 2), robust = TRUE)) |> 
  components() |> autoplot() + ggtitle('window = 2')

model(apple_prices,
      STL(Moscow ~ trend() + season(window = 4), robust = TRUE)) |> 
  components() |> autoplot() + ggtitle('window = 4')

model(apple_prices,
      STL(Moscow ~ trend() + season(window = 6), robust = TRUE)) |> 
  components() |> autoplot() + ggtitle('window = 6')

model(apple_prices,
      STL(Moscow ~ trend() + season(window = 12), robust = TRUE)) |> 
  components() |> autoplot() + ggtitle('window = 12')



# ETS(AAA) разложение
# Другими словами - аддитивный метод Хольта-Винтерса
autoplot(decompose(as.ts(apple_prices, frequency=12))) + 
  ggtitle('ETS(AAA) decomposition') + 
  xlab('Year_Month')


# Поделим на тестовую и тренировочную выборки
# В тестовой выборке 2 последних года
train <- filter(apple_prices, date >= yearmonth(ymd('2013-04-01')))
train <- filter(train, date <= yearmonth(ymd('2018-03-01')))
test <- filter(apple_prices, date > yearmonth(ymd('2018-03-01')))
print(train, n=100)
print(test, n=100)


# Обучим указанные в задании модели
mods = model(train,
             ets_aaa = ETS(Moscow ~ error('A') + trend('A') + season('A')),
             ets_mam = ETS(Moscow ~ error('M') + trend('A') + season('M')),
             snaive = SNAIVE(Moscow),
             theta = THETA(Moscow),
             ets_aaa_ln = ETS(log(Moscow) ~ error('A') + trend('A') + 
                                season('A'))
             )


# Сделаем прогноз и сравним результаты
# Особенно нам интересно MASE
fcst = forecast(mods, h = 24)
accuracy(fcst, apple_prices)

# Построим график
autoplot(fcst, filter(apple_prices, date >= yearmonth(ymd('2017-01-01'))))

# Две лучшие по MASE модели (min MASE) - это snaive и theta 
# Усредним их прогнозы
mod_aggr = mutate(mods, average_models = (snaive + theta) / 2)
fcst = forecast(mod_aggr, h = 24)
accuracy(fcst, apple_prices)

# Усредненная модель оказалось посередине между 1 и 2 местом
# То есть нам не удалось обыграть оба усредняемых подхода, 
# т.к. snaive все равно остался лучшим







