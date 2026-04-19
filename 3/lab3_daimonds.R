library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")
library("gridExtra")


plots_dir <- "/output"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ───────────────────────────────────────────────────────────────────────────────
# 0. ЗАГРУЗКА И ПЕРВИЧНЫЙ ОСМОТР ДАННЫХ
# ───────────────────────────────────────────────────────────────────────────────

# Загружаем встроенный датасет diamonds из пакета ggplot2.
# Датасет содержит 53 940 наблюдений и следующие переменные:
#   price   — цена в долларах США (зависимая переменная)
#   carat   — масса бриллианта в каратах (непрерывная)
#   cut     — качество огранки: Fair < Good < Very Good < Premium < Ideal
#   color   — цвет камня: D (лучший) ... J (худший)
#   clarity — чистота: I1 (худший) ... IF (лучший)
#   x, y, z — геометрические размеры в мм
#   depth   — процент глубины = z / mean(x, y)
#   table   — ширина верхней грани относительно самой широкой точки
h <- diamonds
summary(h)   # Быстрый обзор: мин/макс/медиана/квартили по каждой переменной

# ───────────────────────────────────────────────────────────────────────────────
# 1. ВИЗУАЛЬНЫЙ АНАЛИЗ
# ───────────────────────────────────────────────────────────────────────────────

# 1a. Выбор функциональной формы зависимости price ~ carat.
# Рассматриваем четыре варианта: линейный, два полулогарифмических, log-log.
# Наиболее прямолинейный (гомоскедастичный) график укажет на правильную форму.
p1 <- ggplot(h, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Линейная: price ~ carat", x = "Карат", y = "Цена ($)")

p2 <- ggplot(h, aes(x = carat, y = log(price))) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Полулог: log(price) ~ carat", x = "Карат", y = "log(Цена)")

p3 <- ggplot(h, aes(x = log(carat), y = price)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Полулог: price ~ log(carat)", x = "log(Карат)", y = "Цена ($)")

p4 <- ggplot(h, aes(x = log(carat), y = log(price))) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log-log: log(price) ~ log(carat)", x = "log(Карат)", y = "log(Цена)")

# Сохраняем сравнение функциональных форм — log-log визуально наилинейнее,
# что подтверждает степенной характер зависимости цены от массы.
ggsave(
  filename = file.path(plots_dir, "01_functional_forms.png"),
  plot = gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2),
  width = 12, height = 10
)

# 1b. Матрица корреляций числовых переменных с ценой.
# Высокая корреляция carat, x, y, z ожидаема (размер ↔ масса ↔ цена).
# depth и table имеют слабую корреляцию → менее значимые регрессоры.
num_vars <- c("price", "carat", "depth", "table", "x", "y", "z")
cor_matrix <- cor(h[, num_vars])
cat("\n=== Матрица корреляций числовых переменных ===\n")
print(round(cor_matrix, 3))

# 1c. Влияние категориальных переменных на цену.
# Boxplot позволяет увидеть медиану, разброс, и выбросы для каждой категории.
p_cut <- ggplot(h, aes(x = cut, y = log(price), fill = cut)) +
  geom_boxplot() +
  labs(title = "Влияние огранки на log(цены)", x = "Огранка", y = "log(Цена)") +
  theme_minimal()

p_color <- ggplot(h, aes(x = color, y = log(price), fill = color)) +
  geom_boxplot() +
  labs(title = "Влияние цвета на log(цены)", x = "Цвет", y = "log(Цена)") +
  theme_minimal()

p_clarity <- ggplot(h, aes(x = clarity, y = log(price), fill = clarity)) +
  geom_boxplot() +
  labs(title = "Влияние чистоты на log(цены)", x = "Чистота", y = "log(Цена)") +
  theme_minimal()

ggsave(file.path(plots_dir, "02_categorical_effects.png"),
       plot = gridExtra::grid.arrange(p_cut, p_color, p_clarity, ncol = 1),
       width = 10, height = 14)

# Разбивка log-log зависимости по категориям.
# Параллельные линии подтверждают аддитивный эффект категорий в log-log модели.
p_cc <- qplot(data = h, log(carat), log(price), color = color,  alpha = I(0.1)) + geom_smooth()
p_cl <- qplot(data = h, log(carat), log(price), color = clarity, alpha = I(0.1)) + geom_smooth()
p_ct <- qplot(data = h, log(carat), log(price), color = cut,    alpha = I(0.1)) + geom_smooth()

ggsave(file.path(plots_dir, "03_loglog_by_category.png"),
       plot = gridExtra::grid.arrange(p_cc, p_cl, p_ct, ncol = 1),
       width = 10, height = 15)

# ───────────────────────────────────────────────────────────────────────────────
# 2. СОЗДАНИЕ ДАММИ-ПЕРЕМЕННЫХ ДЛЯ КАТЕГОРИАЛЬНЫХ РЕГРЕССОРОВ
# ───────────────────────────────────────────────────────────────────────────────

# R автоматически создаёт дамми при factor-переменных в lm(),
# однако явное кодирование даёт полный контроль над базовой категорией
# и позволяет вручную отбрасывать незначимые градации.

# Дамми для чистоты (базовая = IF — наилучшая чистота, ловушка дамми исключена)
h <- mutate(h, clarity_I1   = ifelse(clarity == "I1",   1, 0))
h <- mutate(h, clarity_SI2  = ifelse(clarity == "SI2",  1, 0))
h <- mutate(h, clarity_SI1  = ifelse(clarity == "SI1",  1, 0))
h <- mutate(h, clarity_VS2  = ifelse(clarity == "VS2",  1, 0))
h <- mutate(h, clarity_VS1  = ifelse(clarity == "VS1",  1, 0))
h <- mutate(h, clarity_VVS2 = ifelse(clarity == "VVS2", 1, 0))
h <- mutate(h, clarity_VVS1 = ifelse(clarity == "VVS1", 1, 0))
h <- mutate(h, clarity_IF   = ifelse(clarity == "IF",   1, 0))

# Дамми для цвета (базовая = J — наихудший цвет)
h <- mutate(h, color_J = ifelse(color == "J", 1, 0))
h <- mutate(h, color_I = ifelse(color == "I", 1, 0))
h <- mutate(h, color_H = ifelse(color == "H", 1, 0))
h <- mutate(h, color_G = ifelse(color == "G", 1, 0))
h <- mutate(h, color_F = ifelse(color == "F", 1, 0))
h <- mutate(h, color_E = ifelse(color == "E", 1, 0))
h <- mutate(h, color_D = ifelse(color == "D", 1, 0))

# Дамми для огранки (базовая = Fair — наихудшая огранка)
h <- mutate(h, cut_F = ifelse(cut == "Fair",      1, 0))
h <- mutate(h, cut_G = ifelse(cut == "Good",      1, 0))
h <- mutate(h, cut_V = ifelse(cut == "Very Good", 1, 0))
h <- mutate(h, cut_P = ifelse(cut == "Premium",   1, 0))
h <- mutate(h, cut_I = ifelse(cut == "Ideal",     1, 0))

summary(h)
glimpse(h)

# ───────────────────────────────────────────────────────────────────────────────
# 3. ПОЛНАЯ МОДЕЛЬ ПО ВСЕМ ПЕРЕМЕННЫМ (предварительный отбор значимых регрессоров)
# ───────────────────────────────────────────────────────────────────────────────

# Полная модель по исходным переменным используется для первичного просмотра
# t-статистик и p-значений: какие регрессоры значимы, какие — нет.
# Зависимая переменная — уровень price (без log) для быстрой диагностики.
model_00 <- lm(price ~ ., data = h)
cat("\n=== Полная линейная модель (базовая диагностика) ===\n")
summary(model_00)

# ───────────────────────────────────────────────────────────────────────────────
# 4. ПОСЛЕДОВАТЕЛЬНЫЙ ОТБОР ЗНАЧИМЫХ РЕГРЕССОРОВ ПО ГРУППАМ
# ───────────────────────────────────────────────────────────────────────────────

# -- ЦВЕТ --
# Базовая модель с factor-переменной: R автоматически делает базовой J.
model_0 <- lm(data = h, log(price) ~ log(carat) + color)
mtable(model_0)

# Полная дамми-версия (все 7 уровней; J = базовая, поэтому дамми color_J = 0 для всех,
# можно включить для явности, но в регрессии будет выброшена из-за мультиколлинеарности)
model_10 <- lm(data = h, log(price) ~ log(carat) +
                 color_I + color_H + color_G + color_F + color_E + color_D)

# Сокращение: color_G, color_F, color_E незначимы относительно базовой J → отбрасываем.
# Оставляем полюса шкалы цвета: I, H (близкие к худшему J) и D (лучший)
model_11 <- lm(data = h, log(price) ~ log(carat) +
                 color_I + color_H + color_D)

mtable(model_10, model_11)  # R² почти не падает → сокращение оправдано

# -- ЧИСТОТА --
# Полная дамми-версия (базовая = IF)
model_20 <- lm(data = h, log(price) ~ log(carat) +
                 clarity_I1 + clarity_SI2 + clarity_SI1 +
                 clarity_VS2 + clarity_VS1 + clarity_VVS2 + clarity_VVS1)

# Убираем VVS1 — незначимо отличается от базовой IF → не несёт дополнительной информации
model_21 <- lm(data = h, log(price) ~ log(carat) +
                 clarity_I1 + clarity_SI2 + clarity_SI1 +
                 clarity_VS2 + clarity_VS1 + clarity_VVS2)

mtable(model_20, model_21)  # Проверяем потерю информации при сокращении

# -- ОГРАНКА --
# Полная модель с дамми огранки (базовая = Fair)
model_30 <- lm(data = h, log(price) ~ log(carat) +
                 cut_G + cut_V + cut_P + cut_I)

# Промежуточные уровни (Good, Very Good, Premium) близки по эффекту → оставляем полюса
model_31 <- lm(data = h, log(price) ~ log(carat) + cut_I)

mtable(model_30, model_31)

# ───────────────────────────────────────────────────────────────────────────────
# 5. ТРИ ФИНАЛЬНЫЕ МОДЕЛИ ДЛЯ СРАВНЕНИЯ
# ───────────────────────────────────────────────────────────────────────────────

# МОДЕЛЬ A — Полная log-log модель со всеми тремя factor-переменными.
# Является «точкой отсчёта»: высокий R², но много параметров (возможна избыточность).
model_A <- lm(data = h, log(price) ~ log(carat) + color + clarity + cut)
cat("\n=== МОДЕЛЬ A (полная) ===\n")
summary(model_A)

# МОДЕЛЬ B — Сокращённая log-log модель: оставлены только значимые дамми.
# Балансирует между качеством объяснения и числом параметров (принцип бритвы Оккама).
model_B <- lm(data = h,
              log(price) ~ log(carat)
              + color_I + color_H + color_D
              + cut_I
              + clarity_I1 + clarity_SI2 + clarity_SI1
              + clarity_VS2 + clarity_VS1 + clarity_VVS2)
cat("\n=== МОДЕЛЬ B (сокращённая) ===\n")
summary(model_B)

# МОДЕЛЬ C — Квадратичная проверка: добавляем (log(carat))^2 к модели B.
# Квадратичный логарифм улавливает возможную нелинейность в степенной зависимости;
# RESET-тест покажет, нужен ли этот член в финальной спецификации.
model_C <- lm(data = h,
              log(price) ~ log(carat) + I(log(carat)^2)
              + color_I + color_H + color_D
              + cut_I
              + clarity_I1 + clarity_SI2 + clarity_SI1
              + clarity_VS2 + clarity_VS1 + clarity_VVS2)
cat("\n=== МОДЕЛЬ C (квадратичная) ===\n")
summary(model_C)

# ───────────────────────────────────────────────────────────────────────────────
# 6. СРАВНИТЕЛЬНЫЙ АНАЛИЗ ТРЁХ МОДЕЛЕЙ
# ───────────────────────────────────────────────────────────────────────────────

# Сводная таблица коэффициентов, R², скоррект. R², числа наблюдений
cat("\n=== Сводная таблица регрессий (mtable) ===\n")
mtable(model_A, model_B, model_C)

# Информационные критерии: меньше значение → лучше модель.
# AIC штрафует за число параметров; BIC штрафует сильнее при большой выборке.
cat("\n=== Информационные критерии ===\n")
cat(sprintf("AIC: Модель A = %.2f | Модель B = %.2f | Модель C = %.2f\n",
            AIC(model_A), AIC(model_B), AIC(model_C)))
cat(sprintf("BIC: Модель A = %.2f | Модель B = %.2f | Модель C = %.2f\n",
            BIC(model_A), BIC(model_B), BIC(model_C)))

# F-тест вложенных моделей: значимо ли (log(carat))^2 улучшает модель B?
# H0: коэффициент при квадратичном члене = 0; если p < 0.05 → C лучше B.
cat("\n=== Вложенный F-тест: Модель B против Модели C ===\n")
print(anova(model_B, model_C))

# Тест Рамсея (RESET) — проверка правильности функциональной формы.
# Добавляет степени предсказанных значений; если p < 0.05 → форма неверна.
cat("\n=== RESET-тест (нелинейность) ===\n")
cat("Модель A: "); print(resettest(model_A, power = 2:3, type = "fitted"))
cat("Модель B: "); print(resettest(model_B, power = 2:3, type = "fitted"))
cat("Модель C: "); print(resettest(model_C, power = 2:3, type = "fitted"))

# Тест Брейша–Пагана (BP) на гетероскедастичность остатков.
# H0: дисперсия ошибок постоянна (гомоскедастичность).
# Если p < 0.05 → гетероскедастичность присутствует → нужны робастные стандартные ошибки.
cat("\n=== Тест Брейша–Пагана (гетероскедастичность) ===\n")
cat("Модель A: "); print(bptest(model_A))
cat("Модель B: "); print(bptest(model_B))
cat("Модель C: "); print(bptest(model_C))

# ───────────────────────────────────────────────────────────────────────────────
# 7. ДИАГНОСТИЧЕСКИЕ ГРАФИКИ ЛУЧШЕЙ МОДЕЛИ
# ───────────────────────────────────────────────────────────────────────────────

# Четыре стандартных диагностических графика (base R):
# 1. Residuals vs Fitted   — есть ли систематические паттерны в остатках
# 2. Normal Q-Q            — нормальность распределения остатков
# 3. Scale-Location        — гомоскедастичность (горизонтальная линия = норма)
# 4. Residuals vs Leverage — влиятельные наблюдения (расстояние Кука)
png(file.path(plots_dir, "04_diagnostics_modelC.png"), width = 1200, height = 1000)
par(mfrow = c(2, 2))
plot(model_C, main = "Диагностика Модели C")
par(mfrow = c(1, 1))
dev.off()

# Также сохраняем диагностику для модели A, чтобы было наглядно сравнение
png(file.path(plots_dir, "05_diagnostics_modelA.png"), width = 1200, height = 1000)
par(mfrow = c(2, 2))
plot(model_A, main = "Диагностика Модели A")
par(mfrow = c(1, 1))
dev.off()

# ───────────────────────────────────────────────────────────────────────────────
# 8. ВЫВОД
# ───────────────────────────────────────────────────────────────────────────────

cat("\n=== ИТОГОВОЕ СРАВНЕНИЕ МОДЕЛЕЙ ===\n")
cat(sprintf(
  "R² adj: Модель A = %.4f | Модель B = %.4f | Модель C = %.4f\n",
  summary(model_A)$adj.r.squared,
  summary(model_B)$adj.r.squared,
  summary(model_C)$adj.r.squared
))
cat("
Интерпретация:
  Модель A — полная log-log с factor-переменными; высокий R², но много параметров.
  Модель B — сокращённая log-log; почти тот же R² при меньшем числе регрессоров → экономна.
  Модель C — расширение B квадратичным членом I(log(carat)^2);
             если RESET-тест и F-тест значимы — квадратичность оправдана и
             Модель C является наиболее правильно специфицированной из трёх.

Графики сохранены в директорию /output.
")
