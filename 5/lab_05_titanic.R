# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 05 – Логит-модель: предсказание выживаемости на Титанике

library("dplyr")    # манипуляции с данными
library("erer")     # расчет предельных эффектов
library("vcd")      # графики для качественных данных
library("ggplot2")  # графики
library("reshape2") # манипуляции с данными
library("AUC")      # для ROC кривой и AUC-метрики
library("lmtest")   # для LR-теста (lrtest)
library("showtext")

font_add("FreeSans", "/usr/share/fonts/truetype/freefont/FreeSans.ttf")
showtext_auto()

# при загрузке файлов R автоматом переделывает все строковые переменные в
# факторные эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# Директория для сохранения графиков (примонтирована в docker-compose.yml)
plots_dir <- "/output"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ──────────────────────────────────────────────────────────────────────────────
# 1. ЗАГРУЗКА И ПЕРВИЧНЫЙ ОСМОТР ДАННЫХ
# ──────────────────────────────────────────────────────────────────────────────

# читаем данные по пассажирам Титаника
t <- read.csv("titanic3.csv")
# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

# смотрим на набор данных
glimpse(t)

# объясняем R, какие переменные считать факторными:
# survived — зависимая переменная (0/1 → фактор для glm с binomial)
# sex, pclass — категориальные регрессоры
t <- mutate(t,
            sex      = as.factor(sex),
            pclass   = as.factor(pclass),
            survived = as.factor(survived))
summary(t)

# ──────────────────────────────────────────────────────────────────────────────
# 2. ВИЗУАЛЬНЫЙ АНАЛИЗ ЗАВИСИМОСТЕЙ
# ──────────────────────────────────────────────────────────────────────────────

# Мозаичный график: наглядно показывает зависимость выживаемости
# от пола и класса кабины — ключевые предикторы модели
png(file.path(plots_dir, "01_mosaic_sex_pclass_survived.png"),
    width = 900, height = 700)
mosaic(data = t, ~sex + pclass + survived, shade = TRUE,
       main = "Мозаичный график: пол × класс × выживание")
dev.off()

# График-виолончель: распределение возраста среди выживших и погибших
p_violin <- ggplot(t, aes(x = survived, y = age, fill = survived)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.size = 1) +
  labs(title = "Распределение возраста по исходу",
       x = "Выжил (1) / Погиб (0)", y = "Возраст") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(file.path(plots_dir, "02_violin_age_survived.png"),
       plot = p_violin, width = 7, height = 5)

# Сглаженные функции плотности возраста по группам:
# «stack» — абсолютные числа, «fill» — доли (доля выживших по возрасту)
p_dens_stack <- ggplot(t, aes(x = age, y = after_stat(count), fill = survived)) +
  geom_density(position = "stack", alpha = 0.6) +
  labs(title = "Плотность возраста (абсолютная, stack)",
       x = "Возраст", y = "Количество пассажиров") +
  theme_minimal()

p_dens_fill <- ggplot(t, aes(x = age, y = after_stat(count), fill = survived)) +
  geom_density(position = "fill", alpha = 0.6) +
  labs(title = "Плотность возраста (доля, fill)",
       x = "Возраст", y = "Доля") +
  theme_minimal()

ggsave(file.path(plots_dir, "03_density_age_stack.png"),
       plot = p_dens_stack, width = 7, height = 4)
ggsave(file.path(plots_dir, "04_density_age_fill.png"),
       plot = p_dens_fill, width = 7, height = 4)

# ──────────────────────────────────────────────────────────────────────────────
# 3. ОЦЕНИВАНИЕ ЛОГИТ И ПРОБИТ МОДЕЛЕЙ
# ──────────────────────────────────────────────────────────────────────────────

# Логит-модель: связующая функция logit, P(survived=1 | X)
# x=TRUE — сохраняем матрицу регрессоров для последующих расчётов предельных эффектов
m_logit <- glm(data = t,
               survived ~ sex + age + pclass + fare,
               family = binomial(link = "logit"),
               x = TRUE)

# Пробит-модель: альтернативная связующая функция (нормальная CDF)
m_probit <- glm(data = t,
                survived ~ sex + age + pclass + fare,
                family = binomial(link = "probit"),
                x = TRUE)

# Отчёты об оценке: коэффициенты, стандартные ошибки, z-тест, AIC
cat("\n=== ЛОГИТ-МОДЕЛЬ ===\n")
summary(m_logit)
cat("\n=== ПРОБИТ-МОДЕЛЬ ===\n")
summary(m_probit)

# Ковариационная матрица оценок коэффициентов логит-модели
cat("\n=== Ковариационная матрица (логит) ===\n")
print(vcov(m_logit))

# ──────────────────────────────────────────────────────────────────────────────
# 4. ПРОГНОЗИРОВАНИЕ: ВЕРОЯТНОСТЬ ВЫЖИВАНИЯ ДЛЯ МУЖЧИН 2-ГО КЛАССА
# ──────────────────────────────────────────────────────────────────────────────

# Синтетический набор данных: мужчины 2-го класса, fare=100,
# возраст меняется от 5 до 100 лет
newdata <- data.frame(
  age    = seq(from = 5, to = 100, length = 100),
  sex    = "male",
  pclass = "2nd",
  fare   = 100
)

# Прогноз на шкале линейного предиктора (fit) + стандартные ошибки (se.fit)
pr_logit    <- predict(m_logit, newdata, se = TRUE)
newdata_pr  <- cbind(newdata, pr_logit)

# Переводим линейный предиктор в вероятности через логистическую функцию;
# доверительный интервал строится на шкале логита, затем обратно преобразуется
newdata_pr <- mutate(newdata_pr,
                     prob     = plogis(fit),
                     left_ci  = plogis(fit - 1.96 * se.fit),
                     right_ci = plogis(fit + 1.96 * se.fit))

p_forecast <- ggplot(newdata_pr, aes(x = age, y = prob)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_ribbon(aes(ymin = left_ci, ymax = right_ci), alpha = 0.2, fill = "steelblue") +
  labs(title = "Вероятность выживания: мужчина 2-го класса (fare=100)",
       x = "Возраст", y = "P(выжил)") +
  theme_minimal()
ggsave(file.path(plots_dir, "05_forecast_survival_prob.png"),
       plot = p_forecast, width = 7, height = 4)

# ──────────────────────────────────────────────────────────────────────────────
# 5. LR-ТЕСТ: ЗНАЧИМОСТЬ ПРЕДИКТОРОВ pclass и fare
# ──────────────────────────────────────────────────────────────────────────────

# Для LR-теста обе модели должны оцениваться на одинаковом наборе наблюдений:
# убираем строки с NA по используемым переменным
t2 <- select(t, sex, age, pclass, survived, fare) %>% na.omit()

# Ограниченная модель: только пол и возраст
m_logit2 <- glm(data = t2,
                survived ~ sex + age,
                family = binomial(link = "logit"),
                x = TRUE)

# H₀: коэффициенты при pclass и fare равны нулю (не улучшают модель)
cat("\n=== LR-тест: m_logit vs m_logit2 (H0: beta_pclass = beta_fare = 0) ===\n")
print(lrtest(m_logit, m_logit2))

# ──────────────────────────────────────────────────────────────────────────────
# 6. ПРЕДЕЛЬНЫЕ ЭФФЕКТЫ
# ──────────────────────────────────────────────────────────────────────────────

# Предельные эффекты для «среднего» пассажира (все регрессоры = своим средним):
# показывают, как изменяется вероятность выжить при +1 к предиктору
cat("\n=== Предельные эффекты (в средних значениях) ===\n")
print(maBina(m_logit))

# Усредненные предельные эффекты по всем пассажирам:
# вычисляется предельный эффект для каждого наблюдения, затем берётся среднее
cat("\n=== Усредненные предельные эффекты (AME) ===\n")
print(maBina(m_logit, x.mean = FALSE))

# ──────────────────────────────────────────────────────────────────────────────
# 7. МНК-МОДЕЛЬ ДЛЯ СРАВНЕНИЯ (линейная вероятностная модель)
# ──────────────────────────────────────────────────────────────────────────────

# Линейная вероятностная модель: прогнозы могут выходить за [0,1],
# но служит ориентиром для сравнения с логит-моделью
m_ols <- lm(data = t, as.numeric(survived) ~ sex + age + pclass + fare)
cat("\n=== МНК (линейная вероятностная модель) ===\n")
summary(m_ols)

# ──────────────────────────────────────────────────────────────────────────────
# 8. ROC-КРИВАЯ И ВЫБОР ОПТИМАЛЬНОГО ПОРОГА ОТСЕЧЕНИЯ
# ──────────────────────────────────────────────────────────────────────────────

# Прогнозируем скрытую переменную на исходном наборе данных
pr_t <- predict(m_logit, t, se = TRUE)
t    <- cbind(t, pr_t)
# Переводим линейный предиктор в вероятности
t    <- mutate(t, prob = plogis(fit))

# Данные для ROC-кривой: cutoffs, tpr (чувствительность), fpr (1−специфичность)
roc.data <- roc(t$prob, t$survived)
cat("\n=== Структура объекта ROC ===\n")
str(roc.data)

# AUC (площадь под ROC-кривой): ближе к 1 → лучше модель
cat(sprintf("\n=== AUC логит-модели: %.4f ===\n", AUC::auc(roc.data)))

# ── Поиск оптимального порога по индексу Юдена ──
# Индекс Юдена J = Чувствительность + Специфичность − 1
#             J = TPR − FPR  (максимум = идеальная классификация)
# Максимальный J соответствует наилучшему балансу между чувствительностью
# и специфичностью, без предпочтения одной из них
youden_j     <- roc.data$tpr - roc.data$fpr
best_idx     <- which.max(youden_j)
best_cutoff  <- roc.data$cutoffs[best_idx]
best_tpr     <- roc.data$tpr[best_idx]   # чувствительность (Sensitivity)
best_fpr     <- roc.data$fpr[best_idx]   # 1 − специфичность
best_spec    <- 1 - best_fpr              # специфичность (Specificity)

cat(sprintf(
  "\n=== ОПТИМАЛЬНЫЙ ПОРОГ (индекс Юдена) ===\n  Порог        : %.4f\n  Чувствительность: %.4f\n  Специфичность   : %.4f\n  Индекс Юдена J  : %.4f\n",
  best_cutoff, best_tpr, best_spec, youden_j[best_idx]
))

# ── Матрица ошибок при оптимальном пороге ──
# Классифицируем пассажиров: вероятность ≥ порога → предсказываем «выжил»
t_clean    <- t[!is.na(t$prob) & !is.na(t$survived), ]
pred_class <- ifelse(t_clean$prob >= best_cutoff, "1", "0")
conf_mat   <- table(Факт = t_clean$survived, Прогноз = pred_class)
cat("\n=== МАТРИЦА ОШИБОК (при оптимальном пороге) ===\n")
print(conf_mat)

# Точность модели (доля верных классификаций)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat(sprintf("Точность (Accuracy): %.4f\n", accuracy))

# ──────────────────────────────────────────────────────────────────────────────
# 9. ГРАФИКИ ROC-АНАЛИЗА
# ──────────────────────────────────────────────────────────────────────────────

# График 1: Чувствительность (TPR) vs порог — как меняется чувствительность
p_tpr <- ggplot(data.frame(cutoff = roc.data$cutoffs, tpr = roc.data$tpr),
                aes(x = cutoff, y = tpr)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = best_cutoff, color = "red", linetype = "dashed") +
  annotate("point", x = best_cutoff, y = best_tpr, color = "red", size = 3) +
  labs(title = "Чувствительность (TPR) vs порог отсечения",
       x = "Порог", y = "TPR (Чувствительность)") +
  theme_minimal()
ggsave(file.path(plots_dir, "06_roc_tpr_vs_cutoff.png"),
       plot = p_tpr, width = 7, height = 4)

# График 2: FPR (1−специфичность) vs порог
p_fpr <- ggplot(data.frame(cutoff = roc.data$cutoffs, fpr = roc.data$fpr),
                aes(x = cutoff, y = fpr)) +
  geom_line(color = "tomato", linewidth = 1) +
  geom_vline(xintercept = best_cutoff, color = "red", linetype = "dashed") +
  annotate("point", x = best_cutoff, y = best_fpr, color = "red", size = 3) +
  labs(title = "FPR (1−Специфичность) vs порог отсечения",
       x = "Порог", y = "FPR (1−Специфичность)") +
  theme_minimal()
ggsave(file.path(plots_dir, "07_roc_fpr_vs_cutoff.png"),
       plot = p_fpr, width = 7, height = 4)

# График 3: ROC-кривая (FPR vs TPR) + оптимальная точка + диагональ случайного классификатора
p_roc <- ggplot(data.frame(fpr = roc.data$fpr, tpr = roc.data$tpr),
                aes(x = fpr, y = tpr)) +
  geom_line(color = "steelblue", linewidth = 1) +
  # диагональ: классификатор наугад (AUC = 0.5, ориентир для сравнения)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  # оптимальная точка по индексу Юдена
  annotate("point", x = best_fpr, y = best_tpr, color = "red", size = 4) +
  annotate("text",
           x = best_fpr + 0.05, y = best_tpr - 0.04,
           label = sprintf("Порог=%.2f\nTPR=%.2f, Spec=%.2f",
                           best_cutoff, best_tpr, best_spec),
           size = 3.5, color = "red") +
  labs(title = sprintf("ROC-кривая логит-модели  (AUC = %.4f)", AUC::auc(roc.data)),
       x = "FPR (1−Специфичность)", y = "TPR (Чувствительность)") +
  theme_minimal()
ggsave(file.path(plots_dir, "08_roc_curve.png"),
       plot = p_roc, width = 7, height = 5)

# График 4: Индекс Юдена J vs порог — наглядно показывает оптимум
p_youden <- ggplot(data.frame(cutoff = roc.data$cutoffs, j = youden_j),
                   aes(x = cutoff, y = j)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  geom_vline(xintercept = best_cutoff, color = "red", linetype = "dashed") +
  annotate("point", x = best_cutoff, y = youden_j[best_idx], color = "red", size = 3) +
  labs(title = "Индекс Юдена J vs порог отсечения",
       subtitle = sprintf("Оптимальный порог: %.4f  (J = %.4f)", best_cutoff, youden_j[best_idx]),
       x = "Порог", y = "J = TPR − FPR") +
  theme_minimal()
ggsave(file.path(plots_dir, "09_youden_index.png"),
       plot = p_youden, width = 7, height = 4)

# ──────────────────────────────────────────────────────────────────────────────
# 10. ИТОГОВЫЙ ОТЧЁТ
# ──────────────────────────────────────────────────────────────────────────────

cat("\n╔══════════════════════════════════════════════════════════╗")
cat("\n║           ИТОГОВЫЕ ПОКАЗАТЕЛИ ЛОГИТ-МОДЕЛИ               ║")
cat("\n╠══════════════════════════════════════════════════════════╣")
cat(sprintf("\n║  AUC                  : %.4f                          ║", AUC::auc(roc.data)))
cat(sprintf("\n║  Оптимальный порог    : %.4f                          ║", best_cutoff))
cat(sprintf("\n║  Чувствительность     : %.4f                          ║", best_tpr))
cat(sprintf("\n║  Специфичность        : %.4f                          ║", best_spec))
cat(sprintf("\n║  Индекс Юдена J       : %.4f                          ║", youden_j[best_idx]))
cat(sprintf("\n║  Точность (Accuracy)  : %.4f                          ║", accuracy))
cat("\n╚══════════════════════════════════════════════════════════╝\n")
cat("\nГрафики сохранены в директорию /output\n")
