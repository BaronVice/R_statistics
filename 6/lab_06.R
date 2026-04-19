library(dplyr)
library(cluster)
library(ggplot2)
library(reshape2)
library(gridExtra)

# ── Пути к данным и к директории для сохранения графиков ─────────────────────
data_dir <- "Clusters"
out_dir  <- "/output"

# ── Типы столбцов CSV: 7 мета-полей (фактор/строка) + числовые столбцы лет ───
# Годы в файлах идут от 2014 до 1980 (35 столбцов), итого 42 колонки.
# Строгое указание типов ускоряет чтение и предотвращает неверное преобразование.
# Метаданные: Sector, Region, Company, Industry, Country, Ticker, Currency
colTypes <- c("factor", "factor", "character",
              "factor", "factor", "character", "factor",
              rep("numeric", 42 - 7))   # 42 колонки всего, 7 мета + 35 числовых

# ── Вспомогательная функция загрузки одного CSV-показателя ───────────────────
load_csv <- function(name) {
  read.csv(file.path(data_dir, paste0(name, ".csv")),
           header = TRUE, sep = ";", dec = ",",
           colClasses = colTypes)
}

# ── Загрузка всех необходимых финансовых показателей ─────────────────────────
Assets   <- load_csv("Assets")
Debt     <- load_csv("Debt")
Equity   <- load_csv("Equity")
Profit   <- load_csv("Profit")
Interest <- load_csv("Interest")
Goodwill <- load_csv("Goodwill")
PPE      <- load_csv("PPE")
Depr     <- load_csv("Depr")
NCA      <- load_csv("NCA")
OInc     <- load_csv("OInc")
Sales    <- load_csv("Sales")
Treasure <- load_csv("Treasure")

cat("Данные загружены. Компаний в каждом файле:", nrow(Assets), "\n")

# ── Границы фильтрации выбросов ───────────────────────────────────────────────
# Выбросы (экстремальные значения) искажают центроиды кластеров, поэтому
# наблюдения за пределами содержательного диапазона исключаются.
bnd <- c(L_lo = -1,   L_hi = 10,
         R_lo = -0.3, R_hi = 0.6,
         P_lo = -0.5, P_hi = 1.1)

# ── Функция построения таблицы признаков для одного года ─────────────────────
# Каждая строка — одна компания; признаки — безразмерные финансовые коэффициенты,
# что позволяет сравнивать компании разных регионов и размеров.
make_year_df <- function(year) {
  xy <- paste0("X", year)   # имя столбца в CSV (например, "X2007")

  data.frame(
    Company = Assets$Company,
    Sector  = Assets$Sector,
    Region  = Assets$Region,
    # Леверидж: долговая нагрузка относительно собственного капитала
    L     = Debt[[xy]]    / Equity[[xy]],
    # ROCE: рентабельность задействованного капитала (доход на весь капитал)
    R     = (Profit[[xy]] + Interest[[xy]]) / (Debt[[xy]] + Equity[[xy]]),
    # Гудвилл: доля нематериальных активов (гудвилла) в собственном капитале
    G     = Goodwill[[xy]] / Equity[[xy]],
    # Доля финансовых активов среди необоротных (чем больше — тем меньше реальных)
    F_val = 1 - (PPE[[xy]] - Depr[[xy]] + Goodwill[[xy]]) / NCA[[xy]],
    # Доля выкупленных акций (байбек): снижает реальный акционерный капитал
    T_val = -Treasure[[xy]] / (Equity[[xy]] - Treasure[[xy]]),
    # ROS: рентабельность продаж по операционной прибыли
    P     = OInc[[xy]] / Sales[[xy]],
    Year  = factor(year),
    stringsAsFactors = FALSE
  )
}

# ── Объединяем данные за два последовательных года: 2007 и 2008 ───────────────
# Объединение двух лет удваивает выборку и позволяет оценить устойчивость
# кластерных паттернов во времени.
df_all <- bind_rows(make_year_df(2007), make_year_df(2008))

# ── Удаление NA, Inf и наблюдений за пределами содержательного диапазона ──────
# NA/Inf возникают при делении на ноль (например, отрицательный или нулевой
# капитал). Такие строки бессмысленны для анализа расстояний.
feat_cols <- c("L", "R", "G", "F_val", "T_val", "P")

df_clean <- df_all %>%
  filter(
    # if_all() — правильный способ проверить условие по нескольким столбцам в filter()
    # across() возвращает data.frame и не работает напрямую в filter() в новом dplyr
    if_all(all_of(feat_cols), is.finite),
    L > bnd["L_lo"], L < bnd["L_hi"],
    R > bnd["R_lo"], R < bnd["R_hi"],
    P > bnd["P_lo"], P < bnd["P_hi"]
  )

cat("Наблюдений после очистки:", nrow(df_clean), "\n")
cat("  из них 2007:", sum(df_clean$Year == 2007), "\n")
cat("  из них 2008:", sum(df_clean$Year == 2008), "\n\n")

# ── Стандартизация признаков перед кластеризацией ────────────────────────────
# Масштабирование необходимо, чтобы признаки с большим абсолютным разбросом
# (например, Леверидж 0—10) не доминировали над признаками малого масштаба
# при вычислении евклидовых расстояний.
d_scaled <- as.data.frame(scale(df_clean[feat_cols]))

# ── Определение оптимального числа кластеров по среднему силуэту ──────────────
# Коэффициент силуэта s(i) = (b(i) - a(i)) / max(a(i), b(i))
#   a(i) — средняя дистанция внутри своего кластера
#   b(i) — средняя дистанция до ближайшего «чужого» кластера
# Среднее s(i) по всем точкам: числовой критерий качества разбиения.
k_range    <- 2:8
sil_widths <- sapply(k_range, function(k) {
  ck <- pam(d_scaled, k, nstart = 5)   # nstart=5 для устойчивости результата
  summary(silhouette(ck))$avg.width
})
names(sil_widths) <- k_range

cat("Средние значения силуэта по числу кластеров:\n")
print(round(sil_widths, 4))

# Выбираем k с максимальным средним силуэтом — наилучшее разделение
k_opt <- k_range[which.max(sil_widths)]
cat("\nОптимальное число кластеров:", k_opt, "\n\n")

# ── График выбора числа кластеров (кривая силуэта) ───────────────────────────
png(file.path(out_dir, "01_silhouette_k_selection.png"), width = 800, height = 500)
sil_df <- data.frame(k = k_range, AvgSilhouette = sil_widths)
p1 <- ggplot(sil_df, aes(x = k, y = AvgSilhouette)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3.5, color = "steelblue") +
  geom_vline(xintercept = k_opt, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = k_opt + 0.2, y = min(sil_widths),
           label = paste("k =", k_opt), hjust = 0, color = "red", size = 4) +
  labs(title = "Выбор числа кластеров: средний коэффициент силуэта",
       subtitle = "Пунктирная линия — оптимальное k",
       x = "Число кластеров k", y = "Средний коэффициент силуэта") +
  theme_bw(base_size = 13)
print(p1)
dev.off()

# ── PAM-кластеризация с оптимальным числом кластеров ─────────────────────────
# PAM (Partitioning Around Medoids) устойчивее к выбросам, чем k-means,
# так как медоид — реальное наблюдение, а не арифметическое среднее.
ck_opt <- pam(d_scaled, k_opt, nstart = 5)
df_clean <- mutate(df_clean, cl = factor(ck_opt$cluster))

cat("Размеры кластеров:\n")
print(table(df_clean$cl))
cat("\n")

# ── Силуэтный график итоговой кластеризации ───────────────────────────────────
# Визуализирует качество принадлежности каждого наблюдения своему кластеру:
# столбики — индивидуальные силуэты, сгруппированные по кластерам.
png(file.path(out_dir, "02_silhouette_final.png"), width = 900, height = 650)
plot(silhouette(ck_opt),
     col  = 2:(k_opt + 1),
     main = paste0("Силуэты PAM-кластеров (k = ", k_opt, ", 2007–2008)"),
     border = NA)
dev.off()

# ── Диаграмма рассеяния: Леверидж vs ROS, цвет — кластер, форма — год ────────
# Позволяет оценить расположение кластеров в плоскости двух ключевых признаков
# и проверить, не смешиваются ли данные разных лет внутри кластеров.
png(file.path(out_dir, "03_scatter_L_vs_P.png"), width = 900, height = 600)
p3 <- ggplot(df_clean, aes(x = L, y = P, color = cl, shape = Year)) +
  geom_point(alpha = 0.6, size = 2.2) +
  labs(title = "Кластеры компаний: Леверидж vs ROS (2007–2008)",
       x = "Леверидж  (Debt / Equity)",
       y = "ROS  (OInc / Sales)",
       color = "Кластер", shape = "Год") +
  theme_bw(base_size = 13) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         shape = guide_legend(override.aes = list(size = 3)))
print(p3)
dev.off()

# ── Scatter: Леверидж vs ROCE по регионам ─────────────────────────────────────
# Фасетирование по регионам раскрывает географическую структуру кластеров:
# наблюдается ли группировка по регионам или кластеры охватывают несколько регионов.
png(file.path(out_dir, "04_scatter_by_region.png"), width = 1100, height = 750)
p4 <- ggplot(df_clean, aes(x = L, y = R, color = cl, shape = Year)) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_wrap(~Region) +
  labs(title = "Кластеры по регионам: Леверидж vs ROCE (2007–2008)",
       x = "Леверидж", y = "ROCE",
       color = "Кластер", shape = "Год") +
  theme_bw(base_size = 11)
print(p4)
dev.off()

# ── Scatter: Леверидж vs ROS по секторам ─────────────────────────────────────
# Фасетирование по секторам проверяет, не является ли кластер просто
# «срезом» одного отраслевого сектора.
png(file.path(out_dir, "05_scatter_by_sector.png"), width = 1100, height = 750)
p5 <- ggplot(df_clean, aes(x = L, y = P, color = cl, shape = Year)) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_wrap(~Sector) +
  labs(title = "Кластеры по секторам: Леверидж vs ROS (2007–2008)",
       x = "Леверидж", y = "ROS",
       color = "Кластер", shape = "Год") +
  theme_bw(base_size = 11)
print(p5)
dev.off()

# ── Параллельные координаты: профили кластеров по всем шести признакам ────────
# Параллельные координаты — стандартный инструмент многомерной визуализации.
# Каждая линия — одна компания; цвет — кластер. Сходные паттерны
# (например, высокий Леверидж + низкий ROS) выявляют смысловой профиль кластера.
df_para      <- df_clean[, c(feat_cols, "cl", "Year")]
df_para$id   <- seq_len(nrow(df_para))
df_long      <- melt(df_para,
                     id.vars      = c("id", "cl", "Year"),
                     variable.name = "Feature",
                     value.name   = "Value")

# Стандартизованные значения уже в d_scaled; используем их для сравнимых осей
d_long_sc      <- as.data.frame(scale(df_clean[feat_cols]))
d_long_sc$id   <- seq_len(nrow(d_long_sc))
d_long_sc$cl   <- df_clean$cl
d_long_sc$Year <- df_clean$Year
df_long_sc <- melt(d_long_sc,
                   id.vars      = c("id", "cl", "Year"),
                   variable.name = "Feature",
                   value.name   = "Value")

png(file.path(out_dir, "06_parallel_coords.png"), width = 1100, height = 620)
p6 <- ggplot(df_long_sc, aes(x = Feature, y = Value, group = id, color = cl)) +
  geom_line(alpha = 0.15, linewidth = 0.4) +
  # Медиана по кластеру поверх индивидуальных линий — «профиль» кластера
  stat_summary(aes(group = cl), fun = median,
               geom = "line", linewidth = 1.6, alpha = 0.85) +
  facet_wrap(~Year, nrow = 1) +
  labs(title = "Параллельные координаты: профили кластеров (2007–2008)",
       subtitle = "Тонкие линии — отдельные компании; жирные — медианы кластеров",
       x = "Признак", y = "Стандартизованное значение", color = "Кластер") +
  theme_bw(base_size = 12)
print(p6)
dev.off()

# ── Матрица диаграмм рассеяния (pairs): все пары признаков ───────────────────
# Позволяет одновременно увидеть проекции кластеров на все попарные плоскости
# признаков — аналог многомерного «взгляда» на облако данных.
feat_labels <- c("Леверидж", "ROCE", "Гудвилл", "Фин.Активы", "Байбек", "ROS")
png(file.path(out_dir, "07_pairs_matrix.png"), width = 1050, height = 1050)
pairs(df_clean[feat_cols],
      col    = as.integer(df_clean$cl) + 1,
      pch    = 19,
      cex    = 0.35,
      labels = feat_labels,
      main   = paste0("Матрица проекций кластеров PAM  k = ", k_opt, "  (2007–2008)"),
      gap    = 0.3)
# Легенда поверх матрицы
legend("topright",
       legend = paste("Кластер", 1:k_opt),
       col    = 2:(k_opt + 1),
       pch    = 19,
       cex    = 0.9,
       bty    = "n")
dev.off()

# ── Сводные таблицы распределения кластеров по регионам и секторам ────────────
cat("Распределение кластеров по регионам (доли внутри региона):\n")
print(round(prop.table(with(df_clean, table(cl, Region)), 2), 3))
cat("\nРаспределение кластеров по секторам (доли внутри сектора):\n")
print(round(prop.table(with(df_clean, table(cl, Sector)), 2), 3))

# ── Медианные значения признаков по кластерам ─────────────────────────────────
# Интерпретация: что «типично» для каждого кластера по каждому показателю
cat("\nМедианные значения нестандартизованных признаков по кластерам:\n")
medians <- df_clean %>%
  group_by(cl) %>%
  summarise(across(all_of(feat_cols), median, .names = "{.col}"),
            n = n())
print(as.data.frame(medians))

cat("\nАнализ завершён. Графики сохранены в:", out_dir, "\n")
