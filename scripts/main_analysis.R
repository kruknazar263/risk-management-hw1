# 1. Підключення необхідних бібліотек
library(quantmod)
library(PerformanceAnalytics)

# 2. Завантаження даних 
getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = Sys.Date())
prices <- Ad(AAPL)

# 3. Розрахунок логарифмічних дохідностей без пропусків
returns <- na.omit(CalculateReturns(prices, method = "log"))

# 4. Розрахунок метрик VaR та ES
var_hist_95 <- VaR(returns, p = 0.95, method = "historical")
var_hist_99 <- VaR(returns, p = 0.99, method = "historical")
var_norm_95 <- VaR(returns, p = 0.95, method = "gaussian")
es_hist_95  <- ES(returns, p = 0.95, method = "historical")

# Вивід результатів у консоль
cat("--- РЕЗУЛЬТАТИ АНАЛІЗУ РИЗИКУ ДЛЯ AAPL ---\n")
cat("Historical VaR (95%):", round(as.numeric(var_hist_95) * 100, 2), "%\n")
cat("Historical VaR (99%):", round(as.numeric(var_hist_99) * 100, 2), "%\n")
cat("Parametric VaR (95%):", round(as.numeric(var_norm_95) * 100, 2), "%\n")
cat("Expected Shortfall (95%):", round(as.numeric(es_hist_95) * 100, 2), "%\n")

# 5. Збереження графіку безпосередньо у файл
png("data/var_chart.png", width = 800, height = 600)

hist(returns, breaks = 50, col = "lightblue", main = "Розподіл дохідностей та VaR (AAPL)", xlab = "Log Returns")
abline(v = as.numeric(var_hist_95), col = "orange", lwd = 2, lty = 2)
abline(v = as.numeric(var_hist_99), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("VaR 95%", "VaR 99%"), col = c("orange", "red"), lty = 2, lwd = 2)

dev.off()