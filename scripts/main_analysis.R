# 1. Підключення необхідних бібліотек
library(quantmod)
library(PerformanceAnalytics)

# 2. Список активів для аналізу
tickers <- c("GOOG", "NVDA")

# Завантаження даних
getSymbols(tickers, src = "yahoo", from = "2023-01-01", to = Sys.Date())

# Цикл, який зробить аналіз і малюнки для кожного активу автоматично
for (ticker in tickers) {
  
  # Витягуємо ціни та рахуємо дохідності для конкретного активу
  prices <- Ad(get(ticker))
  returns <- na.omit(CalculateReturns(prices, method = "log"))
  
  # Розрахунок метрик VaR та ES
  var_hist_95 <- VaR(returns, p = 0.95, method = "historical")
  var_hist_99 <- VaR(returns, p = 0.99, method = "historical")
  var_norm_95 <- VaR(returns, p = 0.95, method = "gaussian")
  es_hist_95  <- ES(returns, p = 0.95, method = "historical")
  
  # Вивід результатів у консоль
  cat("--- РЕЗУЛЬТАТИ АНАЛІЗУ РИЗИКУ ДЛЯ", ticker, "---\n")
  cat("Historical VaR (95%):", round(as.numeric(var_hist_95) * 100, 2), "%\n")
  cat("Historical VaR (99%):", round(as.numeric(var_hist_99) * 100, 2), "%\n")
  cat("Parametric VaR (95%):", round(as.numeric(var_norm_95) * 100, 2), "%\n")
  cat("Expected Shortfall (95%):", round(as.numeric(es_hist_95) * 100, 2), "%\n\n")
  
  # Збереження графіку (назви файлів будуть var_chart_goog.png та var_chart_nvda.png)
  file_name <- paste0("data/var_chart_", tolower(ticker), ".png")
  png(file_name, width = 800, height = 600)
  
  hist(returns, breaks = 50, col = "lightblue", 
       main = paste("Розподіл дохідностей та VaR (", ticker, ")", sep=""), 
       xlab = "Log Returns")
  abline(v = as.numeric(var_hist_95), col = "orange", lwd = 2, lty = 2)
  abline(v = as.numeric(var_hist_99), col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c("VaR 95%", "VaR 99%"), col = c("orange", "red"), lty = 2, lwd = 2)
  
  dev.off()
}