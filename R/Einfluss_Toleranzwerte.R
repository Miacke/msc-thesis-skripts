# ---------------------------
# Projekt:    Master-Thesis
# Autor:      Mirco Ackermann
# Datum:      19.05.2026
# Zweck:      Skript berechnet den effektive prozentuale Abweichung der Punktdichte bei 
#             den gewählten Toleranzen für Flughöhe und -geschwindigkeit
# ---------------------------

library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)

# --- Parameter ---

delta_v <- 5
mean_v <- seq(40,90, by = 5)
delta_h <- 290
mean_h <- seq(500,2000, by = 100)
PR <- 100000
SR <- 200
scan_a <- 30

# --- Funktionen ---

calc_P <- function(h, v, PR, SR, scan_a) {
  # Berechnet effektive Punktdiche
  FOV      <- tan(scan_a * pi / 180) * h * 2
  d_across <- FOV / (PR / SR)
  d_along  <- v / SR
  1 / (d_across * d_along)
}

calc_P_range <- function(h, v, delta_h, delta_v, PR, SR, scan_a) {
  # Berechnet PUnktdichte und jeweiliges Wors-Case-Szenario gemäss Toleranzrahmen
  P_ref <- calc_P(h, v, PR, SR, scan_a)
  P_max <- calc_P(h - delta_h, v - delta_v, PR, SR, scan_a)
  P_min <- calc_P(h + delta_h, v + delta_v, PR, SR, scan_a)
  
  list(P_ref = P_ref, P_max = P_max, P_min = P_min)
}

calc_P_change <- function(h, v, delta_h, delta_v, PR, SR, scan_a) {
  # Berechnet PUnktdichte und jeweiliges Wors-Case-Szenario gemäss Toleranzrahmen
  P_max <- calc_P(h - delta_h, v - delta_v, PR, SR, scan_a)
  P_min <- calc_P(h + delta_h, v + delta_v, PR, SR, scan_a)
  
  paste0(round(P_min,2), " - ", round(P_max,2))
}

# --- Matrix erstellen --- 
matrix_change <- outer(mean_h, mean_v, 
                       Vectorize(function(h, v) calc_P_change(h, v, delta_h, delta_v, PR, SR, scan_a)))

rownames(matrix_change) <- mean_h
colnames(matrix_change) <- mean_v
write.csv2(matrix_change, "Einfluss_h_v.csv")

# --- Tabelle  für plot erstellen ---
# Alle Kombinationen

df <- expand.grid(h = mean_h[seq(1, length(mean_h), by = 5)], v = mean_v)

# Für jede Zeile P_ref, P_max, P_min berechnen
df$P_ref <- mapply(calc_P, df$h, df$v, MoreArgs = list(PR=PR, SR=SR, scan_a=scan_a))
df$P_max <- mapply(calc_P, df$h - delta_h, df$v - delta_v, MoreArgs = list(PR=PR, SR=SR, scan_a=scan_a))
df$P_min <- mapply(calc_P, df$h + delta_h, df$v + delta_v, MoreArgs = list(PR=PR, SR=SR, scan_a=scan_a))

# --- Visualisierung ---

ggplot(df, aes(x = v, color = factor(h), fill = factor(h))) +
  geom_ribbon(aes(ymin = P_min, ymax = P_max), alpha = 0.15, color = NA) +
  geom_line(aes(y = P_ref), size = 0.8) +
  labs(
    title = "Punktdichte mit Toleranzbereich",
    subtitle = paste0("Δh = ±", delta_h, " m, Δv = ±", delta_v, " m/s"),
    x = "Fluggeschwindigkeit (m/s)",
    y = "Punktdichte (Punkte/m²)",
    color = "Flughöhe (m)",
    fill = "Flughöhe (m)"
  ) +
  theme_minimal()
