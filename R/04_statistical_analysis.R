# ============================================================
# 04_statistical_analysis.R
# ============================================================
# Beschreibung:
#   - Statistische Auswertungen
#
# Input:
#   - Untersuchungsgebiete
#   - ordner mit laz-files
#
# Output:
#   - auf Untersuchungsgebiete zugeschnittene und normalisierte laz-files
#
# Autor:       Mirco Ackermann
# Datum:       19.08.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

library(dplyr)

# Berechnung des t-wertes (Beispieldaten)

vec_1 <- runif(n=12, min = 5000, max = 50000)
vec_2 <- runif(n=12, min = 10000, max = 80000)


vec_diff <- vec_2 - vec_1
shapiro.test(vec_diff)

t.test(vec_1, vec_2, paired = TRUE, conf.level = 0.99)
