# 0. Package Installation Check (Install only if missing)
required_packages <- c("openxlsx", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Path Setup
file_path <- "4_친환경에너지이용률_데이터.xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/4_친환경에너지이용률_데이터.xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14 groups order definition
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# Names mapping
geo_names <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미", "기타지역"),
  stringsAsFactors = FALSE
)

# Load Capacity Factor (CF_annual) worksheet
df_cf <- readWorkbook(file_path, sheet = "CF_annual", startRow = 2)
colnames(df_cf) <- trimws(colnames(df_cf))

# ==============================================================================
# [4-1] 2023년 전세계 국가별 태양광 평균이용률 수준 단계구분도 (5단계)
# ==============================================================================
cat("Generating 4-1) Solar CF Choropleth Map...\n")

df_cf$Solar_Pct <- df_cf$Solar * 100

# Load world map boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
world_energy <- merge(world, df_cf, by.x = "iso_a3", by.y = "code", all.x = TRUE)
world_energy <- world_energy[world_energy$iso_a3 != "ATA", ]

probs <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
breaks <- quantile(df_cf$Solar_Pct, probs = probs, na.rm = TRUE)
breaks <- unique(breaks)
if (length(breaks) < 6) {
  breaks <- seq(min(df_cf$Solar_Pct, na.rm=TRUE), max(df_cf$Solar_Pct, na.rm=TRUE), length.out=6)
}

world_energy$level <- cut(
  world_energy$Solar_Pct, 
  breaks = breaks,
  labels = c("5단계 (하위 10% 이하)", "4단계 (하위 10% ~ 30%)", "3단계 (중간 30% ~ 70%)", "2단계 (상위 10% ~ 30%)", "1단계 (상위 10% 이상)"),
  include.lowest = TRUE
)

solar_colors <- c(
  "5단계 (하위 10% 이하)" = "#FFF5EB",
  "4단계 (하위 10% ~ 30%)" = "#FDD0A2",
  "3단계 (중간 30% ~ 70%)" = "#FDAE6B",
  "2단계 (상위 10% ~ 30%)" = "#F16913",
  "1단계 (상위 10% 이상)" = "#A63603"
)

p_map <- ggplot(data = world_energy) +
  geom_sf(aes(fill = level), color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = solar_colors, na.value = "grey90", name = "태양광 이용률 수준 (2023년)") +
  theme_minimal() +
  labs(
    title = "2023년 전세계 국가별 태양광 평균이용률 수준 단계구분도 (5단계)",
    subtitle = "상위 10%는 1단계(적갈색), 하위 10%는 5단계(연황색)로 분류함",
    caption = "데이터 기준: input_DB_2025 수정본 (CF_annual 시트)"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save with the revised name as requested by the user
ggsave(file.path(fig_dir, "4-1)전세계 국가별 태양광 평균이용률 수준 단계구분도 (5단계).png"), plot = p_map, width = 11, height = 6.5, dpi = 300)


# ==============================================================================
# [4-2] 글로벌 권역별 신재생에너지원별 평균이용률 분할 막대그래프
# ==============================================================================
cat("Generating 4-2) Faceted CF Bar Chart...\n")

df_cf$Group <- toupper(trimws(df_cf[[colnames(df_cf)[grep("UNICON", colnames(df_cf), ignore.case = TRUE)[1]]]]))

solar_mean <- aggregate(df_cf$Solar, by = list(Group = df_cf$Group), FUN = mean, na.rm = TRUE)
windon_mean <- aggregate(df_cf$WindOn, by = list(Group = df_cf$Group), FUN = mean, na.rm = TRUE)
windoff_mean <- aggregate(df_cf$WindOff, by = list(Group = df_cf$Group), FUN = mean, na.rm = TRUE)

colnames(solar_mean)[2] <- "Solar"
colnames(windon_mean)[2] <- "WindOn"
colnames(windoff_mean)[2] <- "WindOff"

wide_cf <- merge(solar_mean, windon_mean, by = "Group", all = TRUE)
wide_cf <- merge(wide_cf, windoff_mean, by = "Group", all = TRUE)
wide_cf[is.na(wide_cf)] <- 0

cf_long <- data.frame(
  Group = rep(wide_cf$Group, 3),
  Name = rep(geo_names$Name[match(wide_cf$Group, geo_names$Group)], 3),
  tech = rep(c("Solar", "WindOn", "WindOff"), each = nrow(wide_cf)),
  CF_Pct = c(wide_cf$Solar, wide_cf$WindOn, wide_cf$WindOff) * 100,
  stringsAsFactors = FALSE
)

wide_cf$Mean_Sum <- (wide_cf$Solar + wide_cf$WindOn + wide_cf$WindOff) * 100
sorted_groups <- geo_names$Name[match(wide_cf$Group[order(wide_cf$Mean_Sum)], geo_names$Group)]
cf_long$Name <- factor(cf_long$Name, levels = sorted_groups)

cf_long$tech_ko <- factor(
  ifelse(cf_long$tech == "Solar", "태양광 (Solar)",
         ifelse(cf_long$tech == "WindOn", "육상풍력 (Onshore Wind)", "해상풍력 (Offshore Wind)")),
  levels = c("태양광 (Solar)", "육상풍력 (Onshore Wind)", "해상풍력 (Offshore Wind)")
)

cf_colors <- c(
  "태양광 (Solar)" = "#F0E442",
  "육상풍력 (Onshore Wind)" = "#009E73",
  "해상풍력 (Offshore Wind)" = "#2B5C43"
)

p_bar <- ggplot(cf_long, aes(x = CF_Pct, y = Name, fill = tech_ko)) +
  geom_col(color = "#FFFFFF", linewidth = 0.2, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", CF_Pct)), hjust = -0.15, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = cf_colors) +
  facet_wrap(~tech_ko, ncol = 3) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 글로벌 권역별 신재생에너지원별 평균이용률 비교 매트릭스",
    subtitle = "각 신재생에너지원별 이용률(%) 수준을 개별 가로 막대로 병렬 대조함",
    x = "평균이용률 (%)",
    y = "국가 (그룹)",
    caption = "데이터 기준: 4_친환경에너지이용률_데이터.xlsx (CF_annual 시트)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "4-2) 글로벌 권역별 신재생에너지원별 평균이용률 분할 막대그래프.png"), plot = p_bar, width = 11, height = 7, dpi = 300)

cat("SUCCESS: All 2 Capacity Factor visualizations generated successfully.\n")
