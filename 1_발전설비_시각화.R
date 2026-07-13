# 0. 패키지 설치 확인 (누락된 패키지만 설치)
required_packages <- c("openxlsx", "ggplot2", "treemapify")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(treemapify)

# 1. 경로 설정
file_path <- "input_DB_2025_v.13_2(송부용).xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/input_DB_2025_v.13_2(송부용).xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14개 권역(그룹) 정렬 정의
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# 발전기술원별 색상 매핑 정의
tech_colors <- c(
  "Coal" = "#4A4A4A",        # Charcoal
  "LNG" = "#E69F00",         # Orange
  "Nuclear" = "#56B4E9",     # Sky Blue
  "Hydro" = "#0072B2",       # Ocean Blue
  "Solar" = "#F0E442",       # Sunny Yellow
  "WindOn" = "#009E73",      # Jade Green
  "WindOff" = "#2B5C43",     # Dark Green
  "Biomass" = "#CC79A7",     # Rose Pink
  "Geothermal" = "#D55E00",  # Red-Orange
  "Oil" = "#8B5A2B",         # Brown
  "PSH" = "#999999",         # Medium Grey
  "Waste" = "#6E8B3D"        # Olive Green
)

# LOCF(직전 유효값 대체) 보정 함수
apply_locf <- function(df, years) {
  for (i in 1:nrow(df)) {
    for (y_idx in 2:length(years)) {
      current_yr <- years[y_idx]
      if (is.na(df[i, current_yr])) {
        prev_years <- years[1:(y_idx - 1)]
        prev_vals <- sapply(prev_years, function(py) df[i, py])
        valid_prev_idx <- which(!is.na(prev_vals))
        if (length(valid_prev_idx) > 0) {
          closest_idx <- max(valid_prev_idx)
          df[i, current_yr] <- df[i, years[closest_idx]]
        }
      }
    }
  }
  return(df)
}

# 발전설비(capacity) 시트 로드
df_cap <- readWorkbook(file_path, sheet = "capacity", startRow = 2)
colnames(df_cap) <- trimws(colnames(df_cap))
colnames(df_cap)[colnames(df_cap) == "Technology"] <- "tech"
colnames(df_cap)[colnames(df_cap) == "X2"] <- "tech"
df_cap$Group <- toupper(trimws(df_cap[[colnames(df_cap)[grep("UNICON", colnames(df_cap), ignore.case = TRUE)[1]]]]))
df_cap <- apply_locf(df_cap, as.character(2017:2023))

# --- [DEFENSIVE CODE] Check capacity data integrity ---
required_cap_cols <- c("tech", "Group", as.character(2017:2023))
missing_cap_cols <- setdiff(required_cap_cols, colnames(df_cap))
if (length(missing_cap_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in capacity sheet:", paste(missing_cap_cols, collapse = ", ")))
}
for (yr in as.character(2017:2023)) {
  if (!is.numeric(df_cap[[yr]])) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "in capacity sheet is not numeric. Converting..."))
    df_cap[[yr]] <- as.numeric(df_cap[[yr]])
  }
  if (any(is.na(df_cap[[yr]]))) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "contains NA/NaN values in capacity sheet."))
  }
}


# ==============================================================================
# [1-1] 2023년 국가(그룹)별 Coal 설비용량 (GW)
# --- [1. 발전설비 - 석탄 설비용량] 핵심 분석 및 연산 로직: MW 단위인 석탄 설비용량을 1,000으로 나누어 GW 단위로 환산 및 합산 분석 ---
# ==============================================================================
cat("Generating 1-1) Coal Capacity Map...\n")
df_coal <- df_cap[tolower(trimws(df_cap$tech)) == "coal", ]
group_coal <- aggregate(df_coal[["2023"]], by = list(Group = df_coal$Group), FUN = sum, na.rm = TRUE)
colnames(group_coal)[2] <- "Value_GW"
# Y축 정렬 시 대용량이 상단에 오도록 설비용량 기준 오름차순 정렬
sorted_groups_coal <- group_coal$Group[order(group_coal$Value_GW)]
group_coal$Group <- factor(group_coal$Group, levels = sorted_groups_coal)

p_coal <- ggplot(group_coal, aes(x = Value_GW, y = Group)) +
  geom_bar(stat = "identity", fill = "#222222", width = 0.6) +
  geom_text(aes(label = ifelse(Value_GW > 0, paste0(round(Value_GW, 1), " GW"), "")), 
            hjust = -0.1, size = 3.2, fontface = "bold") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 국가(그룹)별 Coal 발전설비용량 (GW)",
    subtitle = "전체 발전설비 중 석탄(Coal)에 해당하는 설비용량만 필터링한 결과",
    x = "설비용량 (GW)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
  ) +
  theme(
    axis.text.y = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#333333"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "1-1)2023년 국가(그룹)별 Coal 설비용량 (GW).png"), plot = p_coal, width = 10, height = 6.5, dpi = 300)


# ==============================================================================
# [1-2] 2023년 국가(그룹)별 발전설비 구성비 및 총 설비용량 (GW)
# --- [1. 발전설비] 핵심 분석 및 연산 로직: 연도별 전원별 설비용량을 GW 단위로 합산하여 누적 막대그래프를 생성하고 총량 기준 정렬하여 전원별 구성비 분석 ---
# ==============================================================================
cat("Generating 1-1) Clustered Capacity Mix...\n")
cap_agg <- aggregate(df_cap[["2023"]], by = list(Group = df_cap$Group, tech = df_cap$tech), FUN = sum, na.rm = TRUE)
colnames(cap_agg)[3] <- "Capacity_GW"
cap_agg <- cap_agg[cap_agg$Capacity_GW > 0, ]

cap_totals <- aggregate(Capacity_GW ~ Group, data = cap_agg, FUN = sum)
# Y축 정렬 시 대용량이 상단에 오도록 총 설비용량 기준 오름차순 정렬
sorted_groups_mix <- cap_totals$Group[order(cap_totals$Capacity_GW)]
cap_agg$Group <- factor(cap_agg$Group, levels = sorted_groups_mix)
cap_agg$tech <- factor(cap_agg$tech, levels = names(tech_colors))
cap_totals$Group <- factor(cap_totals$Group, levels = sorted_groups_mix)

p_mix <- ggplot(cap_agg, aes(x = Capacity_GW, y = Group, fill = tech)) +
  geom_col(color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = tech_colors, name = "발전설비원") +
  geom_text(data = cap_totals, aes(x = Capacity_GW, y = Group, label = sprintf("%.1f GW", Capacity_GW)),
            hjust = -0.15, size = 3, fontface = "bold", inherit.aes = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "2023년 국가(그룹)별 발전설비 구성비 및 총 설비용량 (GW)",
    x = "설비용량 (GW)",
    y = "국가 (그룹)"
  ) +
  xlim(0, max(cap_totals$Capacity_GW) * 1.15)

ggsave(file.path(fig_dir, "1-1)2023년 국가(그룹)별 발전설비 구성비 및 총 설비용량 (GW).png"), plot = p_mix, width = 10, height = 6.5, dpi = 300)


cat("SUCCESS: All 2 Power Capacity visualizations generated successfully.\n")
