# 0. 패키지 설치 확인 (누락된 패키지만 설치)
required_packages <- c("openxlsx", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)

# 1. 경로 설정
file_path <- "input_DB_2025_v.13_2(송부용).xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/input_DB_2025_v.13_2(송부용).xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14개 권역(그룹) 정렬 정의
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# 권역(그룹)별 색상 매핑 정의
group_colors <- c(
  "KOR" = "#E41A1C",
  "CHN" = "#FF7F00",
  "JPN" = "#984EA3",
  "IND" = "#377EB8",
  "ASEAN" = "#4DAF4A",
  "OCE" = "#A65628",
  "USA" = "#F781BF",
  "CAN" = "#FFD700",
  "EUR" = "#1B9E77",
  "FSU" = "#66C2A5",
  "SSA" = "#8DA0CB",
  "MENA" = "#E6AB02",
  "LATAM" = "#66A61E",
  "ROW" = "#999999"
)

# 발전기술원별 색상 매핑 정의 (개별 국가 단위 설비도입연도 차트용)
tech_colors <- c(
  "Coal" = "#4A4A4A",        # Charcoal
  "LNG" = "#E69F00",         # Orange
  "Nuclear" = "#56B4E9",     # Sky Blue
  "Solar" = "#F0E442",       # Sunny Yellow
  "WindOn" = "#009E73",      # Jade Green
  "WindOff" = "#2B5C43",     # Dark Green
  "Oil" = "#8B5A2B"          # Brown
)

# 발전설비 도입연도(capacity_vintage) 시트 로드
df_vin <- readWorkbook(file_path, sheet = "capacity_vintage", startRow = 2)
colnames(df_vin) <- trimws(colnames(df_vin))
colnames(df_vin)[colnames(df_vin) == "Technology"] <- "tech"
colnames(df_vin)[colnames(df_vin) == "X2"] <- "tech"
df_vin$Group <- toupper(trimws(df_vin[[colnames(df_vin)[grep("UNICON", colnames(df_vin), ignore.case = TRUE)[1]]]]))

year_cols <- colnames(df_vin)[grepl("^[0-9]{4}$", colnames(df_vin))]

# --- [DEFENSIVE CODE] Check capacity_vintage data integrity ---
required_vin_cols <- c("tech", "Group", year_cols)
missing_vin_cols <- setdiff(required_vin_cols, colnames(df_vin))
if (length(missing_vin_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in capacity_vintage sheet:", paste(missing_vin_cols, collapse = ", ")))
}
for (yc in year_cols) {
  if (!is.numeric(df_vin[[yc]])) {
    warning(paste("DEFENSIVE WARNING: Column", yc, "in capacity_vintage sheet is not numeric. Converting..."))
    df_vin[[yc]] <- as.numeric(df_vin[[yc]])
  }
}
# 주의: 설비도입연도별 데이터는 연도마다 독립적인 신규 설치량이므로,
# 다른 시각화 스크립트에서 사용하는 LOCF(직전값 대체) 보정을 적용하지 않음

# 설비도입연도 x 그룹 롱포맷 변환 함수 (연도별로 그룹 단위 합산)
build_vintage_long <- function(df_tech, year_cols) {
  long_list <- lapply(year_cols, function(yc) {
    agg <- aggregate(df_tech[[yc]], by = list(Group = df_tech$Group), FUN = sum, na.rm = TRUE)
    colnames(agg)[2] <- "Capacity_GW"
    agg$Year <- as.integer(yc)
    agg
  })
  do.call(rbind, long_list)
}

# 설비도입연도 누적 막대그래프 생성 함수 (국가/그룹별 색상)
generate_vintage_chart <- function(long_df, title_txt, subtitle_txt, file_name) {
  long_df <- long_df[long_df$Capacity_GW > 0, ]
  long_df$Group <- factor(long_df$Group, levels = groups_order)

  p <- ggplot(long_df, aes(x = factor(Year), y = Capacity_GW, fill = Group)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = group_colors, name = "국가(그룹)") +
    scale_x_discrete(breaks = as.character(seq(1940, 2024, by = 2))) +
    theme_minimal() +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "설비도입연도",
      y = "설비용량 (GW)",
      caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx (capacity_vintage 시트)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(file.path(fig_dir, file_name), plot = p, width = 12, height = 6.5, dpi = 300)
}

# 설비도입연도 x 발전기술원 롱포맷 변환 함수 (특정 국가 필터 후 연도별 기술 합산)
build_vintage_long_by_tech <- function(df_country, year_cols) {
  long_list <- lapply(year_cols, function(yc) {
    agg <- aggregate(df_country[[yc]], by = list(tech = df_country$tech), FUN = sum, na.rm = TRUE)
    colnames(agg)[2] <- "Capacity_GW"
    agg$Year <- as.integer(yc)
    agg
  })
  do.call(rbind, long_list)
}

# 개별 국가용 설비도입연도 누적 막대그래프 생성 함수 (발전기술원별 색상)
generate_vintage_chart_by_tech <- function(long_df, title_txt, subtitle_txt, file_name) {
  long_df <- long_df[long_df$Capacity_GW > 0, ]
  long_df$tech <- factor(long_df$tech, levels = names(tech_colors))

  p <- ggplot(long_df, aes(x = factor(Year), y = Capacity_GW, fill = tech)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = tech_colors, name = "발전기술원") +
    scale_x_discrete(breaks = as.character(seq(1940, 2024, by = 2))) +
    theme_minimal() +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "설비도입연도",
      y = "설비용량 (GW)",
      caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx (capacity_vintage 시트)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(file.path(fig_dir, file_name), plot = p, width = 12, height = 6.5, dpi = 300)
}

# ==============================================================================
# [6-1] 국가(그룹)별 석탄 발전설비 설비도입연도 (GW)
# --- [6-1. 설비도입연도 - 석탄] 핵심 분석 및 연산 로직: 석탄 발전설비의 설비도입연도별 설비용량(GW)을 국가(그룹) 단위로 합산하여 분석 ---
# ==============================================================================
cat("Generating 6-1) Coal Vintage-Year Chart...\n")
df_coal <- df_vin[tolower(trimws(df_vin$tech)) == "coal", ]
long_coal <- build_vintage_long(df_coal, year_cols)
generate_vintage_chart(
  long_coal,
  "석탄발전 설비도입연도 (GW)",
  "설비도입연도별 석탄 설비용량을 국가(그룹) 단위로 누적 표시",
  "6-1)석탄발전 설비도입연도 (GW).png"
)

# ==============================================================================
# [6-2] 국가(그룹)별 LNG 발전설비 설비도입연도 (GW)
# --- [6-2. 설비도입연도 - LNG] 핵심 분석 및 연산 로직: LNG 발전설비의 설비도입연도별 설비용량(GW)을 국가(그룹) 단위로 합산하여 분석 ---
# ==============================================================================
cat("Generating 6-2) LNG Vintage-Year Chart...\n")
df_lng <- df_vin[tolower(trimws(df_vin$tech)) == "lng", ]
long_lng <- build_vintage_long(df_lng, year_cols)
generate_vintage_chart(
  long_lng,
  "LNG발전 설비도입연도 (GW)",
  "설비도입연도별 LNG 설비용량을 국가(그룹) 단위로 누적 표시",
  "6-2)LNG발전 설비도입연도 (GW).png"
)

# ==============================================================================
# [6-3] 국가(그룹)별 원자력 발전설비 설비도입연도 (GW)
# --- [6-3. 설비도입연도 - 원자력] 핵심 분석 및 연산 로직: 원자력 발전설비의 설비도입연도별 설비용량(GW)을 국가(그룹) 단위로 합산하여 분석 ---
# ==============================================================================
cat("Generating 6-3) Nuclear Vintage-Year Chart...\n")
df_nuclear <- df_vin[tolower(trimws(df_vin$tech)) == "nuclear", ]
long_nuclear <- build_vintage_long(df_nuclear, year_cols)
generate_vintage_chart(
  long_nuclear,
  "원자력발전 설비도입연도 (GW)",
  "설비도입연도별 원자력 설비용량을 국가(그룹) 단위로 누적 표시",
  "6-3)원자력발전 설비도입연도 (GW).png"
)

# ==============================================================================
# [6-4] 한국의 기술별 설비도입연도 (GW) - 특정 국가 샘플
# --- [6-4. 설비도입연도 - 국가별 샘플] 핵심 분석 및 연산 로직: 특정 국가(KOR)의 설비도입연도별 설비용량(GW)을 발전기술원 단위로 합산하여 분석 ---
# ==============================================================================
cat("Generating 6-4) Korea Vintage-Year Chart (by tech)...\n")
df_kor <- df_vin[df_vin$Group == "KOR", ]
long_kor <- build_vintage_long_by_tech(df_kor, year_cols)
generate_vintage_chart_by_tech(
  long_kor,
  "한국의 기술별 설비도입연도 (GW)",
  "설비도입연도별 한국 발전설비 용량을 발전기술원 단위로 누적 표시",
  "6-4)한국의 기술별 설비도입연도 (GW).png"
)

cat("SUCCESS: All 4 Capacity Vintage-Year visualizations generated successfully.\n")
