# 0. 패키지 설치 확인 (누락된 패키지만 설치)
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

# 1. 경로 설정
file_path <- "input_DB_2025_v.13_2(송부용).xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/input_DB_2025_v.13_2(송부용).xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14개 권역(그룹) 정렬 정의
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# 권역 영문명-한글명 매핑 데이터
geo_names <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미", "기타지역"),
  stringsAsFactors = FALSE
)

# 친환경 이용률(CF_annual) 시트 로드
df_cf <- readWorkbook(file_path, sheet = "CF_annual", startRow = 2)
colnames(df_cf) <- trimws(colnames(df_cf))
df_cf$code <- toupper(trimws(df_cf[[colnames(df_cf)[grep("code|X1", colnames(df_cf), ignore.case = TRUE)[1]]]]))

# --- [DEFENSIVE CODE] Check capacity factor data integrity ---
required_cf_cols <- c("Solar", "WindOn", "WindOff", "code")
missing_cf_cols <- setdiff(required_cf_cols, colnames(df_cf))
if (length(missing_cf_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in CF_annual sheet:", paste(missing_cf_cols, collapse = ", ")))
}
for (col in c("Solar", "WindOn", "WindOff")) {
  if (!is.numeric(df_cf[[col]])) {
    warning(paste("DEFENSIVE WARNING: Column", col, "in CF_annual sheet is not numeric. Converting..."))
    df_cf[[col]] <- as.numeric(df_cf[[col]])
  }
}

# ==============================================================================
# [4-1] 전세계 국가별 신재생에너지원별 평균이용률 수준 단계구분도 (5단계)
# --- [4-1. 평균이용률] 핵심 분석 및 연산 로직: 태양광, 육상풍력, 해상풍력별로 연간 이용률(%) 데이터를 분위수 기반 5단계로 분류 분석 ---
# ==============================================================================
solar_colors <- c(
  "5단계 (하위 10% 이하)" = "#FFF5EB",
  "4단계 (하위 10% ~ 30%)" = "#FDD0A2",
  "3단계 (중간 30% ~ 70%)" = "#FDAE6B",
  "2단계 (상위 10% ~ 30%)" = "#F16913",
  "1단계 (상위 10% 이상)" = "#A63603"
)

windon_colors <- c(
  "5단계 (하위 10% 이하)" = "#F7FCF5",
  "4단계 (하위 10% ~ 30%)" = "#E5F5E0",
  "3단계 (중간 30% ~ 70%)" = "#A1D99B",
  "2단계 (상위 10% ~ 30%)" = "#41AB5D",
  "1단계 (상위 10% 이상)" = "#006D2C"
)

windoff_colors <- c(
  "5단계 (하위 10% 이하)" = "#F7FCF0",
  "4단계 (하위 10% ~ 30%)" = "#E0F3DB",
  "3단계 (중간 30% ~ 70%)" = "#A8DDB5",
  "2단계 (상위 10% ~ 30%)" = "#4EB3D3",
  "1단계 (상위 10% 이상)" = "#08589E"
)

generate_cf_map <- function(tech_col, title_prefix, colors_map, legend_name, file_prefix) {
  cat(sprintf("Generating 4-1) %s CF Choropleth Map...\n", title_prefix))
  
  df_cf[[paste0(tech_col, "_Pct")]] <- df_cf[[tech_col]] * 100
  
  # 세계 지형 경계 데이터 로드
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_energy <- merge(world, df_cf, by.x = "iso_a3", by.y = "code", all.x = TRUE)
  world_energy <- world_energy[world_energy$iso_a3 != "ATA", ]
  
  val_pct <- df_cf[[paste0(tech_col, "_Pct")]]
  valid_vals <- val_pct[!is.na(val_pct)]
  
  # 분위구간 경계값 계산
  probs <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
  breaks <- quantile(valid_vals, probs = probs, na.rm = TRUE)
  breaks <- unique(breaks)
  if (length(breaks) < 6) {
    breaks <- seq(min(valid_vals, na.rm=TRUE), max(valid_vals, na.rm=TRUE), length.out=6)
  }
  
  # 경계값 절삭 방지를 위한 보정
  breaks[1] <- breaks[1] - 0.0001
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.0001
  
  cf_labels <- c(
    sprintf("5단계 (%.1f%% 이하)", breaks[2]),
    sprintf("4단계 (%.1f%% ~ %.1f%%)", breaks[2], breaks[3]),
    sprintf("3단계 (%.1f%% ~ %.1f%%)", breaks[3], breaks[4]),
    sprintf("2단계 (%.1f%% ~ %.1f%%)", breaks[4], breaks[5]),
    sprintf("1단계 (%.1f%% 이상)", breaks[5])
  )
  
  world_energy$level <- cut(
    world_energy[[paste0(tech_col, "_Pct")]], 
    breaks = breaks,
    labels = cf_labels,
    include.lowest = TRUE
  )
  
  map_colors <- colors_map
  names(map_colors) <- cf_labels
  
  p_map <- ggplot(data = world_energy) +
    geom_sf(aes(fill = level), color = "#FFFFFF", linewidth = 0.1) +
    scale_fill_manual(values = map_colors, na.value = "grey90", name = legend_name) +
    theme_minimal() +
    labs(
      title = sprintf("전세계 국가별 %s 평균이용률 수준 단계구분도 (5단계)", title_prefix),
      subtitle = sprintf("상위 10%%는 1단계(가장 짙은색), 하위 10%%는 5단계(가장 옅은색)로 분류함"),
      caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx (CF_annual 시트)"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  file_name <- sprintf("4-1)전세계 국가별 %s 평균이용률 수준 단계구분도 (5단계).png", file_prefix)
  ggsave(file.path(fig_dir, file_name), plot = p_map, width = 11, height = 6.5, dpi = 300)
}

# 3대 전원별 단계구분도 생성
generate_cf_map("Solar", "태양광", solar_colors, "태양광 이용률 수준 (2023년)", "태양광")
generate_cf_map("WindOn", "육상풍력", windon_colors, "육상풍력 이용률 수준 (2023년)", "육상풍력")
generate_cf_map("WindOff", "해상풍력", windoff_colors, "해상풍력 이용률 수준 (2023년)", "해상풍력")


# ==============================================================================
# [4-2] 글로벌 권역별 신재생에너지원별 평균이용률 막대그래프 (개별 분할)
# --- [4-2. 평균이용률 GIS] 핵심 분석 및 연산 로직: 권역별 산술 평균 이용률을 구하되, 결측(NA) 권역을 사하라이남 권역 하단으로 배치하고 텍스트 라벨을 미출력하여 시각적 혼선 예방 분석 ---
# ==============================================================================
df_cf$Group <- toupper(trimws(df_cf[[colnames(df_cf)[grep("UNICON", colnames(df_cf), ignore.case = TRUE)[1]]]]))

generate_cf_bar_chart <- function(tech_col, title_prefix, color_val, file_prefix) {
  cat(sprintf("Generating 4-2) %s CF Bar Chart...\n", title_prefix))
  
  # UNICON 그룹별 평균 이용률 계산
  df_mean <- aggregate(df_cf[[tech_col]], by = list(Group = df_cf$Group), FUN = mean, na.rm = TRUE)
  colnames(df_mean)[2] <- "CF_Val"
  df_mean$CF_Pct <- df_mean$CF_Val * 100
  
  # 한글 그룹명 병합
  df_mean <- merge(df_mean, geo_names, by = "Group")
  
  # 결측치(NA)가 차트 최하단에 위치하도록 오름차순 정렬
  sorted_groups <- df_mean$Name[order(df_mean$CF_Pct, na.last = FALSE)]
  df_mean$Name <- factor(df_mean$Name, levels = sorted_groups)
  
  p_bar <- ggplot(df_mean, aes(x = CF_Pct, y = Name)) +
    geom_col(fill = color_val, color = "#FFFFFF", linewidth = 0.2, width = 0.6) +
    geom_text(aes(label = ifelse(is.na(CF_Pct), "", sprintf("%.1f%%", CF_Pct))), hjust = -0.15, size = 3.2, fontface = "bold") +
    theme_minimal() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = sprintf("글로벌 권역별 %s 평균이용률 막대그래프", title_prefix),
      subtitle = sprintf("각 권역별 %s의 연간 평균 이용률(%%) 대조", title_prefix),
      x = "평균이용률 (%)",
      y = "국가 (그룹)",
      caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx (CF_annual 시트)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
      axis.text.y = element_text(face = "bold", size = 10, color = "#333333"),
      axis.text.x = element_text(size = 9, color = "#333333"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  file_name <- sprintf("4-2) 글로벌 권역별 %s 평균이용률 막대그래프.png", file_prefix)
  ggsave(file.path(fig_dir, file_name), plot = p_bar, width = 10, height = 6.5, dpi = 300)
}

generate_cf_bar_chart("Solar", "태양광", "#F0E442", "태양광")
generate_cf_bar_chart("WindOn", "육상풍력", "#009E73", "육상풍력")
generate_cf_bar_chart("WindOff", "해상풍력", "#2B5C43", "해상풍력")

cat("SUCCESS: All 6 Capacity Factor visualizations generated successfully.\n")
