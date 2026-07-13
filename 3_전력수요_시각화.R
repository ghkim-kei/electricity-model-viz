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

# 연간 수요(demand_annual) 시트 로드
df_dem <- readWorkbook(file_path, sheet = "demand_annual", startRow = 2)
colnames(df_dem) <- trimws(colnames(df_dem))
df_dem$code <- toupper(trimws(df_dem[[colnames(df_dem)[grep("code|X1", colnames(df_dem), ignore.case = TRUE)[1]]]]))
df_dem$Group <- toupper(trimws(df_dem[[colnames(df_dem)[grep("UNICON", colnames(df_dem), ignore.case = TRUE)[1]]]]))
df_dem <- apply_locf(df_dem, as.character(2017:2023))

# --- [DEFENSIVE CODE] Check demand data integrity ---
required_dem_cols <- c("Group", "code", as.character(2017:2023))
missing_dem_cols <- setdiff(required_dem_cols, colnames(df_dem))
if (length(missing_dem_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in demand sheet:", paste(missing_dem_cols, collapse = ", ")))
}
for (yr in as.character(2017:2023)) {
  if (!is.numeric(df_dem[[yr]])) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "in demand sheet is not numeric. Converting..."))
    df_dem[[yr]] <- as.numeric(df_dem[[yr]])
  }
  if (any(is.na(df_dem[[yr]]))) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "contains NA/NaN values in demand sheet."))
  }
}

# ==============================================================================
# [3-1] 2023년 국가(그룹)별 연간 전력수요량 (TWh) - 바차트 정렬
# --- [3-1. 총전력수요] 핵심 분석 및 연산 로직: GWh 단위인 연간 전력수요량을 1,000으로 나누어 TWh 단위로 환산 및 합산 분석 ---
# ==============================================================================
cat("Generating 3-1) Demand Group Bar Chart...\n")
dem_2023_g <- aggregate(df_dem[["2023"]], by = list(Group = df_dem$Group), FUN = sum, na.rm = TRUE)
colnames(dem_2023_g)[2] <- "Demand_GWh"
dem_2023_g$Demand_TWh <- dem_2023_g$Demand_GWh / 1000

# Merge Korean Names
dem_2023_g <- merge(dem_2023_g, geo_names, by = "Group")

# Sort ascending of Demand_TWh to put the largest (CHN) on top of Y-axis
sorted_groups_dem <- dem_2023_g$Name[order(dem_2023_g$Demand_TWh)]
dem_2023_g$Name <- factor(dem_2023_g$Name, levels = sorted_groups_dem)

p_bar <- ggplot(dem_2023_g, aes(x = Demand_TWh, y = Name)) +
  geom_bar(stat = "identity", fill = "#0072B2", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f TWh", Demand_TWh)), 
            hjust = -0.1, size = 3.2, fontface = "bold") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 국가(그룹)별 연간 전력수요량 (TWh)",
    subtitle = "각 국가(그룹)별 2023년 연간 총 전력소비량 대조 (GWh -> TWh 환산)",
    x = "전력수요량 (TWh)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
  ) +
  theme(
    axis.text.y = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#333333"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "3-1)2023년 국가(그룹)별 연간 전력수요량 (TWh).png"), plot = p_bar, width = 10, height = 6.5, dpi = 300)


# ==============================================================================
# [3-2. 총전력수요 GIS] 전세계 국가별 총 전력수요 수준 단계구분도 (5단계)
# --- [3-2. 총전력수요 GIS] 핵심 분석 및 연산 로직: 국가별 전력수요량(TWh)을 분위수 기준으로 나누어 5단계로 분류하고, 세계 지도 폴리곤 경계면에 분위수 등급 정보를 연동하여 공간 분석 가능 ---
# ==============================================================================
cat("3-2) 5단계 전력수요 단계구분도 생성 중...\n")
country_total <- aggregate(df_dem[["2023"]], by = list(code = df_dem$code), FUN = sum, na.rm = TRUE)
colnames(country_total)[2] <- "Demand_GWh"
country_total$Demand_TWh <- country_total$Demand_GWh / 1000

# 세계 지형 경계 데이터 로드
world <- ne_countries(scale = "medium", returnclass = "sf")
world_energy <- merge(world, country_total, by.x = "iso_a3", by.y = "code", all.x = TRUE)
world_energy <- world_energy[world_energy$iso_a3 != "ATA", ]

probs <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
breaks <- quantile(country_total$Demand_TWh, probs = probs, na.rm = TRUE)
breaks <- unique(breaks)
if (length(breaks) < 6) {
  breaks <- seq(min(country_total$Demand_TWh, na.rm=TRUE), max(country_total$Demand_TWh, na.rm=TRUE), length.out=6)
}

world_energy$level <- cut(
  world_energy$Demand_TWh, 
  breaks = breaks,
  labels = c("5단계 (하위 10% 이하)", "4단계 (하위 10% ~ 30%)", "3단계 (중간 30% ~ 70%)", "2단계 (상위 10% ~ 30%)", "1단계 (상위 10% 이상)"),
  include.lowest = TRUE
)

demand_colors <- c(
  "5단계 (하위 10% 이하)" = "#F7FBFF",
  "4단계 (하위 10% ~ 30%)" = "#DEEBF7",
  "3단계 (중간 30% ~ 70%)" = "#9ECAE1",
  "2단계 (상위 10% ~ 30%)" = "#4292C6",
  "1단계 (상위 10% 이상)" = "#08306B"
)

p_map <- ggplot(data = world_energy) +
  geom_sf(aes(fill = level), color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = demand_colors, na.value = "grey90", name = "전력수요량 수준") +
  theme_minimal() +
  labs(
    title = "전세계 국가별 총 전력수요 수준 단계구분도 (5단계)",
    subtitle = "연간 총 전력소비량(TWh) 규모에 따라 5개 분위수 그룹으로 국가별 편차 시각화",
    caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "3-1)전세계 국가별 총 전력수요 수준 단계구분도 (5단계).png"), plot = p_map, width = 11, height = 6.5, dpi = 300)

cat("SUCCESS: All 1 Power Demand visualizations generated successfully.\n")
