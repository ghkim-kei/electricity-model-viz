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

# 발전량(generation) 시트 로드
df_gen <- readWorkbook(file_path, sheet = "generation", startRow = 2)
colnames(df_gen) <- trimws(colnames(df_gen))
colnames(df_gen)[colnames(df_gen) == "Technology"] <- "tech"
colnames(df_gen)[colnames(df_gen) == "X2"] <- "tech"
df_gen$code <- toupper(trimws(df_gen[[colnames(df_gen)[grep("code|X1", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen$Group <- toupper(trimws(df_gen[[colnames(df_gen)[grep("UNICON", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen <- apply_locf(df_gen, as.character(2017:2023))

# --- [DEFENSIVE CODE] Check generation data integrity ---
required_gen_cols <- c("tech", "Group", "code", as.character(2017:2023))
missing_gen_cols <- setdiff(required_gen_cols, colnames(df_gen))
if (length(missing_gen_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in generation sheet:", paste(missing_gen_cols, collapse = ", ")))
}
for (yr in as.character(2017:2023)) {
  if (!is.numeric(df_gen[[yr]])) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "in generation sheet is not numeric. Converting..."))
    df_gen[[yr]] <- as.numeric(df_gen[[yr]])
  }
  if (any(is.na(df_gen[[yr]]))) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "contains NA/NaN values in generation sheet."))
  }
}


# ==============================================================================
# [2-1] 2023년 국가(그룹)별 Coal 발전량 (TWh)
# --- [2-1. 발전량 - 석탄 발전량] 핵심 분석 및 연산 로직: GWh 단위인 석탄 발전량을 1,000으로 나누어 TWh 단위로 환산 및 합산 분석 ---
# ==============================================================================
cat("Generating 2-1) Coal Generation Chart...\n")
df_coal <- df_gen[tolower(trimws(df_gen$tech)) == "coal", ]
group_coal <- aggregate(df_coal[["2023"]], by = list(Group = df_coal$Group), FUN = sum, na.rm = TRUE)
colnames(group_coal)[2] <- "Value_TWh"
group_coal$Value_TWh <- group_coal$Value_TWh / 1000
# Y축 정렬 시 대용량이 상단에 오도록 발전량 기준 오름차순 정렬
sorted_groups_coal <- group_coal$Group[order(group_coal$Value_TWh)]
group_coal$Group <- factor(group_coal$Group, levels = sorted_groups_coal)

p_coal <- ggplot(group_coal, aes(x = Value_TWh, y = Group)) +
  geom_bar(stat = "identity", fill = "#222222", width = 0.6) +
  geom_text(aes(label = ifelse(Value_TWh > 0, paste0(round(Value_TWh, 1), " TWh"), "")), 
            hjust = -0.1, size = 3.2, fontface = "bold") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 국가(그룹)별 Coal 발전량 (TWh)",
    subtitle = "발전량 중 석탄(Coal)에 해당하는 발전량만 필터링한 결과 (GWh -> TWh 환산)",
    x = "발전량 (TWh)",
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

ggsave(file.path(fig_dir, "2-1)2023년 국가(그룹)별 Coal 발전량 (TWh).png"), plot = p_coal, width = 10, height = 6.5, dpi = 300)


# ==============================================================================
# [2-1_2023] 국가(그룹)별 발전량 믹스 구성비 및 총 발전량 (TWh)
# --- [2-1. 발전량] 핵심 분석 및 연산 로직: 연도별 전력원 믹스를 전체 발전량(TWh) 대비 전원별 구성비(%) 누적으로 분석 ---
# ==============================================================================
cat("Generating 2-1) Clustered Generation Mix...\n")
gen_agg <- aggregate(df_gen[["2023"]], by = list(Group = df_gen$Group, tech = df_gen$tech), FUN = sum, na.rm = TRUE)
colnames(gen_agg)[3] <- "Generation_GWh"
gen_agg <- gen_agg[gen_agg$Generation_GWh > 0, ]
gen_agg$Generation_TWh <- gen_agg$Generation_GWh / 1000

gen_totals <- aggregate(Generation_TWh ~ Group, data = gen_agg, FUN = sum)
# Y축 정렬 시 대용량이 상단에 오도록 총 발전량 기준 오름차순 정렬
sorted_groups_mix <- gen_totals$Group[order(gen_totals$Generation_TWh)]
gen_agg$Group <- factor(gen_agg$Group, levels = sorted_groups_mix)
gen_agg$tech <- factor(gen_agg$tech, levels = names(tech_colors))
gen_totals$Group <- factor(gen_totals$Group, levels = sorted_groups_mix)

p_mix <- ggplot(gen_agg, aes(x = Generation_TWh, y = Group, fill = tech)) +
  geom_col(color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = tech_colors, name = "발전연료원") +
  geom_text(data = gen_totals, aes(x = Generation_TWh, y = Group, label = sprintf("%.1f TWh", Generation_TWh)),
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
    title = "2023년 국가(그룹)별 발전량 믹스 구성비 및 총 발전량 (TWh)",
    x = "발전량 (TWh)",
    y = "국가 (그룹)"
  ) +
  xlim(0, max(gen_totals$Generation_TWh) * 1.15)

ggsave(file.path(fig_dir, "2-1_2023년 국가(그룹)별 발전량 믹스 구성비 및 총 발전량 (TWh).png"), plot = p_mix, width = 10, height = 6.5, dpi = 300)


# ==============================================================================
# Helper for Dominant Source Maps (2017 & 2023)
# --- [2-2 주력발전원 GIS] 핵심 분석 및 연산 로직: 국가별로 10개 발전원 중 가장 큰 발전량을 가진 1대 대표 발전원을 추출하고, 국가 3글자 코드를 기반으로 세계 지도 폴리곤 경계선에 매핑하여 공간 분석 가능 ---
# [통합 처리 사유]
# 1) 지도 가독성 확보: 유사 재생원(육상/해상풍력 -> 풍력, 바이오/폐기물 -> 바이오/폐기물) 통합으로 범례 단순화
# 2) 주력원 판별 오류 방지: 개별 분류 시 분할되어 발생할 수 있는 순위 왜곡(예: 육상 10% + 해상 12% = 풍력 22%가 가스 15%를 제치고 실제 1위로 판별되도록 유도)을 방지
# ==============================================================================
generate_dominant_map <- function(year) {
  cat("Generating 2-2) Dominant Source Map for", year, "...\n")
  
  df_gen$tech_combined <- df_gen$tech
  df_gen$tech_combined[df_gen$tech_combined %in% c("WindOn", "WindOff")] <- "Wind"
  df_gen$tech_combined[df_gen$tech_combined %in% c("Biomass", "Waste")] <- "Biomass & Waste"
  
  # 국가 및 결합 발전원별 합산
  country_tech <- aggregate(df_gen[[as.character(year)]], by = list(code = df_gen$code, tech_combined = df_gen$tech_combined), FUN = sum, na.rm = TRUE)
  colnames(country_tech)[3] <- "Gen_GWh"
  country_tech <- country_tech[country_tech$Gen_GWh > 0, ]
  
  # 국가별 최대 발전량을 가진 주력 발전원 판별
  split_df <- split(country_tech, country_tech$code)
  dominant_list <- lapply(split_df, function(sub_df) sub_df[which.max(sub_df$Gen_GWh), ])
  dominant_df <- do.call(rbind, dominant_list)
  rownames(dominant_df) <- NULL
  
  # 한글 명칭 변환
  dominant_df$tech_ko <- ifelse(dominant_df$tech_combined == "Coal", "석탄 (Coal)",
                         ifelse(dominant_df$tech_combined == "LNG", "가스 (LNG)",
                         ifelse(dominant_df$tech_combined == "Oil", "석유 (Oil)",
                         ifelse(dominant_df$tech_combined == "Nuclear", "원자력 (Nuclear)",
                         ifelse(dominant_df$tech_combined == "Hydro", "수력 (Hydro)",
                         ifelse(dominant_df$tech_combined == "Solar", "태양광 (Solar)",
                         ifelse(dominant_df$tech_combined == "Wind", "풍력 (Wind)",
                         ifelse(dominant_df$tech_combined == "Biomass & Waste", "바이오/폐기물",
                         ifelse(dominant_df$tech_combined == "Geothermal", "지열 (Geothermal)",
                         ifelse(dominant_df$tech_combined == "PSH", "양수 (PSH)", "데이터 없음"))))))))))
  
  # 범례 팩터 레벨 설정
  ko_levels <- c(
    "석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "원자력 (Nuclear)", 
    "수력 (Hydro)", "태양광 (Solar)", "풍력 (Wind)", 
    "바이오/폐기물", "지열 (Geothermal)", "양수 (PSH)", "데이터 없음"
  )
  
  # 범례 색상 매핑 정의
  ko_color_map <- c(
    "석탄 (Coal)" = "#222222",        # 검정색
    "가스 (LNG)" = "#E31A1C",         # 빨간색
    "석유 (Oil)" = "#FF7F00",         # 주황색
    "원자력 (Nuclear)" = "#6A3D9A",     # 보라색
    "수력 (Hydro)" = "#1F78B4",       # 하늘색
    "태양광 (Solar)" = "#0055FF",      # 파란색
    "풍력 (Wind)" = "#33A02C",        # 초록색
    "바이오/폐기물" = "#B2DF8A",       # 연두색
    "지열 (Geothermal)" = "#8C564B",   # 갈색
    "양수 (PSH)" = "#A6CEE3",          # 연하늘색
    "데이터 없음" = "#EAEAEA"         # 회색
  )
  
  # 세계 지형 경계 데이터 로드
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world[world$iso_a3 != "ATA", ]
  
  world_energy <- merge(world, dominant_df, by.x = "iso_a3", by.y = "code", all.x = TRUE)
  world_energy$tech_ko[is.na(world_energy$tech_ko)] <- "데이터 없음"
  world_energy$tech_ko <- factor(world_energy$tech_ko, levels = ko_levels)
  
  p_map <- ggplot(data = world_energy) +
    geom_sf(aes(fill = tech_ko), color = "#FFFFFF", linewidth = 0.1) +
    scale_fill_manual(values = ko_color_map, name = "주력 발전원 (최대 발전량 기준)") +
    theme_minimal() +
    labs(
      title = paste0(year, "년 전세계 국가별 주력 발전원(최대 비중) 분포도"),
      subtitle = "각 국가에서 연간 발전량(GWh)이 가장 많은 1대 대표 에너지원을 기준으로 채색함",
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
  
  ggsave(file.path(fig_dir, sprintf("2-2)%d년 전세계 국가별 주력 발전원(최대 비중) 분포도.png", year)), plot = p_map, width = 11, height = 6.5, dpi = 300)
}

generate_dominant_map(2023)

cat("SUCCESS: All 3 Power Generation visualizations generated successfully.\n")
