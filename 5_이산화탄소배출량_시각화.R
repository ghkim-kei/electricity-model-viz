# 0. 패키지 설치 확인 (누락된 패키지만 설치)
required_packages <- c("openxlsx", "ggplot2", "treemapify", "maps", "rnaturalearth", "rnaturalearthdata", "sf")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(treemapify)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

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

# 2. 발전량 및 탄소 배출계수 데이터 로드
df_gen <- readWorkbook(file_path, sheet = "generation", startRow = 2)
colnames(df_gen) <- trimws(colnames(df_gen))
colnames(df_gen)[colnames(df_gen) == "Technology"] <- "tech"
colnames(df_gen)[colnames(df_gen) == "X2"] <- "tech"
df_gen$code <- toupper(trimws(df_gen[[colnames(df_gen)[grep("code|X1", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen$Group <- toupper(trimws(df_gen[[colnames(df_gen)[grep("UNICON", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen <- apply_locf(df_gen, as.character(2017:2023))

# --- [방어적 코드 1] 발전량 데이터 무결성 검사 ---
required_gen_cols <- c("tech", "Group", "code", as.character(2017:2023))
missing_gen_cols <- setdiff(required_gen_cols, colnames(df_gen))
if (length(missing_gen_cols) > 0) {
  stop(paste("ASSERT ERROR: Missing critical columns in generation sheet:", paste(missing_gen_cols, collapse = ", ")))
}
# 타입 검증 및 결측치 체크
for (yr in as.character(2017:2023)) {
  if (!is.numeric(df_gen[[yr]])) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "in generation sheet is not numeric. Converting..."))
    df_gen[[yr]] <- as.numeric(df_gen[[yr]])
  }
  if (any(is.na(df_gen[[yr]]))) {
    warning(paste("DEFENSIVE WARNING: Column", yr, "contains NA/NaN values in generation sheet."))
  }
}

df_coeff <- readWorkbook(file_path, sheet = "coeff", startRow = 2)
colnames(df_coeff) <- trimws(colnames(df_coeff))

# --- [방어적 코드 2] 배출계수 데이터 무결성 검사 ---
if (ncol(df_coeff) < 46) {
  stop(paste("ASSERT ERROR: coeff sheet columns count is too small:", ncol(df_coeff)))
}

# 엑셀 3번째 블록(32~46열)에서 2018-2020 평균 탄소 배출계수 추출
# 1열: 원소(CO2), 32열: 발전원(석탄/가스/석유/바이오), 33:46열: 권역별 배출계수 값
coeff_co2 <- df_coeff[1:12, c(1, 32, 33:46)]
colnames(coeff_co2) <- c("element", "tech", groups_order)

# 발전원 명칭 통일 및 수치형 변환
normalize_tech_coeff <- function(df) {
  df$tech <- trimws(tolower(df$tech))
  df$tech[df$tech == "biofuels"] <- "biomass"
  df$element <- trimws(toupper(df$element))
  for (g in groups_order) {
    if (!is.numeric(df[[g]])) {
      warning(paste("DEFENSIVE WARNING: Group", g, "column in coeff sheet is not numeric. Converting..."))
      df[[g]] <- as.numeric(df[[g]])
    }
    if (any(is.na(df[[g]]))) {
      warning(paste("DEFENSIVE WARNING: Group", g, "column in coeff sheet contains NA/NaN values."))
    }
  }
  return(df)
}
coeff_co2 <- normalize_tech_coeff(coeff_co2)

# 발전 연료별 탄소배출량 계산 함수
calculate_emissions_by_fuel <- function(year, coeff_set) {
  emissions_list <- list()
  combust_techs <- c("coal", "oil", "lng", "biomass")
  
  for (g in groups_order) {
    df_g <- df_gen[df_gen$Group == g, ]
    for (t in combust_techs) {
      gen_tech_name <- ifelse(t == "biomass", "Biomass", 
                       ifelse(t == "coal", "Coal", 
                       ifelse(t == "oil", "Oil", "LNG")))
      
      val_gen_gwh <- sum(df_g[tolower(trimws(df_g$tech)) == tolower(gen_tech_name), as.character(year)], na.rm = TRUE)
      co2_coef <- coeff_set[coeff_set$element == "CO2" & coeff_set$tech == t, g]
      if (length(co2_coef) == 0 || is.na(co2_coef)) co2_coef <- 0
      
      co2_mt <- (val_gen_gwh * co2_coef) / 1000000
      
      emissions_list[[length(emissions_list) + 1]] <- data.frame(
        Group = g,
        tech = t,
        CO2_Mt = co2_mt,
        stringsAsFactors = FALSE
      )
    }
  }
  return(do.call(rbind, emissions_list))
}

# 모든 연도에 단일 2018-2020 평균 탄소 배출계수를 적용하여 배출량 계산
df_emissions_fuel_2017 <- calculate_emissions_by_fuel(2017, coeff_co2)
df_emissions_fuel_2023 <- calculate_emissions_by_fuel(2023, coeff_co2)

# 요구 사항에 따라 2017~2023 전체 연도의 배출량 계산 목록 생성
emissions_all_years <- list()
for (yr in 2017:2023) {
  emissions_all_years[[as.character(yr)]] <- calculate_emissions_by_fuel(yr, coeff_co2)
}

total_co2_2017 <- aggregate(CO2_Mt ~ Group, data = df_emissions_fuel_2017, FUN = sum)
total_co2_2023 <- aggregate(CO2_Mt ~ Group, data = df_emissions_fuel_2023, FUN = sum)

# 국가 수준의 이산화탄소 배출량 산출 보조 함수 정의
calculate_country_emissions <- function(year, coeff_set) {
  combust_techs <- c("coal", "oil", "lng", "biomass")
  df_combust <- df_gen[tolower(trimws(df_gen$tech)) %in% combust_techs, ]
  df_combust$coeff_tech <- tolower(trimws(df_combust$tech))
  
  df_combust$co2_coef <- sapply(1:nrow(df_combust), function(i) {
    g <- df_combust$Group[i]
    t <- df_combust$coeff_tech[i]
    coef_row <- coeff_set[coeff_set$element == "CO2" & coeff_set$tech == t, ]
    if (nrow(coef_row) == 0 || !(g %in% colnames(coeff_set))) {
      return(0)
    } else {
      val <- coef_row[[g]]
      return(ifelse(is.na(val), 0, val))
    }
  })
  
  df_combust$CO2_Mt <- (df_combust[[as.character(year)]] * df_combust$co2_coef) / 1000000
  country_totals <- aggregate(CO2_Mt ~ code, data = df_combust, FUN = sum, na.rm = TRUE)
  country_totals <- country_totals[!is.na(country_totals$code) & country_totals$code != "", ]
  return(country_totals)
}

# ==============================================================================
# [5-1. 탄소배출량] 2023년 국가(그룹)별 이산화탄소 배출량 연료원별 기여도
# --- [5-1. 탄소배출량] 핵심 분석 및 연산 로직: 화석연료 발전량(TWh)에 2018년-2020년 연소 배출계수 평균값(average(2018-2020))을 곱하여 배출량(Mt) 산출 분석 ---
# ==============================================================================
cat("Generating 5-1) CO2 Stacked Bar Chart...\n")
df_chart1 <- merge(df_emissions_fuel_2023, geo_names, by = "Group")
df_totals1 <- merge(total_co2_2023, geo_names, by = "Group")

sorted_groups_c1 <- df_totals1$Name[order(df_totals1$CO2_Mt)]
df_chart1$Name <- factor(df_chart1$Name, levels = sorted_groups_c1)
df_totals1$Name <- factor(df_totals1$Name, levels = sorted_groups_c1)

df_chart1$tech_ko <- factor(
  ifelse(df_chart1$tech == "coal", "석탄 (Coal)",
         ifelse(df_chart1$tech == "lng", "가스 (LNG)",
                ifelse(df_chart1$tech == "oil", "석유 (Oil)", "바이오/폐기물"))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "바이오/폐기물")
)

coal_colors <- c(
  "석탄 (Coal)" = "#222222",
  "가스 (LNG)" = "#E31A1C",
  "석유 (Oil)" = "#FF7F00",
  "바이오/폐기물" = "#B2DF8A"
)

p1 <- ggplot(df_chart1, aes(x = CO2_Mt, y = Name, fill = tech_ko)) +
  geom_col(color = "#FFFFFF", linewidth = 0.2, width = 0.65) +
  geom_text(data = df_totals1, aes(x = CO2_Mt, y = Name, label = sprintf("%.1f Mt", CO2_Mt)),
            hjust = -0.15, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = coal_colors, name = "발전연료원") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 국가(그룹)별 이산화탄소(CO2) 배출량 및 연료원별 기여도 (Mt)",
    subtitle = "가로 막대의 총 길이는 총 배출량이며, 내부 누적 영역은 발전원별 탄소 배출비중을 나타냄",
    x = "이산화탄소 배출량 (Mt)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "5-1)2023년 국가(그룹)별 이산화탄소 배출량 연료원별 기여도.png"), plot = p1, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [5-2. 탄소배출량 GIS] 2023년 전세계 국가별 이산화탄소 배출 수준 단계구분도 (5단계)
# --- [5-2. 탄소배출량 GIS] 핵심 분석 및 연산 로직: 국가 단위 배출량을 계산하고 분위수 기준 5단계로 분류하여 세계 지도 폴리곤 경계면에 연동 분석 가능 ---
# ==============================================================================
cat("5-2) CO2 5단계 단계구분도 생성 중...\n")
df_country_co2_2023 <- calculate_country_emissions(2023, coeff_co2)

# 세계 지형 경계 데이터 로드
world <- ne_countries(scale = "medium", returnclass = "sf")
world_energy <- merge(world, df_country_co2_2023, by.x = "iso_a3", by.y = "code", all.x = TRUE)
world_energy <- world_energy[world_energy$iso_a3 != "ATA", ]

probs <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
breaks <- quantile(df_country_co2_2023$CO2_Mt, probs = probs, na.rm = TRUE)
breaks <- unique(breaks)
if (length(breaks) < 6) {
  breaks <- seq(min(df_country_co2_2023$CO2_Mt, na.rm=TRUE), max(df_country_co2_2023$CO2_Mt, na.rm=TRUE), length.out=6)
}

world_energy$level <- cut(
  world_energy$CO2_Mt, 
  breaks = breaks,
  labels = c("5단계 (하위 10% 이하)", "4단계 (하위 10% ~ 30%)", "3단계 (중간 30% ~ 70%)", "2단계 (상위 10% ~ 30%)", "1단계 (상위 10% 이상)"),
  include.lowest = TRUE
)

co2_map_colors <- c(
  "5단계 (하위 10% 이하)" = "#FEE5D9",
  "4단계 (하위 10% ~ 30%)" = "#FCAE91",
  "3단계 (중간 30% ~ 70%)" = "#FB6A4A",
  "2단계 (상위 10% ~ 30%)" = "#DE2D26",
  "1단계 (상위 10% 이상)" = "#A50F15"
)

p2_map <- ggplot(data = world_energy) +
  geom_sf(aes(fill = level), color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = co2_map_colors, na.value = "grey90", name = "이산화탄소 배출량 수준") +
  theme_minimal() +
  labs(
    title = "전세계 국가별 이산화탄소(CO2) 배출 수준 단계구분도 (5단계)",
    subtitle = "발전부문 연간 이산화탄소 배출량(Mt) 규모에 따라 5개 분위수 그룹으로 국가별 편차 시각화",
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

ggsave(file.path(fig_dir, "5-2)전세계 국가별 이산화탄소 배출 수준 단계구분도 (5단계).png"), plot = p2_map, width = 11, height = 6.5, dpi = 300)


# ==============================================================================
# [5-3. 배출 증감량] 2017년 대비 2023년 국가(그룹)별 이산화탄소 배출량 증감량 (연료원별 양방향 누적)
# --- [5-3. 배출 증감량] 핵심 분석 및 연산 로직: 2017년과 2023년의 그룹별 연료원 배출량 차이를 구하여 전세계 총합(WORLD)을 포함한 순(Net) 증감량 분석 ---
# ==============================================================================
cat("5-3) CO2 증감량 막대그래프 생성 중...\n")
df_change_fuel <- merge(
  df_emissions_fuel_2017,
  df_emissions_fuel_2023,
  by = c("Group", "tech"),
  suffixes = c("_2017", "_2023")
)
df_change_fuel$Change_Mt <- df_change_fuel$CO2_Mt_2023 - df_change_fuel$CO2_Mt_2017

# 전세계(WORLD) 증감량 행 생성
world_fuel_change <- aggregate(cbind(CO2_Mt_2017, CO2_Mt_2023, Change_Mt) ~ tech, data = df_change_fuel, FUN = sum)
world_fuel_change$Group <- "WORLD"
df_change_fuel <- rbind(df_change_fuel, world_fuel_change[, colnames(df_change_fuel)])
df_change_fuel <- merge(df_change_fuel, rbind(geo_names, data.frame(Group="WORLD", Name="전세계")), by = "Group")

df_net_change <- aggregate(Change_Mt ~ Group + Name, data = df_change_fuel, FUN = sum)
df_net_change$Abs_Net_Change <- abs(df_net_change$Change_Mt)
df_net_change$Abs_Net_Change[df_net_change$Group == "WORLD"] <- 999999

sorted_groups_c2 <- df_net_change$Name[order(df_net_change$Abs_Net_Change)]
df_change_fuel$Name <- factor(df_change_fuel$Name, levels = sorted_groups_c2)
df_net_change$Name <- factor(df_net_change$Name, levels = sorted_groups_c2)

df_change_fuel$tech_ko <- factor(
  ifelse(df_change_fuel$tech == "coal", "석탄 (Coal)",
         ifelse(df_change_fuel$tech == "lng", "가스 (LNG)",
                ifelse(df_change_fuel$tech == "oil", "석유 (Oil)", "바이오/폐기물"))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "바이오/폐기물")
)

# 텍스트 범례 위치 지정을 위한 그룹별 증가분 및 감소분 합산
sum_pos_df <- aggregate(Change_Mt ~ Group, data = df_change_fuel[df_change_fuel$Change_Mt > 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_pos_df)[2] <- "Sum_Pos"

sum_neg_df <- aggregate(Change_Mt ~ Group, data = df_change_fuel[df_change_fuel$Change_Mt < 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_neg_df)[2] <- "Sum_Neg"

df_net_change <- merge(df_net_change, sum_pos_df, by = "Group", all.x = TRUE)
df_net_change$Sum_Pos[is.na(df_net_change$Sum_Pos)] <- 0

df_net_change <- merge(df_net_change, sum_neg_df, by = "Group", all.x = TRUE)
df_net_change$Sum_Neg[is.na(df_net_change$Sum_Neg)] <- 0

df_net_change$x_pos <- df_net_change$Change_Mt
df_net_change$hjust_val <- ifelse(df_net_change$Change_Mt >= 0, -0.15, 1.15)

# 미국(USA)의 경우 텍스트 라벨 겹침 방지를 위해 출력 위치 조정
df_net_change$x_pos[df_net_change$Group == "USA"] <- df_net_change$Sum_Pos[df_net_change$Group == "USA"]
df_net_change$hjust_val[df_net_change$Group == "USA"] <- -0.15

df_net_change$Name <- factor(df_net_change$Name, levels = sorted_groups_c2)

p3_div <- ggplot(df_change_fuel, aes(x = Change_Mt, y = Name, fill = tech_ko)) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_col(width = 0.65, color = "#FFFFFF", linewidth = 0.1) +
  geom_text(
    data = df_net_change,
    aes(x = x_pos, y = Name, label = paste0(ifelse(Change_Mt >= 0, "+", ""), round(Change_Mt, 1), " Mt"), hjust = hjust_val),
    size = 3.0,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = coal_colors, name = "발전연료원") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "2017년 대비 2023년 국가(그룹)별 이산화탄소(CO2) 배출 연료원별 양방향 증감 (Mt)",
    subtitle = "막대의 성분은 연료원별 배출량 증감을 나타내며, 텍스트 라벨은 순(Net) 증감량을 나타냄",
    x = "이산화탄소 증감량 (Mt)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "5-3)2017년 대비 2023년 국가(그룹)별 이산화탄소 배출량 증감량.png"), plot = p3_div, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [5-4. 배출규모 점유율] 2017년 & 2023년 글로벌 발전 부문 이산화탄소 배출량 국가별 점유 비중 트리맵
# --- [5-4. 배출규모 점유율] 핵심 분석 및 연산 로직: 국가별 배출량 점유율(%)을 구하고 소수 첫째 자리 반올림 기준값에 맞게 대형(10% 이상), 중형(2%~10%), 소형(2% 미만)으로 분류 분석 ---
# ==============================================================================
generate_co2_treemap <- function(year, total_co2_data) {
  cat(sprintf("Generating 5-4) %d CO2 Share Treemap...\n", year))
  df_tree <- merge(total_co2_data, geo_names, by = "Group")
  df_tree$Share <- (df_tree$CO2_Mt / sum(df_tree$CO2_Mt)) * 100
  df_tree$Share_Label <- round(df_tree$Share, 1)
  
  df_tree$Emission_Tier <- factor(
    ifelse(df_tree$Share_Label >= 10, "대형 배출국 (비중 10% 이상)",
           ifelse(df_tree$Share_Label >= 2, "중형 배출국 (비중 2% ~ 10%)", "소형 배출국 (비중 2% 미만)")),
    levels = c("대형 배출국 (비중 10% 이상)", "중형 배출국 (비중 2% ~ 10%)", "소형 배출국 (비중 2% 미만)")
  )
  
  tier_colors <- c(
    "대형 배출국 (비중 10% 이상)" = "#991B1B",   # Deep Red
    "중형 배출국 (비중 2% ~ 10%)" = "#1E3A8A", # Deep Blue
    "소형 배출국 (비중 2% 미만)" = "#115E59"      # Deep Teal
  )
  
  p4 <- ggplot(df_tree, aes(
    area = CO2_Mt, 
    fill = Emission_Tier, 
    label = paste0(Name, "\n", round(CO2_Mt, 1), " Mt\n(", Share_Label, "%)")
  )) +
    geom_treemap(color = "white", size = 1.5) +
    geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, fontface = "bold") +
    scale_fill_manual(values = tier_colors, name = "배출 비중 구분") +
    labs(
      title = sprintf("%d년 글로벌 발전 부문 이산화탄소(CO2) 배출량 국가별 점유 비중 트리맵", year),
      subtitle = "전세계 총 발전 탄소 배출량 중 개별 국가(그룹)가 차지하는 비율(%) 및 배출규모(Mt)",
      caption = "데이터 기준: input_DB_2025_v.13_2(송부용).xlsx"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
      legend.position = "bottom"
    )
  
  ggsave(file.path(fig_dir, sprintf("5-4) %d년 글로벌 발전 부문 이산화탄소 배출량 국가별 점유 비중.png", year)), 
         plot = p4, width = 10, height = 7.5, dpi = 300)
}

generate_co2_treemap(2017, total_co2_2017)
generate_co2_treemap(2023, total_co2_2023)

cat("SUCCESS: All CO2 Emissions visualizations generated successfully.\n")
