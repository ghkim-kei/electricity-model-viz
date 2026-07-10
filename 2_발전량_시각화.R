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
file_path <- "2_발전량_데이터.xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/2_발전량_데이터.xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14 groups order definition
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# Technology Color mapping definition
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

# Names mapping
geo_names <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미", "기타지역"),
  stringsAsFactors = FALSE
)

# LOCF Correction Function
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

# Load generation worksheet
df_gen <- readWorkbook(file_path, sheet = "generation", startRow = 2)
colnames(df_gen) <- trimws(colnames(df_gen))
colnames(df_gen)[colnames(df_gen) == "Technology"] <- "tech"
colnames(df_gen)[colnames(df_gen) == "X2"] <- "tech"
df_gen$Group <- toupper(trimws(df_gen[[colnames(df_gen)[grep("UNICON", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen <- apply_locf(df_gen, as.character(2017:2023))


# ==============================================================================
# [2-1] 2023년 국가(그룹)별 Coal 발전량 (TWh)
# ==============================================================================
cat("Generating 2-1) Coal Generation Chart...\n")
df_coal <- df_gen[tolower(trimws(df_gen$tech)) == "coal", ]
group_coal <- aggregate(df_coal[["2023"]], by = list(Group = df_coal$Group), FUN = sum, na.rm = TRUE)
colnames(group_coal)[2] <- "Value_TWh"
group_coal$Value_TWh <- group_coal$Value_TWh / 1000
group_coal$Group <- factor(group_coal$Group, levels = rev(groups_order))

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
    caption = "데이터 기준: 2_발전량_데이터.xlsx"
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
# ==============================================================================
cat("Generating 2-1) Clustered Generation Mix...\n")
gen_agg <- aggregate(df_gen[["2023"]], by = list(Group = df_gen$Group, tech = df_gen$tech), FUN = sum, na.rm = TRUE)
colnames(gen_agg)[3] <- "Generation_GWh"
gen_agg <- gen_agg[gen_agg$Generation_GWh > 0, ]
gen_agg$Generation_TWh <- gen_agg$Generation_GWh / 1000

gen_totals <- aggregate(Generation_TWh ~ Group, data = gen_agg, FUN = sum)
gen_agg$Group <- factor(gen_agg$Group, levels = rev(groups_order))
gen_agg$tech <- factor(gen_agg$tech, levels = names(tech_colors))
gen_totals$Group <- factor(gen_totals$Group, levels = rev(groups_order))

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
# ==============================================================================
generate_dominant_map <- function(year) {
  cat("Generating 2-2) Dominant Source Map for", year, "...\n")
  
  df_gen$tech_combined <- df_gen$tech
  df_gen$tech_combined[df_gen$tech_combined %in% c("WindOn", "WindOff")] <- "Wind"
  df_gen$tech_combined[df_gen$tech_combined %in% c("Biomass", "Waste")] <- "Biomass & Waste"
  
  # Aggregate by country code and tech combined
  country_tech <- aggregate(df_gen[[as.character(year)]], by = list(code = df_gen$code, tech_combined = df_gen$tech_combined), FUN = sum, na.rm = TRUE)
  colnames(country_tech)[3] <- "Gen_GWh"
  country_tech <- country_tech[country_tech$Gen_GWh > 0, ]
  
  # Find dominant technology per country code
  split_df <- split(country_tech, country_tech$code)
  dominant_list <- lapply(split_df, function(sub_df) sub_df[which.max(sub_df$Gen_GWh), ])
  dominant_df <- do.call(rbind, dominant_list)
  rownames(dominant_df) <- NULL
  
  # Map translation
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
  
  # Legend factor levels
  ko_levels <- c(
    "석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "원자력 (Nuclear)", 
    "수력 (Hydro)", "태양광 (Solar)", "풍력 (Wind)", 
    "바이오/폐기물", "지열 (Geothermal)", "양수 (PSH)", "데이터 없음"
  )
  
  # Color mapping
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
  
  # Load world map boundaries
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
      caption = "데이터 기준: input_DB_2025 수정본"
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

generate_dominant_map(2017)
generate_dominant_map(2023)


# ==============================================================================
# [2-3] 2023년 전세계 주요 25개국 발전원별 구성 비중 히트맵
# ==============================================================================
cat("Generating 2-3) Generation Mix Heatmap...\n")
country_totals <- aggregate(df_gen[["2023"]], by = list(Country = df_gen$code), FUN = sum, na.rm = TRUE)
colnames(country_totals)[2] <- "Total_GWh"

top_25 <- head(country_totals$Country[order(-country_totals$Total_GWh)], 25)

df_heat <- df_gen[df_gen$code %in% top_25, ]
df_heat$tech_combined <- df_heat$tech
df_heat$tech_combined[df_heat$tech_combined %in% c("WindOn", "WindOff")] <- "Wind"
df_heat$tech_combined[df_heat$tech_combined %in% c("Biomass", "Waste")] <- "Biomass"

agg_heat <- aggregate(df_heat[["2023"]], by = list(Country = df_heat$code, tech = df_heat$tech_combined), FUN = sum, na.rm = TRUE)
colnames(agg_heat)[3] <- "Value_GWh"

# Calculate shares
agg_heat$Share <- 0
for (c in unique(agg_heat$Country)) {
  total_c <- country_totals$Total_GWh[country_totals$Country == c]
  if (length(total_c) > 0 && total_c > 0) {
    agg_heat$Share[agg_heat$Country == c] <- (agg_heat$Value_GWh[agg_heat$Country == c] / total_c) * 100
  }
}

# Heatmap sorting and translation
tech_order <- c("Coal", "LNG", "Oil", "Nuclear", "Hydro", "Solar", "Wind", "Biomass", "Geothermal", "PSH")
agg_heat$tech <- factor(agg_heat$tech, levels = tech_order)
agg_heat$tech_ko <- factor(
  ifelse(agg_heat$tech == "Coal", "석탄 (Coal)",
         ifelse(agg_heat$tech == "LNG", "가스 (LNG)",
                ifelse(agg_heat$tech == "Oil", "석유 (Oil)",
                       ifelse(agg_heat$tech == "Nuclear", "원자력 (Nuclear)",
                              ifelse(agg_heat$tech == "Hydro", "수력 (Hydro)",
                                     ifelse(agg_heat$tech == "Solar", "태양광 (Solar)",
                                            ifelse(agg_heat$tech == "Wind", "풍력 (Wind)",
                                                   ifelse(agg_heat$tech == "Biomass", "바이오 (Biomass)",
                                                          ifelse(agg_heat$tech == "Geothermal", "지열 (Geothermal)", "양수 (PSH)"))))))))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "원자력 (Nuclear)", "수력 (Hydro)", "태양광 (Solar)", "풍력 (Wind)", "바이오 (Biomass)", "지열 (Geothermal)", "양수 (PSH)")
)

agg_heat$Country <- factor(agg_heat$Country, levels = rev(top_25))

p_heat <- ggplot(agg_heat, aes(x = tech_ko, y = Country, fill = Share)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradientn(
    colors = c("#FFFFFF", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"),
    limits = c(1, 100),
    na.value = "#FFFFFF",
    name = "발전비중 (%)"
  ) +
  geom_text(aes(label = ifelse(Share >= 1.0, sprintf("%.1f%%", Share), "")), size = 2.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "2023년 전세계 주요 25개국 발전원별 구성 비중 히트맵",
    subtitle = "발전 총량 기준 상위 25개국의 국가별 에너지믹스 비중 대조 (1% 미만 셀 생략)",
    x = "발전 기술 (Fossil -> Nuclear -> Renewable)",
    y = "주요 25개국 (발전량 역순 정렬)",
    caption = "데이터 기준: 2_발전량_데이터.xlsx"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", size = 9, color = "#333333"),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    panel.grid = element_blank()
  )

ggsave(file.path(fig_dir, "2-3)2023년 전세계 주요 25개국 발전원별 구성 비중 히트맵.png"), plot = p_heat, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [2-4] 2023년 전세계 국가별 총 발전량 수준 단계구분도 (5단계)
# ==============================================================================
cat("Generating 2-4) 5-class Choropleth Map...\n")
country_total <- aggregate(df_gen[["2023"]], by = list(code = df_gen$code), FUN = sum, na.rm = TRUE)
colnames(country_total)[2] <- "Total_GWh"

world <- ne_countries(scale = "medium", returnclass = "sf")
world_energy <- merge(world, country_total, by.x = "iso_a3", by.y = "code", all.x = TRUE)
world_energy <- world_energy[world_energy$iso_a3 != "ATA", ]

probs <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
breaks <- quantile(country_total$Total_GWh, probs = probs, na.rm = TRUE)
breaks <- unique(breaks)
if (length(breaks) < 6) {
  breaks <- seq(min(country_total$Total_GWh, na.rm=TRUE), max(country_total$Total_GWh, na.rm=TRUE), length.out=6)
}

world_energy$level <- cut(
  world_energy$Total_GWh, 
  breaks = breaks,
  labels = c("5단계 (하위 10% 이하)", "4단계 (하위 10% ~ 30%)", "3단계 (중간 30% ~ 70%)", "2단계 (상위 10% ~ 30%)", "1단계 (상위 10% 이상)"),
  include.lowest = TRUE
)

tier_colors <- c(
  "5단계 (하위 10% 이하)" = "#EDF8E9",
  "4단계 (하위 10% ~ 30%)" = "#BAE4B3",
  "3단계 (중간 30% ~ 70%)" = "#74C476",
  "2단계 (상위 10% ~ 30%)" = "#31A354",
  "1단계 (상위 10% 이상)" = "#006D2C"
)

p_map_c4 <- ggplot(data = world_energy) +
  geom_sf(aes(fill = level), color = "#FFFFFF", linewidth = 0.1) +
  scale_fill_manual(values = tier_colors, na.value = "grey90", name = "발전량 수준 (2023년)") +
  theme_minimal() +
  labs(
    title = "2023년 전세계 국가별 총 발전량 수준 단계구분도 (5단계)",
    subtitle = "발전 총량(TWh) 크기에 따라 5개 계층으로 국가별 기여도 매핑",
    caption = "데이터 기준: input_DB_2025 수정본"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "2-4)2023년 전세계 국가별 총 발전량 수준 단계구분도 (5단계).png"), plot = p_map_c4, width = 11, height = 6.5, dpi = 300)


# ==============================================================================
# [2-5] 2017년 대비 2023년 국가(그룹)별 발전량 변화량 (TWh) - 연료원별 누적 증감
# ==============================================================================
cat("Generating 2-5) Stacked Generation Change Chart...\n")

df_gen$tech_combined <- df_gen$tech
df_gen$tech_combined[df_gen$tech_combined %in% c("WindOn", "WindOff")] <- "Wind"
df_gen$tech_combined[df_gen$tech_combined %in% c("Biomass", "Waste")] <- "Biomass & Waste"

gen_2017 <- aggregate(df_gen[["2017"]], by = list(Group = df_gen$Group, tech = df_gen$tech_combined), FUN = sum, na.rm = TRUE)
colnames(gen_2017)[3] <- "Val_2017"
gen_2023 <- aggregate(df_gen[["2023"]], by = list(Group = df_gen$Group, tech = df_gen$tech_combined), FUN = sum, na.rm = TRUE)
colnames(gen_2023)[3] <- "Val_2023"
gen_change <- merge(gen_2017, gen_2023, by = c("Group", "tech"))
gen_change$Change_TWh <- (gen_change$Val_2023 - gen_change$Val_2017) / 1000

# World generation sum
world_gen_change <- aggregate(gen_change$Change_TWh, by = list(tech = gen_change$tech), FUN = sum, na.rm = TRUE)
colnames(world_gen_change)[2] <- "Change_TWh"
world_gen_change$Group <- "WORLD"
gen_change <- rbind(gen_change[, c("Group", "tech", "Change_TWh")], world_gen_change[, c("Group", "tech", "Change_TWh")])

# Color maps and labels
gen_change$tech_ko <- factor(
  ifelse(
    gen_change$tech == "Coal", "석탄 (Coal)",
    ifelse(gen_change$tech == "LNG", "가스 (LNG)",
    ifelse(gen_change$tech == "Oil", "석유 (Oil)",
    ifelse(gen_change$tech == "Nuclear", "원자력 (Nuclear)",
    ifelse(gen_change$tech == "Hydro", "수력 (Hydro)",
    ifelse(gen_change$tech == "Solar", "태양광 (Solar)",
    ifelse(gen_change$tech == "Wind", "풍력 (Wind)",
    ifelse(gen_change$tech == "Biomass & Waste", "바이오/폐기물",
    ifelse(gen_change$tech == "Geothermal", "지열 (Geothermal)",
    ifelse(gen_change$tech == "PSH", "양수 (PSH)", "기타")))))))))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "원자력 (Nuclear)", "수력 (Hydro)", "태양광 (Solar)", "풍력 (Wind)", "바이오/폐기물", "지열 (Geothermal)", "양수 (PSH)")
)

ko_color_map <- c(
  "석탄 (Coal)" = "#222222",
  "가스 (LNG)" = "#E31A1C",
  "석유 (Oil)" = "#FF7F00",
  "원자력 (Nuclear)" = "#6A3D9A",
  "수력 (Hydro)" = "#1F78B4",
  "태양광 (Solar)" = "#0055FF",
  "풍력 (Wind)" = "#33A02C",
  "바이오/폐기물" = "#B2DF8A",
  "지열 (Geothermal)" = "#8C564B",
  "양수 (PSH)" = "#A6CEE3"
)

abs_gen_change <- aggregate(abs(gen_change$Change_TWh), by = list(Group = gen_change$Group), FUN = sum, na.rm = TRUE)
colnames(abs_gen_change)[2] <- "Abs_Sum"
abs_gen_change$Abs_Sum[abs_gen_change$Group == "WORLD"] <- 999999
sorted_groups_gen <- abs_gen_change$Group[order(abs_gen_change$Abs_Sum)]

gen_change$Group <- factor(gen_change$Group, levels = sorted_groups_gen)

# Net change sum for text labels
df_net_change_gen <- aggregate(Change_TWh ~ Group, data = gen_change, FUN = sum)
df_net_change_gen$Group <- factor(df_net_change_gen$Group, levels = sorted_groups_gen)

# Calculate sum of positive and negative changes for outer edge placement
sum_pos_df <- aggregate(Change_TWh ~ Group, data = gen_change[gen_change$Change_TWh > 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_pos_df)[2] <- "Sum_Pos"

sum_neg_df <- aggregate(Change_TWh ~ Group, data = gen_change[gen_change$Change_TWh < 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_neg_df)[2] <- "Sum_Neg"

df_net_change_gen <- merge(df_net_change_gen, sum_pos_df, by = "Group", all.x = TRUE)
df_net_change_gen$Sum_Pos[is.na(df_net_change_gen$Sum_Pos)] <- 0

df_net_change_gen <- merge(df_net_change_gen, sum_neg_df, by = "Group", all.x = TRUE)
df_net_change_gen$Sum_Neg[is.na(df_net_change_gen$Sum_Neg)] <- 0

df_net_change_gen$x_pos <- df_net_change_gen$Change_TWh
df_net_change_gen$hjust_val <- ifelse(df_net_change_gen$Change_TWh >= 0, -0.15, 1.15)

# Specific overrides for USA and EUR to avoid overlap (place on the opposite end of the stacked bar)
df_net_change_gen$x_pos[df_net_change_gen$Group == "USA"] <- df_net_change_gen$Sum_Neg[df_net_change_gen$Group == "USA"]
df_net_change_gen$hjust_val[df_net_change_gen$Group == "USA"] <- 1.15

df_net_change_gen$x_pos[df_net_change_gen$Group == "EUR"] <- df_net_change_gen$Sum_Pos[df_net_change_gen$Group == "EUR"]
df_net_change_gen$hjust_val[df_net_change_gen$Group == "EUR"] <- -0.15

p_chg_gen <- ggplot() +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_bar(
    data = gen_change, 
    aes(x = Change_TWh, y = Group, fill = tech_ko), 
    stat = "identity", 
    position = "stack", 
    width = 0.65
  ) +
  geom_text(
    data = df_net_change_gen,
    aes(
      x = x_pos, 
      y = Group, 
      label = paste0(ifelse(Change_TWh >= 0, "+", ""), round(Change_TWh, 1), " TWh"),
      hjust = hjust_val
    ),
    size = 3.0,
    fontface = "bold"
  ) +
  scale_fill_manual(values = ko_color_map, name = "발전원") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "2017년 대비 2023년 국가(그룹)별 발전량 변화량 (TWh)",
    subtitle = "막대는 각 발전원별 발전량 증감(TWh)을 누적하여 표시함",
    x = "발전량 증감량 (TWh)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025 수정본"
  ) +
  theme(
    axis.text.y = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#333333"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "2-5)2017년 대비 2023년 국가(그룹)별 발전량 변화량 (TWh).png"), plot = p_chg_gen, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [2-5] 2017년 대비 2023년 국가(그룹)별 Coal 발전량 변화량 (TWh)
# ==============================================================================
cat("Generating 2-5) Coal Generation Change Chart...\n")

# Re-run aggregation of changes specifically for Coal
df_change_list_coal <- list()
for (g in groups_order) {
  df_g <- df_gen[df_gen$Group == g, ]
  val_2017 <- sum(df_g[df_g$tech == "Coal", "2017"], na.rm = TRUE) / 1000
  val_2023 <- sum(df_g[df_g$tech == "Coal", "2023"], na.rm = TRUE) / 1000
  df_change_list_coal[[length(df_change_list_coal) + 1]] <- data.frame(
    Group = g,
    Change_TWh = val_2023 - val_2017,
    stringsAsFactors = FALSE
  )
}
df_coal_change <- do.call(rbind, df_change_list_coal)
df_coal_change$Abs_Change <- abs(df_coal_change$Change_TWh)

world_coal_change <- data.frame(
  Group = "WORLD",
  Change_TWh = sum(df_coal_change$Change_TWh),
  Abs_Change = 999999
)
df_coal_change <- rbind(df_coal_change, world_coal_change)
df_coal_change <- merge(df_coal_change, rbind(geo_names, data.frame(Group="WORLD", Name="전세계")), by = "Group")

# Use sorted_groups_gen order but matching group names
name_order_match <- geo_names$Name[match(sorted_groups_gen, geo_names$Group)]
name_order_match[is.na(name_order_match)] <- "전세계"
df_coal_change$Name <- factor(df_coal_change$Name, levels = name_order_match)
df_coal_change$Color_Cat <- ifelse(df_coal_change$Change_TWh > 0, "증가", "감축")
change_colors <- c("증가" = "#E31A1C", "감축" = "#1F78B4")

p_chg_coal <- ggplot(df_coal_change, aes(x = Change_TWh, y = Name, fill = Color_Cat)) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_col(width = 0.65, color = "#FFFFFF", linewidth = 0.1) +
  geom_text(
    aes(label = paste0(ifelse(Change_TWh >= 0, "+", ""), round(Change_TWh, 1), " TWh")),
    hjust = ifelse(df_coal_change$Change_TWh >= 0, -0.15, 1.15),
    size = 3.0,
    fontface = "bold"
  ) +
  scale_fill_manual(values = change_colors, name = "석탄 발전량 증감 분류") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "2017년 대비 2023년 국가(그룹)별 Coal 발전량 변화량 (TWh)",
    subtitle = "붉은색 막대는 석탄 발전 증가국, 파란색 막대는 석탄 발전 감소 국가군을 나타냄",
    x = "석탄 발전량 변화량 (TWh)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025 수정본"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "2-5)2017년 대비 2023년 국가(그룹)별 Coal 발전량 변화량 (TWh).png"), plot = p_chg_coal, width = 11, height = 7, dpi = 300)

cat("SUCCESS: All 8 Power Generation visualizations generated successfully.\n")
