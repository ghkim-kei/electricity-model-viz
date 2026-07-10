# 0. Package Installation Check (Install only if missing)
required_packages <- c("openxlsx", "ggplot2", "treemapify")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(treemapify)

# 1. Path Setup
file_path <- "1_발전설비_데이터.xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/1_발전설비_데이터.xlsx"
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

# Load capacity worksheet
df_cap <- readWorkbook(file_path, sheet = "capacity", startRow = 2)
colnames(df_cap) <- trimws(colnames(df_cap))
colnames(df_cap)[colnames(df_cap) == "Technology"] <- "tech"
colnames(df_cap)[colnames(df_cap) == "X2"] <- "tech"
df_cap$Group <- toupper(trimws(df_cap[[colnames(df_cap)[grep("UNICON", colnames(df_cap), ignore.case = TRUE)[1]]]]))
df_cap <- apply_locf(df_cap, as.character(2017:2023))

# Combine Wind and Biomass/Waste for Treemaps
df_cap$tech_combined <- df_cap$tech
df_cap$tech_combined[df_cap$tech_combined %in% c("WindOn", "WindOff")] <- "Wind"
df_cap$tech_combined[df_cap$tech_combined %in% c("Biomass", "Waste")] <- "Biomass & Waste"


# ==============================================================================
# [1-1] 2023년 국가(그룹)별 Coal 설비용량 (GW)
# ==============================================================================
cat("Generating 1-1) Coal Capacity Map...\n")
df_coal <- df_cap[tolower(trimws(df_cap$tech)) == "coal", ]
group_coal <- aggregate(df_coal[["2023"]], by = list(Group = df_coal$Group), FUN = sum, na.rm = TRUE)
colnames(group_coal)[2] <- "Value_GW"
group_coal$Group <- factor(group_coal$Group, levels = rev(groups_order))

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
    caption = "데이터 기준: input_DB_2025 수정본"
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
# [1-1] 2023년 국가(그룹)별 발전설비 구성비 및 총 설비용량 (GW)
# ==============================================================================
cat("Generating 1-1) Clustered Capacity Mix...\n")
cap_agg <- aggregate(df_cap[["2023"]], by = list(Group = df_cap$Group, tech = df_cap$tech), FUN = sum, na.rm = TRUE)
colnames(cap_agg)[3] <- "Capacity_GW"
cap_agg <- cap_agg[cap_agg$Capacity_GW > 0, ]

cap_totals <- aggregate(Capacity_GW ~ Group, data = cap_agg, FUN = sum)
cap_agg$Group <- factor(cap_agg$Group, levels = rev(groups_order))
cap_agg$tech <- factor(cap_agg$tech, levels = names(tech_colors))
cap_totals$Group <- factor(cap_totals$Group, levels = rev(groups_order))

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


# ==============================================================================
# [1-2] 2023년 전세계 발전설비 용량의 에너지원별 비중 트리맵
# ==============================================================================
cat("Generating 1-2) Treemap by Energy Source...\n")
world_tech <- aggregate(df_cap[["2023"]], by = list(tech = df_cap$tech_combined), FUN = sum, na.rm = TRUE)
colnames(world_tech)[2] <- "Value_GW"
world_tech <- world_tech[world_tech$Value_GW > 0, ]
world_tech$Share <- (world_tech$Value_GW / sum(world_tech$Value_GW)) * 100

world_tech$tech_ko <- ifelse(
  world_tech$tech == "Coal", "석탄 (Coal)",
  ifelse(world_tech$tech == "LNG", "가스 (LNG)",
  ifelse(world_tech$tech == "Oil", "석유 (Oil)",
  ifelse(world_tech$tech == "Nuclear", "원자력 (Nuclear)",
  ifelse(world_tech$tech == "Hydro", "수력 (Hydro)",
  ifelse(world_tech$tech == "Solar", "태양광 (Solar)",
  ifelse(world_tech$tech == "Wind", "풍력 (Wind)",
  ifelse(world_tech$tech == "Biomass & Waste", "바이오/폐기물",
  ifelse(world_tech$tech == "Geothermal", "지열 (Geothermal)",
  ifelse(world_tech$tech == "PSH", "양수 (PSH)", "기타"))))))))))

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
world_tech$text_color <- ifelse(world_tech$tech_ko %in% c("바이오/폐기물", "양수 (PSH)", "지열 (Geothermal)"), "black", "white")

p_tree_tech <- ggplot(world_tech, aes(
  area = Value_GW, 
  fill = tech_ko, 
  label = paste0(tech_ko, "\n", round(Value_GW, 1), " GW\n(", round(Share, 1), "%)")
)) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(aes(colour = text_color), place = "centre", reflow = TRUE, fontface = "bold") +
  scale_fill_manual(values = ko_color_map, name = "발전설비원") +
  scale_colour_identity() +
  labs(
    title = "2023년 전세계 발전설비 용량의 에너지원별 비중 트리맵",
    subtitle = "전세계 설비용량 중 개별 에너지원이 차지하는 비율(%) 및 설비규모(GW)",
    caption = "데이터 기준: 1_발전설비_데이터.xlsx (capacity 시트)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "1-2)2023년 전세계 발전설비 용량의 에너지원별 비중 트리맵.png"), plot = p_tree_tech, width = 10, height = 7.5, dpi = 300)


# ==============================================================================
# [1-2] 2023년 전세계 발전설비 용량의 국가(그룹)별 비중 트리맵
# ==============================================================================
cat("Generating 1-2) Treemap by Country Group...\n")
world_group <- aggregate(df_cap[["2023"]], by = list(Group = df_cap$Group), FUN = sum, na.rm = TRUE)
colnames(world_group)[2] <- "Value_GW"
world_group <- world_group[world_group$Value_GW > 0, ]
world_group$Share <- (world_group$Value_GW / sum(world_group$Value_GW)) * 100

p_tree_group <- ggplot(world_group, aes(
  area = Value_GW, 
  fill = Value_GW, 
  label = paste0(Group, "\n", round(Value_GW, 1), " GW\n(", round(Share, 1), "%)")
)) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, fontface = "bold") +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "설비용량 (GW)") +
  labs(
    title = "2023년 전세계 발전설비 용량의 국가(그룹)별 비중 트리맵",
    subtitle = "전세계 총 발전설비용량 중 개별 국가(그룹)가 차지하는 비율(%) 및 설비규모(GW)",
    caption = "데이터 기준: 1_발전설비_데이터.xlsx (capacity 시트)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "1-2)2023년 전세계 발전설비 용량의 국가(그룹)별 비중 트리맵.png"), plot = p_tree_group, width = 10, height = 7.5, dpi = 300)

cat("SUCCESS: All 4 Power Capacity visualizations generated successfully.\n")
