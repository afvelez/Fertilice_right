#### Lets check the information related to the Fertilice right project ####

list_p <- c("tidyverse", "readxl", "soiltexture", "readr", 
            "ggplot2", "caret", "splines", "haven")

lapply(list_p, function(x) {
  if(!require(x, character.only = TRUE)){
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

a

### Lets upload the library ###
df <- read_dta("D:\\OneDrive - CGIAR\\R_projects\\New folder\\Data/mod_g2_long_v3.dta", 
               encoding = NULL)
labels <- read.csv("D:\\OneDrive - CGIAR\\R_projects\\New folder\\Data/Labels.csv")
rename_vector <- setNames(labels$Shp_name, labels$Label)
df_mod <- df |>
  dplyr::rename_with(~ rename_vector[.x], 
              .cols = all_of(names(rename_vector))) |>
  select(!c(id_lote_g6, g14, g9_1:g11a_5, g13_1:g13_5, row, ax_2, 
            a12a, a13, `_merge`)) |>
  as.data.frame()
unique(df_mod[["Variety"]])

# df_reshape_WM <- df_mod |>
#   group_by(ID, Apli_Event) |>
#   select(!c(Fert_M1:Fert_M5))|>
#   pivot_longer(c(Fert_WM1:Fert_WM5), names_to = "Fert_WM", values_to = "Prod_name", 
#                names_pattern = "Fert_WM(\\d+)") |>
#   mutate(Fert_Index = as.integer(Fert_WM)) |>  # Create an index from Fert_WM1 to Fert_WM5
#   relocate(c(Fert_WM, Prod_name, Fert_Index), .after = DDE) |>
#   select(ID, Apli_Event, DDE, Fert_WM, Fert_Index, Prod_name) |>
#   as.data.frame()

df_reshape_M <- df_mod |>
  group_by(ID, Apli_Event) |>
  select(!c(Fert_WM1:Fert_WM5))|>
  # pivot_longer(c(Fert_WM1:Fert_WM5), names_to = "Fert_WM", values_to = "Prod_name" ) |>
  pivot_longer(c(Fert_M1:Fert_M5), names_to = "Level", values_to = "ModNamProd", 
               names_pattern = "Fert_M(\\d+)") |>
  mutate(Index = as.integer(Level)) |>  # Create an index from Fert_M1 to Fert_M5
  relocate(c(Level, ModNamProd, Index), .after = DDE) |>
  select(ID, Apli_Event, DDE, Level, Index, ModNamProd, Location_1:Sow_Area) |>
  as.data.frame() 

# df_reshape <- merge(df_reshape_WM, df_reshape_M, by = c("ID", "Apli_Event", "Fert_Index"))

# df_reshape <- df_reshape |>
#   arrange(ID, Apli_Event, Fert_Index) |>
#   select(ID, Apli_Event, Fert_WM, Prod_name, Fert_M, ModNamProd)


## lets check differences between fertilizer names

# df_Fzer_comp <- df_reshape |>
#   mutate(Name_change = ifelse(Prod_name == ModNamProd, "Same", "Changed"))

## lets work with the modified ##

df_Fert_amou <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Quantity_1:Quantity_5),
    names_to = "Level", 
    values_to = "Fert_Quant",
    names_pattern = "Quantity_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Fert_Quant, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Uni <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Unit_1:Unit_5),
    names_to = "Level", 
    values_to = "Unit",
    names_pattern = "Unit_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Unit, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Other_uni <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Ot_uni_1:Ot_uni_5),
    names_to = "Level", 
    values_to = "Other_uni",
    names_pattern = "Ot_uni_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Other_uni, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Equiv <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Equi_Uni_1:Equi_Uni_5),
    names_to = "Level", 
    values_to = "Equiv_uni",
    names_pattern = "Equi_Uni_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Equiv_uni, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_K20 <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(K2O_1:K2O_5),
    names_to = "Level", 
    values_to = "K2O",
    names_pattern = "K2O_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, K2O, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_NTot <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Ntotal_1:Ntotal_5),
    names_to = "Level", 
    values_to = "Ntotal",
    names_pattern = "Ntotal_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Ntotal, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_NAmon <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Namoniacal_1:Namoniacal_5),
    names_to = "Level", 
    values_to = "Namoniacal",
    names_pattern = "Namoniacal_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Namoniacal, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Nnitri <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Nnitrico_1:Nnitrico_5),
    names_to = "Level", 
    values_to = "Nnitrico",
    names_pattern = "Nnitrico_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Nnitrico, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_NUrei <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Nureico_1:Nureico_5),
    names_to = "Level", 
    values_to = "Nureico",
    names_pattern = "Nureico_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Nureico, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_P2O5 <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(P2O5_1:P2O5_5),
    names_to = "Level", 
    values_to = "P2O5",
    names_pattern = "P2O5_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, P2O5, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Fert_Typ <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Tipo_Nivel_0_1:Tipo_Nivel_0_5),
    names_to = "Level", 
    values_to = "Fert",
    names_pattern = "Tipo_Nivel_0_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Fert, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Fert <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Tipo_Nivel_1_1:Tipo_Nivel_1_5),
    names_to = "Level", 
    values_to = "Fert_type",
    names_pattern = "Tipo_Nivel_1_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Fert_type, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Fert2 <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(Tipo_Nivel_2_1:Tipo_Nivel_2_5),
    names_to = "Level", 
    values_to = "Fert_type2",
    names_pattern = "Tipo_Nivel_2_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Fert_type2, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Cant_Lha <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(cantidad_litros_ha_1:cantidad_litros_ha_5),
    names_to = "Level", 
    values_to = "Qtity_L_ha",
    names_pattern = "cantidad_litros_ha_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Qtity_L_ha, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

df_Cant_Kha <- df_mod |>
  group_by(ID, Apli_Event) |>
  pivot_longer(
    cols = c(cantidad_kilos_ha_1:cantidad_kilos_ha_5),
    names_to = "Level", 
    values_to = "Qtity_Kl_ha",
    names_pattern = "cantidad_kilos_ha_(\\d+)") |>
  mutate(Index = as.integer(Level)) |>
  relocate(c(Level, Qtity_Kl_ha, Index), .after = DDE) |> 
  select(ID:Index, Location_1:Sow_Area)

############################################################################

### Lets merge all the datasets ####

df_mod_merged <- df_reshape_M |>
  merge(df_Fert_amou, by = c("ID", "DDE", "Apli_Event", 
                             "Index", "Location_1", "Location_2",
                             "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Uni, by = c("ID", "DDE", "Apli_Event", 
                             "Index", "Location_1", "Location_2",
                             "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Other_uni, by = c("ID", "DDE", "Apli_Event", 
                       "Index", "Location_1", "Location_2",
                       "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Equiv, by = c("ID", "DDE", "Apli_Event", 
                             "Index", "Location_1", "Location_2",
                             "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_K20, by = c("ID", "DDE", "Apli_Event", 
                         "Index", "Location_1", "Location_2",
                         "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_NTot, by = c("ID", "DDE", "Apli_Event", 
                       "Index", "Location_1", "Location_2",
                       "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_NAmon, by = c("ID", "DDE", "Apli_Event", 
                        "Index", "Location_1", "Location_2",
                        "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Nnitri, by = c("ID", "DDE", "Apli_Event", 
                         "Index", "Location_1", "Location_2",
                         "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_NUrei, by = c("ID", "DDE", "Apli_Event", 
                          "Index", "Location_1", "Location_2",
                          "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_P2O5, by = c("ID", "DDE", "Apli_Event", 
                         "Index", "Location_1", "Location_2",
                         "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Fert_Typ, by = c("ID", "DDE", "Apli_Event", 
                        "Index", "Location_1", "Location_2",
                        "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Fert, by = c("ID", "DDE", "Apli_Event", 
                            "Index", "Location_1", "Location_2",
                            "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Fert2, by = c("ID", "DDE", "Apli_Event", 
                        "Index", "Location_1", "Location_2",
                        "Variety", "Level", "Sow_Area"), all = TRUE) |>
  merge(df_Cant_Lha, by =c("ID", "DDE", "Apli_Event", 
                           "Index", "Location_1", "Location_2",
                           "Variety", "Level", "Sow_Area"), all =TRUE) |>
  merge(df_Cant_Kha, by = c("ID", "DDE", "Apli_Event", 
                            "Index", "Location_1", "Location_2",
                            "Variety", "Level", "Sow_Area"), all = TRUE) |>
  relocate(c(Location_1:Variety), .after = Qtity_Kl_ha) |>
  select(!Level) |>
  filter(ModNamProd != "", 
         Fert != "Bactericida",
         Fert != "Adherente",
         Fert != "Insecticida",
         Fert != "Surfactante",
         Fert != "Herbicida",
         Fert != "Fungicida",
         Fert != "Controlador biologico", 
         Fert != "No definido",
         Fert_type != "Fertilizante organico",
         !(is.na(Qtity_L_ha) & is.na(Qtity_Kl_ha))) |>
  as.data.frame()

df_mod_merged$Other_uni[df_mod_merged$Other_uni == ""] <- NA
df_mod_merged$Fert[df_mod_merged$Fert == ""] <- NA
df_mod_merged$Fert_type[df_mod_merged$Fert_type == ""] <- NA
df_mod_merged$Fert_type2[df_mod_merged$Fert_type2 == ""] <- NA

#### Once we have our data organized lets select the N fertilizers ####

df_Nitro <- df_mod_merged |>
  filter(Ntotal > 0, 
         !is.na(Qtity_Kl_ha)) |>
  as.data.frame()

df_units <- df_Nitro |>
  # filter(Location_2 == 5) |>
  select(ID:Equiv_uni, Qtity_Kl_ha:Variety) |>
  as.data.frame()

# View(df_units |>
#          filter(Qtity_Kl_ha <= 10) |>
#          distinct(ModNamProd))
# 
# View(df_units |>
#        filter(Qtity_Kl_ha > 400 & Qtity_Kl_ha <= 650) |>
#        distinct(ModNamProd))
# 
# View(df_units |>
#        filter(Qtity_Kl_ha > 250 & Qtity_Kl_ha <= 400) |>
#        distinct(ID))

# lets check how many appear in a different unit.

# Unit_count <- df_units |>
#   group_by(ModNamProd) |>
#   summarise(Total = n()) 
# 
# Unit_class <- df_units |>
#   group_by(ModNamProd, Unit) |> 
#   summarise(Unit_T = n()) |>
#   left_join(Unit_count, by = "ModNamProd") |>
#   select(ModNamProd, Unit, Unit_T)
# 
# Unit_fert <- Unit_class |>
#   pivot_wider(names_from = Unit,
#               values_from = Unit_T,
#               names_prefix = "Unit_") |>
#   left_join(Unit_count, by = "ModNamProd") |>
#   rowwise() |>
#   mutate(Units_sum = sum(!is.na(c_across(starts_with("Unit_"))))) |>
#   relocate(Total, .after = ModNamProd) |>
#   arrange(ModNamProd) |>
#   as.data.frame()
# 
# fert_ids <- df_mod_merged |>
#   filter(ModNamProd %in% Unit_fert$ModNamProd[Unit_fert$Units_sum>1]) |>
#   distinct(ModNamProd)

########################################################################

#### lets remove outliners from our reference Nitrogen database ####

Select_df <- df_units |>
  filter(!(Qtity_Kl_ha <= 10 | (Qtity_Kl_ha >= 500 & Qtity_Kl_ha <= 650))) |>
  as.data.frame()

Summary_fert <- Select_df |>
  group_by(Apli_Event, ModNamProd) |>
  summarise(
    Total_use = n(),
    Avg_use = mean(Qtity_Kl_ha, na.rm = TRUE),
    Sd= sd(Qtity_Kl_ha, na.rm = TRUE),
    median = median(Qtity_Kl_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(Total_use >= 10) |>
  as.data.frame()

Summary_fert |>
  select(Apli_Event, ModNamProd, Total_use) |>
  group_by(Apli_Event) |> 
  ggplot(aes(x= reorder(ModNamProd, Total_use), y = Total_use, fill = ModNamProd)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  coord_flip() + 
  facet_wrap( ~ Apli_Event, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

Select_df |>
  group_by(Apli_Event, ModNamProd) |>
  filter(Location_2 == 5) |>
  summarise(
    Total_use = n(),
    Avg_use = mean(Qtity_Kl_ha, na.rm = TRUE),
    Sd= sd(Qtity_Kl_ha, na.rm = TRUE),
    median = median(Qtity_Kl_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # filter(Total_use >= ) |>
  select(Apli_Event, ModNamProd, Avg_use) |>
  group_by(Apli_Event) |> 
  ggplot(aes(x= reorder(ModNamProd, Avg_use), y = Avg_use, fill = ModNamProd)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  coord_flip() + 
  facet_wrap( ~ Apli_Event, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")
  

# df_units |>
#   group_by(factor(Location_2)) |>
#   top_n(10, ModNamProd) |>
#   ggplot(aes(ModNamProd, Qtity_Kl_ha, fill = Qtity_Kl_ha)) +
#   scale_fill_gradient(low = "blue", high = "darkblue")+
#   geom_col() +
#   coord_flip()


### we will create some ranges ####

min_dose <- floor(min(Select_df$Qtity_Kl_ha, na.rm = TRUE))
max_dose <- ceiling(max(Select_df$Qtity_Kl_ha, na.rm = TRUE))
intervals <- 5
breaks <- seq(min_dose, max_dose, length.out = intervals + 1)

Org_unit <- Select_df |>
  group_by(ID, Apli_Event) |>
  mutate(Q_kg_R = as.factor(cut(Qtity_Kl_ha, breaks = breaks,
                                include.lowest = TRUE, labels = FALSE))) |>
  relocate(Q_kg_R, .before = Qtity_Kl_ha)

Unit_leyend <- c("0-81", "81-162", "162-243", "243-325", "325-406", "406-487", "487-586", "586-650")

Org_unit |> 
  group_by(ID, Apli_Event) |>
  arrange(Q_kg_R) |>
  # filter(remove_outliers(Qtity_Kl_ha)) |>
  # mutate(Fert_Quant= scales::rescale(Fert_Quant)) |>
  ggplot(aes(x = factor(Q_kg_R, levels = sort(unique(levels(Q_kg_R)))), 
             y = Qtity_Kl_ha, 
             fill = factor(Q_kg_R),
             colour = factor(Q_kg_R))) +
  geom_boxplot(position = position_dodge(width = 0.75), 
               width = 0.5 , 
               outlier.shape = NA, 
               alpha = 0.65) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.35) +
  guides(color = "none") +
  scale_fill_hue(label = Unit_leyend)

### we will evaluate the ranges present in our data ####
# Lets make a quick evaluation to observe the statistics of our data according to each region

stats <- df_units |>
  group_by(Location_2) |>
  filter(Location_2 %in% c(7,5)) |> 
  select(Qtity_Kl_ha) |>
  reframe(n = n(),
          mean = mean(Qtity_Kl_ha, na.rm = TRUE),
          sd = sd(Qtity_Kl_ha, na.rm = TRUE),
          median = median(Qtity_Kl_ha, na.rm = TRUE), 
          min = min(Qtity_Kl_ha, na.rm = TRUE), 
          max = max(Qtity_Kl_ha, na.rm = TRUE))


#######################################################################
Total_N <- df_units |>
  filter(Location_2 == 5) |>
  group_by(ID) |>
  reframe(Total_N = sum(Qtity_Kl_ha, na.rm = TRUE))

df_percent_N <- df_units |>
  filter(Location_2 == 5) |>
  group_by(ID, Apli_Event) |>
  summarise(Apli_N = sum(Qtity_Kl_ha, na.rm = TRUE)) |> 
  left_join(Total_N, by = "ID") |>  
  mutate(Percent_N = round(((Apli_N / Total_N) * 100),2)) |>
  select(ID, Apli_Event, Percent_N)

df_N_dist <- df_percent_N |>
  pivot_wider(names_from = Apli_Event, 
              values_from = Percent_N, 
              names_prefix = "Event_") |>
  left_join(Total_N, by = "ID") |>
  relocate(Total_N, .after = ID) |>
  arrange(ID) |>
  as.data.frame()

df_Nitro_sol <- df_mod_merged |>
  filter(!is.na(Qtity_L_ha)) |>
  as.data.frame()
## Nutri Inicio, Yaramidas

### Boxplot and descriptive stats ####

stats_units <- df_units |> 
  group_by(Apli_Event, Location_2) |>
  filter(remove_outliers(Fert_Quant)) |>
  # filter(Location_2 %in% c(7,5)) |>
  select(Qtity_Kl_ha) |>
  reframe(n = n(),
          mean = mean(Qtity_Kl_ha, na.rm = TRUE),
          sd = sd(Qtity_Kl_ha, na.rm = TRUE),
          median = median(Qtity_Kl_ha, na.rm = TRUE), 
          min = min(Qtity_Kl_ha, na.rm = TRUE), 
          max = max(Qtity_Kl_ha, na.rm = TRUE))

df_units |> 
  group_by(ID) |>
  filter(remove_outliers(Fert_Quant)) |>
  # mutate(Fert_Quant= scales::rescale(Fert_Quant)) |>
  ggplot(aes(x = factor(Apli_Event), 
             y = Qtity_Kl_ha, 
             fill = factor(Apli_Event),
             colour = factor(Apli_Event))) +
  geom_boxplot(position = position_dodge(width = 0.75), 
               width = 0.5 , 
               outlier.shape = NA, 
               alpha = 0.65) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
            alpha = 0.35)  
  
