library("devtools")
library(geokz)
library(dplyr)
library(sf)

devtools::install_github("arodionoff/geokz")

plot(x = geokz::get_kaz_oblasts_map(Year=2024)["ADM1_EN"], cex.main = 1.0,
     pal = topo.colors(n=17, alpha = 0.5),
     main = "Oblystar (2024)")


IQR <- data(package = "geokz")
dplyr::as_tibble(IQR$results) %>% 
  dplyr::select(Item, Title) %>% 
  dplyr::filter(grepl("kaz_adm1_sf", Item))

names(geokz::kaz_adm1_sf)

geokz::kaz_adm1_sf %>%
  sf::st_drop_geometry() %>% 
  dplyr::count(ADM1_PCODE, ADM1_EN, ADM1_KK, ADM1_RU, ISO_3166_2)

Population_Density_df <- 
  data.frame(
    ISO_3166_2 = c("KZ-ABY", "KZ-AKM", "KZ-AKT", "KZ-ALM", "KZ-ATY", "KZ-VOS", "KZ-ZHA", "KZ-ZHT", "KZ-KAR", "KZ-KUS", "KZ-KZY", "KZ-MAN", "KZ-SEV", "KZ-PAV", "KZ-YUZ", "KZ-ULT", "KZ-ZAP", "KZ-AST", "KZ-ALA", "KZ-SHY"),
    Region = c("Abay", "Akmola", "Aktobe", "Almaty Region", "Atyrau", "East Kazakhstan", "Jambyl", "Zhetysu", "Karaganda", "Kostanay", "Kyzylorda", "Mangystau", "North Kazakhstan", "Pavlodar", "Turkistan", "Ulytau", "West Kazakhstan", "Astana", "Almaty city", "Shymkent"),
    Area = c(185500, 146219, 300629, 105263, 118631, 97800, 144264, 118648, 239045, 196001, 226019, 165642, 97993, 124755, 117249, 188936, 151339, 797, 683, 1170),
    Population = c(606527, 788518, 941765, 1537874, 706711, 726384, 1222811, 696976, 1134932, 828785, 843383, 791636, 527480, 753484, 2145213, 221531, 694081, 1451525, 2244824, 1231796)
  ) %>% 
  dplyr::mutate(Population_Density = round( Population / Area, 1))

knitr::kable(Population_Density_df, caption = 'Demographic statistics of <b>Kazakhstan</b>',
             format.args = list(big.mark = ' '))


dplyr::inner_join( x = get_kaz_oblasts_map( Year = 2024 ),
                   y = Population_Density_df[, c("ISO_3166_2", "Population_Density")],
                   by = c("ISO_3166_2" = "ISO_3166_2") ) %>% 
  tmap::qtm(
    shp = ., 
    fill = "Population_Density", 
    fill.n = 6,
    fill.style = "quantile",
    fill.text  = "Population_Density",
    fill.title = "чел. на кв. км",
    text = "Population_Density",
    title = "Плотность населения РК (апрель 2024)",
    format = "World"
  )
