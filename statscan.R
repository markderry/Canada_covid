library(cansim)
library(data.table)
library(plotly)

deaths <- get_cansim("13-10-0710-01")
weekly_deaths <- get_cansim("13-10-0783-01")
pop <- data.table(get_cansim("17-10-0009-01"))
deaths <- data.table(deaths)
weekly_deaths <- data.table(weekly_deaths)
deaths[`Age at time of death`=="Age at time of death, all ages" & Sex == "Both sexes" & Characteristics == "Number of deaths", .(REF_DATE, GEO, VALUE)]
ddata <- weekly_deaths[Release == "October 28th, 2020" & Characteristics == "Number of deaths" & GEO == "Alberta, place of occurrence", .(GEO, deaths = VALUE, REF_DATE = as.Date(REF_DATE), REF_DATE_LAG = as.Date(shift(REF_DATE, -1)))]
ddata <- ddata[!is.na(REF_DATE_LAG)]
pdata <- pop[GEO == "Alberta", .(population = VALUE, REF_DATE = as.Date(paste0(REF_DATE, "-01")), REF_DATE_LAG = as.Date(shift(paste0(REF_DATE, "-01"), -1)))]
pdata <- pdata[is.na(REF_DATE_LAG), REF_DATE_LAG := as.Date("2020-10-01")]


setkey(ddata, "REF_DATE", "REF_DATE_LAG")
setkey(pdata, "REF_DATE", "REF_DATE_LAG")

out <- foverlaps(ddata, pdata, by.x = c("REF_DATE", "REF_DATE_LAG"), by.y = c("REF_DATE", "REF_DATE_LAG"), mult = "first")
out[, death_percentage := (deaths/population)*100]
out[, c("REF_DATE", "REF_DATE_LAG", "i.REF_DATE_LAG"):=NULL ]
setnames(out, "i.REF_DATE", "ref_date")
out[, year := substr(ref_date, 1, 4)]
out[, month := as.numeric(substr(ref_date, 6, 7))]
out[, month_day := as.numeric(substr(ref_date, 6, 7))+as.numeric(substr(ref_date, 9, 10))/31]
out



plot_ly(out, x = ~ref_date, y = ~death_percentage, type = 'scatter', mode = 'lines')
plot_ly(out, x = ~month_day, y = ~death_percentage, color = ~year, type = 'scatter', mode = 'lines+markers')%>% 
         layout(title = 'Alberta Weekly Deaths as a % of Population',
         xaxis = list(title = 'Month'),
         yaxis = list (range = c(0, 0.02), title = 'Death Percentage'))

plot_ly(out, x = ~month_day, y = ~deaths, color = ~year, type = 'scatter', mode = 'lines+markers')%>% 
  layout(title = 'Alberta Weekly Deaths',
         xaxis = list(title = 'Month'),
         yaxis = list(title = 'Deaths'))

monthly_deaths <- out[, .(deaths = sum(deaths), death_percentage = mean(death_percentage)), by = c("GEO", "year", "month")]

plot_ly(monthly_deaths, x = ~month, y = ~death_percentage, color = ~year, type = 'scatter', mode = 'lines')
plot_ly(monthly_deaths, x = ~month, y = ~deaths, color = ~year, type = 'scatter', mode = 'lines')

yearly_deaths <- out[month < 9, .(deaths = sum(deaths), death_percentage = mean(death_percentage)), by = c("year")]

plot_ly(yearly_deaths, x = ~year, y = ~death_percentage, type = 'scatter', mode = 'lines+markers')%>% 
  layout(title = 'Alberta Yearly Deaths as % of Population',
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Deaths as % of Population'))

plot_ly(yearly_deaths, x = ~year, y = ~deaths, type = 'bar')%>% 
  layout(title = 'Alberta Yearly Deaths',
         xaxis = list(title = 'Year'),
         yaxis = list(title = 'Deaths'))
