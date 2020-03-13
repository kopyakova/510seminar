import numpy as np
import pandas as pd

df = pd.read_csv("raw_weather_data.csv")
df = pd.DataFrame(df)
df = df.rename(columns = {'JAXA_GPM_L3_GSMaP_v6_operational_hourlyPrecipRateGC':"perc_old",
 'MODIS_006_MOD11A1_LST_Day_1km':"ls_temp_day",
 'MODIS_006_MOD11A1_LST_Night_1km' : "ls_temp_night",
 'MODIS_006_MYD13A1_EVI' : "evi",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Qair_f_tavg': "humid",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Rainf_f_tavg': "perc",
 "NASA_FLDAS_NOAH01_C_GL_M_V001_SoilMoi00_10cm_tavg":"soil_mois",
 "NASA_FLDAS_NOAH01_C_GL_M_V001_SoilTemp00_10cm_tavg":"soil_temp",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Tair_f_tavg': "ns_temp",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Wind_f_tavg': "wind_speed",
                         'adm_level':'adm_level','date':'date' })

adm = df.adm_level.unique() #list of administrative regions
var_names = df.columns      #list variable names

#Zero Kelvin values as NaN
df["ls_temp_day"] = df["ls_temp_day"].replace(0, np.nan)
df["ls_temp_night"] = df["ls_temp_night"].replace(0, np.nan)
df["ns_temp"] = df["ns_temp"].replace(0, np.nan)

#Transform the temperature to Celsius
df["ls_temp_day"] = df[["ls_temp_day"]].transform(lambda x: x*0.02-273.15)
df["ls_temp_night"] = df[["ls_temp_night"]].transform(lambda x: x*0.02-273.15)
df["ns_temp"] = df[["ns_temp"]].transform(lambda x: x-273.15)

#Night temperature below zero as NaN
df["ls_temp_night"] = df["ls_temp_night"].mask(df["ls_temp_night"] < 15)
df["ls_temp_day"] = df["ls_temp_day"].mask(df["ls_temp_day"] < 15)

#Rescale evi to have it between 0 and 1
df["evi"] = df["evi"].transform(lambda x: x*0.0001)

#Transform wind speed to m/s to km/h
df["wind_speed"] = df["wind_speed"].transform(lambda x: x*3.6)

#Transform perciptitation 
df["perc"] = df["perc"].transform(lambda x: x*3600)

#Add observation for 'NCR, City of Manila, First District'
#replicate data frame with new Hierarchical Indexes (adm primary)
df1 = df.set_index(['date', 'adm_level'])
df1 = df1.swaplevel(i='adm_level', j='date')
df1.sort_index(inplace = True)

#make a new data frame with Manila's neighbours and variables of interest (Hierarchical Indexes (date primary))
neighbours = ['NCR, Fourth District','NCR, Second District', 'NCR, Third District']
df2 = df1.loc[neighbours]
df2 = df2[['perc_old', 'humid', 'ns_temp', 'wind_speed', "perc", "soil_mois", "soil_temp"]]
df2 = df2.swaplevel(j='adm_level', i='date')

#create dictionaries with dates as keys and empty values
dates =  df.date.unique()
perc_old_Manila = dict.fromkeys(dates)
humid_Manila = dict.fromkeys(dates)
ns_temp_Manila = dict.fromkeys(dates)
wind_speed_Manila = dict.fromkeys(dates)

perc_Manila = dict.fromkeys(dates)
soil_mois_Manila = dict.fromkeys(dates)
soil_temp_Manila = dict.fromkeys(dates)

#check values in neighbouring provinces and take an average for a given date
for date in dates:
    perc_old_Manila[date] = df2.loc[date].perc_old.mean()
    humid_Manila[date] = df2.loc[date].humid.mean()
    ns_temp_Manila[date] = df2.loc[date].ns_temp.mean()
    wind_speed_Manila[date] = df2.loc[date].wind_speed.mean()
    perc_Manila[date] = df2.loc[date].perc.mean()
    soil_mois_Manila[date] = df2.loc[date].soil_mois.mean()
    soil_temp_Manila[date] = df2.loc[date].soil_temp.mean()
    
    
#uodate missing values for Manila
idx = pd.IndexSlice
df1.loc[idx['NCR, City of Manila, First District',:],"perc_old"] = np.fromiter(perc_old_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"humid"] = np.fromiter(humid_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"ns_temp"] = np.fromiter(ns_temp_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"wind_speed"] = np.fromiter(wind_speed_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"perc"] = np.fromiter(perc_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"soil_mois"] = np.fromiter(soil_mois_Manila.values(), dtype=float)
df1.loc[idx['NCR, City of Manila, First District',:],"soil_temp"] = np.fromiter(soil_temp_Manila.values(), dtype=float)


df1.loc['NCR, City of Manila, First District'] #check values fot Manila
#substitute original df with df1, where values for Manila are modified
df1.reset_index(inplace=True)
df = df1
df[df.adm_level == 'NCR, City of Manila, First District']

#substitute original df with df1, where values for Manila are modified
df1.reset_index(inplace=True)
df = df1
df[df.adm_level == 'NCR, City of Manila, First District']

df.to_csv(r'weather.csv')
