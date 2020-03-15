
# coding: utf-8

import numpy as np
import pandas as pd
import matplotlib.pylab as plt
import seaborn as sns


df = pd.read_csv(".../Data/raw_weather_data.csv")
df = pd.DataFrame(df)
df = df.rename(columns = {'JAXA_GPM_L3_GSMaP_v6_operational_hourlyPrecipRateGC':"perc",
 'JAXA_GPM_L3_GSMaP_v6_reanalysis_hourlyPrecipRate':'perc_real',
 'MODIS_006_MOD11A1_LST_Day_1km':"ls_temp_day",
 'MODIS_006_MOD11A1_LST_Night_1km' : "ls_temp_night",
 'MODIS_006_MYD13A1_EVI' : "evi",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Qair_f_tavg': "humid",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Rainf_f_tavg': "perc_new",
 "NASA_FLDAS_NOAH01_C_GL_M_V001_SoilMoi00_10cm_tavg":"soil_mois",
"NASA_FLDAS_NOAH01_C_GL_M_V001_SoilTemp00_10cm_tavg":"soil_temp",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Tair_f_tavg': "ns_temp",
 'NASA_FLDAS_NOAH01_C_GL_M_V001_Wind_f_tavg': "wind_speed",
                         'adm_level':'adm_level','date':'date' })

df.head()
df['date'] = pd.to_datetime(df['date'])
df = df[((df['date'].dt.year>2012) & (df['date'].dt.year<2017))]

df = df[~((df['date'].dt.year==2013)& (df['date'].dt.month<3))]
df = df[~((df['date'].dt.year==2016)& (df['date'].dt.month>2))]

import matplotlib.dates as mdates
import matplotlib.ticker as ticker
#Visualize missing values - code was found on the Internet
def missing_values_graph(frame, obs, date, var, Yname):
    df = frame[[obs, date, var]]
    
    df[var].fillna(-1, inplace=True)

    df = df.groupby([obs, date])[var].aggregate('mean').unstack()
    df.columns = df.columns.strftime('%Y-%m')
    df[df > 0] = 1
    cmap = sns.xkcd_palette(["light grey", "medium grey"])
    plt.figure(figsize=(18, 5))
    ax = sns.heatmap(df, center=0, cmap=cmap, cbar=False,
                     cbar_kws={'ticks': [-1, 0, 1]})
    ax.set(yticklabels=[])
    ax.set_xticklabels(df, fontsize='x-large', rotation=40)
    plt.ylabel(Yname+ " (N=" + str(len(df)) + ")", fontsize=14)
    plt.tight_layout()
    plt.show()

missing_values_graph(df, "adm_level", "date", "ls_temp_day", "Present observations Per Province")
missing_values_graph(df, "adm_level", "date", "ls_temp_night", "Present observations Per Province")

