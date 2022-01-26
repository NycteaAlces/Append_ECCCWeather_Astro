#Created by Mike Russell (NycteaAlces) 2022/01/26 to manage a specific dataset

#Directly calls on ECCC weathercan app, and suncalc app.
#Application to evaluate influence of environmental factors that may influence observations, detections or occupancy.
#Weather information relies on nearest weather station within date range, irregardless of parameters collected at the weather station except the criteria for hourly summaries. This instance uses the rounded hour/day and lat, long coordinates to assign the nearest weather station information. Currently ther eis no feature to optimize the selection of weather station basewd on filters such as comprehensive list of parameters. Hopefully this will be developed in a later version.
#Moon and solar information are assigned based on the day, time, and for some atributes that are specific to azimuth based on lat and long. Some of these parameters vary by hour, but many such as phase/rise/set will not vary substantially within the same day. As such, consideration for grouping and scale of anlysis units will be useful.

# Append_ECCCWeather_Astro
#Appends nearest ECCC weather station data and solar and/or moon metrics to table of point locations


#This file was created to handle point locations generated in ios AVENZA (e.g. data formats)
#Posting on gitHub for personal use/needs, however the references may be useful to others for their applications

#Added as a Shiny App. If ther eis interest I would be happy to modify the App to be used and applied to more generic 
#datasources.


#Additonal reference:
#From suncalc documentation, the descriptions for the columns appended with suncalc:getSunlightTimes may be useful:

# suncalc:getSunlightTimes results:
#   Available variables are :
#   
#  "sunrise" : sunrise (top edge of the sun appears on the horizon)
# 
# "sunriseEnd" : sunrise ends (bottom edge of the sun touches the horizon)
# 
# "goldenHourEnd" : morning golden hour (soft light, best time for photography) ends
# 
# "solarNoon" : solar noon (sun is in the highest position)
# 
# "goldenHour" : evening golden hour starts
# 
# "sunsetStart" : sunset starts (bottom edge of the sun touches the horizon)
# 
# "sunset" : sunset (sun disappears below the horizon, evening civil twilight starts)
# 
# "dusk" : dusk (evening nautical twilight starts)
# 
# "nauticalDusk" : nautical dusk (evening astronomical twilight starts)
# 
# "night" : night starts (dark enough for astronomical observations)
# 
# "nadir" : nadir (darkest moment of the night, sun is in the lowest position)
# 
# "nightEnd" : night ends (morning astronomical twilight starts)
# 
# "nauticalDawn" : nautical dawn (morning nautical twilight starts)
# 
# "dawn" : dawn (morning nautical twilight ends, morning civil twilight starts)
