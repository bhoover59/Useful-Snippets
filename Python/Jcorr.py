def Jcorr(df_Diurnal):
    for i in range(0, len(df_Diurnal)):
        if df_Diurnal.loc[i,"JNO2_TUV"] == 0:
            df_Diurnal.loc[i,"JNO2_TUV"] = df_Diurnal.loc[22, "JNO2_TUV"]
    df_Diurnal['Jcorr'] = df_Diurnal['JNO2'] / df_Diurnal['JNO2_TUV']
    for i in range(0, len(df_Diurnal)):
            if df_Diurnal.loc[i,"Jcorr"] > 1:
                df_Diurnal.loc[i,"Jcorr"] = df_Diurnal.loc[21, "Jcorr"]
            df_Diurnal.loc[7, "Jcorr"] = df_Diurnal.loc[21, "Jcorr"]
    df_Diurnal = df_Diurnal.fillna(df_Diurnal.loc[21, "Jcorr"]) # replace NA with value of row 21 
    plt.plot(df_Diurnal['Hours'], df_Diurnal['Jcorr'], color = 'blue')
    plt.title("Jcorr") 
    df_Diurnal['J_HONO'] = df_Diurnal['JHONO_TUV'] * df_Diurnal['Jcorr']
