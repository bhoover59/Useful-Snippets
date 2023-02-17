def DiurnalAvg(df, TimeColumn):
    # Convert time & create hourly bins
    df[TimeColumn] = pd.to_datetime(df[TimeColumn])
    df["Hour"] = df[TimeColumn].dt.hour

    # Mean of each bin
    df_avg = df.groupby("Hour").mean()
    df_avg.reset_index(inplace = True)

    # Standard deviation of each bin
    df_avg_sd = df.groupby("Hour").std()
    df_avg_sd.rename(columns = lambda x: x + "_sd", inplace = True)
    df_avg_sd.reset_index(inplace = True)

    # Count number of points in each bin
    df_avg_count = df.groupby("Hour").count()
    df_avg_count.rename(columns = lambda x: x + "_count", inplace = True)
    df_avg_count.reset_index(inplace = True)

    # Merge data frames
    df_tot = df_avg.merge(df_avg_sd, on = "Hour", how = "outer").merge(df_avg_count, on = "Hour", how = "outer")
    
    return df_tot
