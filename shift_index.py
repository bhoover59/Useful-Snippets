def shift_index(df, column, shift_by):
    df = df.copy()
    shifted_index = (df.index + shift_by) % 24
    df.index = shifted_index
    df[column] = df[column].shift(shift_by)
    df.loc[df.index[:shift_by], column] = df[column].iloc[-shift_by:].values
    return df
