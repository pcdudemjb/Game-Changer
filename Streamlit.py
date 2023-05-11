# Import Packages
import pickle
import streamlit as st
import pandas as pd
import numpy as np

# Import Data
nbaR = pd.read_csv('nbaR.csv')
nbaR2 = nbaR.dropna()
nbaR3 = nbaR2[['fg_percent', 'x2p_percent', 'o_rtg', 'd_rtg', 'n_rtg', 'x3p_ar', 'ts_percent', 'e_fg_percent', 'ft_fga', 'opp_e_fg_percent']]

# Loading model to predict the results
model = pickle.load(open('model.pkl','rb'))

# Creating the interactive predictor values 
st.title("Predicting Number Of Wins For The Season")
fg_percent_range = np.arange(0.408, 0.546, 0.001)
fg_percent = st.select_slider("Choose Team's Field Goal Percentage:", options=fg_percent_range, format_func=lambda x: f"{x:.3f}")

x2p_percent_range = np.arange(0.427, 0.585, 0.001)
x2p_percent = st.select_slider("Choose Team's Two Point Percentage:", options=x2p_percent_range, format_func=lambda x: f"{x:.3f}")

o_rtg_range = np.arange(92.2, 118.3, 0.1)
o_rtg = st.select_slider("Choose Team's Offensive Rating:", options=o_rtg_range, format_func=lambda x: f"{x:.1f}")

d_rtg_range = np.arange(120.3, 94.1, -0.1)
d_rtg = st.select_slider("Choose Team's Defensive Rating:", options=d_rtg_range, format_func=lambda x: f"{x:.1f}")

n_rtg = st.number_input("Team's Net Rating:", (o_rtg - d_rtg))

x3p_ar_range = np.arange(0.013, 0.519, 0.001)
x3p_ar = st.select_slider("Choose Team's Percentage Of FG Attempts From 3-Point Range:", options=x3p_ar_range, format_func=lambda x: f"{x:.3f}")

ts_percent_range = np.arange(0.469, 0.61, 0.001)
ts_percent = st.select_slider("Choose Team's True Shooting Percentage:", options=ts_percent_range, format_func=lambda x: f"{x:.3f}")

e_fg_percent_range = np.arange(0.428, 0.582, 0.001)
e_fg_percent = st.select_slider("Choose Team's Effective Field Goal Percentage:", options=e_fg_percent_range, format_func=lambda x: f"{x:.3f}")

ft_fga_range = np.arange(0.143, 0.303, 0.001)
ft_fga = st.select_slider("Choose Team's Free Throws Per Field Goal Attempt Percentage:", options=ft_fga_range, format_func=lambda x: f"{x:.3f}")

opp_e_fg_percent_range = np.arange(0.433, 0.578, 0.001)
opp_e_fg_percent = st.select_slider("Choose Opposing Teams' Effective Field Goal Percentage:", options=opp_e_fg_percent_range, format_func=lambda x: f"{x:.3f}")


# ['fg_percent', 'x2p_percent', 'o_rtg', 'd_rtg', 'n_rtg', 'x3p_ar', 'ts_percent', 'e_fg_percent', 'ft_fga', 'opp_e_fg_percent']

# Create function that applies selected values into the model.  Also established limits.
def predict():
    row = np.array([fg_percent, x2p_percent, o_rtg, d_rtg, n_rtg, x3p_ar, ts_percent, e_fg_percent, ft_fga, opp_e_fg_percent])
    X = pd.DataFrame([row],columns=nbaR3.columns)
    prediction = int(model.predict(X)[0])
    prediction = max(min(prediction, 82), 0)  # Limit prediction to 82 wins maximum and 0 wins minimum.
    st.write(f"Predicted Number Of Wins: {prediction}")


# Create a button that initiates the model/output.
st.button('Predict', on_click=predict)

# Powershell code to start web based application.
#       streamlit run Streamlit.py

# http://localhost:8501

# Chicago Bulls Example Numbers: (3/25/23)
# .489
# .551
# 113.4
# 112.4
# Net
# .331
# .588
# .550
# .257
# .541
# Current Record 35-38, 9 games remaining in season. (3/25/23)
# Predicted 39 Wins.  Actual amount today...?