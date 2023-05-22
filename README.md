<h1>Electricity Consumption Forecasting using MLP-NN</h1>
<h2>Description</h2>
<p>This repository contains code for forecasting electricity consumption using multilayer neural networks (MLP-NN). The goal is to predict the next day's electricity consumption for a specific hour based on historical data.</p>
<h2>Dataset</h2>
<p>The dataset used in this project is the hourly electricity consumption data for the University Building at 115 New Cavendish Street, London, for the years 2018 and 2019. The provided dataset (UoW_consumption.xlsx) includes daily electricity consumption data for three hours (20:00, 19:00, and 18:00) for the specified period.</p>
<h2>Objectives</h2>
<p>The project is divided into two subtasks:</p>
<h3>Subtask 1: Autoregressive (AR) Approach</h3>
<ul>
  <li>Experiment with various input vectors up to (t-4) level.</li>
  <li>Investigate the influence of the (t-7) load value on the forecasting performance.</li>
  <li>Construct input/output matrices (I/O) for MLP training/testing using time-delayed electricity loads.</li>
  <li>Normalize the I/O matrices for MLP modeling.</li>
  <li>Develop MLP models with different internal network structures.</li>
  <li>Evaluate the testing performance of the models using statistical indices (RMSE, MAE, MAPE, sMAPE).</li>
  <li>Create a comparison table of the models' performance and provide a brief description of each model's structure.</li>
  <li>Analyze the efficiency of the best one-hidden layer and two-hidden layer networks based on the total number of weight parameters.</li>
</ul>
<h3>Subtask 2: Nonlinear Autoregressive Exogenous (NARX) Approach</h3>
<p>In addition to the AR approach, we consider additional input vectors by including information from the 19th and 18th hour attributes. The objectives for this subtask are similar to Subtask 1, but the input vectors and model structures will differ.</p>

<h2>Implementation Details</h2>
<ol>
        <li>Lag the data to create time-delayed values for input variables.</li>
        <li>Generate a list of column combinations representing different input vectors.</li>
        <li>Create input/output matrices (I/O) for each column combination.</li>
        <li>Normalize the I/O matrices to prepare data for MLP modeling.</li>
        <li>Develop MLP models with various internal network structures.</li>
        <li>Evaluate the performance of each model using statistical indices.</li>
        <li>Create a comparison table of the model performances.</li>
        <li>Analyze the efficiency of the best-performing one-hidden layer and two-hidden layer networks.</li>
        <li>Implement the NARX approach following a similar procedure as the AR approach.</li>
        <li>Provide graphical representations of the results (prediction output vs. desired output).</li>
        <li>Include the full code developed for these tasks as an appendix in the report.</li>
    </ol>
    <h2>Licensing</h2>
    <p>This project is licensed under the MIT License. See the <a href="LICENSE">LICENSE</a> file for more information.</p>
