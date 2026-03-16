# Stock Price Prediction and Embedding Analysis Plan

This document outlines a plan to perform a deep learning analysis on stock market data. The primary goals are to predict future stock prices using a Gated Recurrent Unit (GRU) model and to explore the relationships between different stocks by analyzing the learned embedding matrix.

This project will be executed within a Quarto (`.qmd`) document, making it a self-contained, reproducible report with code, visualizations, and narrative explanations.

---

## 1. Introduction & Setup

*   **Objective**: Briefly introduce the project's goal: to build a GRU-based neural network in PyTorch to predict the next day's stock price and to analyze the learned stock embeddings to uncover relationships between different companies.
*   **Dataset**: Describe the provided `all_stocks_2017-01-01_to_2018-01-01.csv` dataset, noting its columns (`Date`, `Open`, `High`, `Low`, `Close`, `Volume`, `Name`). Mention the one-year time frame.
*   **Tools**: List the primary Python libraries: `quarto`, `pandas`, `numpy`, `scikit-learn`, `pytorch`, `matplotlib`, and `seaborn`.
*   **Quarto Setup**: Create the initial `.qmd` file and set up the YAML header to execute Python code.

## 2. Data Loading and Preprocessing

This section will focus on getting the data ready for the model.

*   **Load Data**: Use `pandas` to load the CSV into a DataFrame.
*   **Data Cleaning**:
    *   Parse the `Date` column into datetime objects.
    *   Check for and handle missing values. The data preview shows some missing values; a forward-fill (`fillna(method='ffill')`) strategy is a reasonable approach for time-series data.
*   **Exploratory Data Analysis (EDA)**:
    *   Plot histograms of the price and volume features to assess their distribution. Decide if a log transformation is necessary to reduce skewness.
    *   Plot the 'Close' price over time for a few sample stocks (e.g., `AAPL`, `GOOGL`, `XOM`) to visualize their trends.
    *   Create a list of all unique stock tickers (`Name`) present in the dataset.
*   **Feature Engineering**:
    *   **Log Transformation**: Apply a log transformation (e.g., `numpy.log1p`) to the price and volume features (`Open`, `High`, `Low`, `Close`, `Volume`) to normalize their distributions. This should be done before scaling.
    *   **Stock Ticker Mapping**: Create a dictionary to map each unique stock `Name` to an integer index (e.g., `{'MMM': 0, 'AXP': 1, ...}`). This is essential for the embedding layer.
*   **Data Splitting (Chronological)**:
*   **Feature Scaling**:
    *   Neural networks perform best with normalized data. Use `scikit-learn`'s `MinMaxScaler`.
    *   **Important**: For each stock, fit a scaler *only* on its training data. Then, use that *same fitted scaler* to transform the validation and test sets for that stock. This prevents information from the future (validation/test sets) from leaking into the training process.
*   **Sequence Creation (Sliding Window)**:
    *   Define a `sequence_length` (e.g., 30 days).
    *   Write a function that takes the scaled data and creates input/output pairs. For each stock, it will iterate through the data, creating sequences.
    *   An input `X` will be the data from day `t` to `t + 29`.
    *   The corresponding output `y` will be the 'Close' price on day `t + 30`.
    *   This process should be applied to the training, validation, and test sets.

## 3. PyTorch Dataset and DataLoader

This section bridges the gap between `numpy` arrays and what PyTorch expects.

*   **Create a Custom `Dataset`**:
    *   Define a Python class that inherits from `torch.utils.data.Dataset`.
    *   The `__init__` method will store the sequences of features, the stock ticker indices, and the target labels.
    *   The `__len__` method will return the total number of samples.
    *   The `__getitem__` method will return one sample: a tuple containing the feature sequence, the stock index, and the target price.
*   **Create `DataLoaders`**:
    *   Instantiate the custom `Dataset` for the training, validation, and test sets.
    *   Wrap each dataset in a `torch.utils.data.DataLoader`. This will handle batching, shuffling (for the training set), and parallel data loading.

## 4. GRU Model Architecture

Here, we'll define the neural network using PyTorch's `nn.Module`.

*   **Model Class**: Define a class `StockPredictor(nn.Module)`.
*   **Layers in `__init__`**:
    *   `nn.Embedding`: This layer will take the stock's integer index and output a dense vector (the embedding). We need to decide on the `embedding_dim` (e.g., 10 or 20).
    *   `nn.GRU`: The core recurrent layer. It will process the sequence of scaled price/volume data. We'll need to define `hidden_size` and `num_layers`.
    *   `nn.Linear`: A final fully connected layer to map the GRU's output to a single predicted price.
*   **`forward` Method**:
    *   Define the data flow:
        1.  The stock index is passed through the embedding layer.
        2.  The resulting stock embedding is concatenated with each time step of the input feature sequence.
        3.  This combined sequence is fed into the GRU.
        4.  The final hidden state from the GRU is passed to the linear layer to get the prediction.

## 5. Model Training and Evaluation

This section covers the training loop and assessing the model's performance.

*   **Instantiate Model**: Create an instance of the `StockPredictor` model.
*   **Loss Function and Optimizer**:
    *   **Loss**: Since this is a regression problem (predicting a price), `nn.MSELoss()` (Mean Squared Error) is a suitable choice.
    *   **Optimizer**: `torch.optim.Adam` is a robust and popular choice.
*   **Training Loop**:
    *   Write a loop that iterates for a fixed number of `epochs`.
    *   Inside, have a loop that iterates over the training `DataLoader`.
    *   For each batch: perform the forward pass, calculate the loss, perform backpropagation (`loss.backward()`), and update the weights (`optimizer.step()`).
*   **Validation Loop**:
    *   After each epoch, run a loop over the validation `DataLoader` to calculate the validation loss. This is crucial for monitoring overfitting. Use `with torch.no_grad():` to ensure gradients aren't calculated.
    *   Keep track of the best model based on validation loss and save its state (`torch.save(model.state_dict(), 'best_model.pth')`).
*   **Visualization**: Plot the training and validation loss curves over epochs to visualize the learning process.
*   **Final Evaluation**:
    *   Load the best model weights.
    *   Run the model on the test set.
    *   Calculate the final RMSE (Root Mean Squared Error) to get an interpretable error metric.
    *   Plot the model's predictions against the actual values for a few stocks in the test set.

## 6. Embedding Analysis

Now for the fun part: exploring what the model has learned about the stocks.

*   **Extract Embeddings**: Retrieve the learned weight matrix from the `nn.Embedding` layer. This matrix will have a shape of `(num_stocks, embedding_dim)`.
*   **Gather Sector Information**:
    *   Create a mapping of the stock tickers in your dataset to their respective industry sectors (e.g., 'AAPL' -> 'Information Technology', 'XOM' -> 'Energy'). This can be done with a quick web search or by referencing a public financial data source. [12, 13, 14, 16]
*   **Dimensionality Reduction and Visualization**:
    *   The embeddings are high-dimensional. To visualize them, we'll reduce them to 2D using a technique like **t-SNE** (t-Distributed Stochastic Neighbor Embedding) from `scikit-learn`. [1, 7, 11]
    *   Create a scatter plot of the 2D embeddings.
    *   Color-code each point by its sector and add text labels for the stock tickers. This will visually reveal if the model has learned to group stocks from the same industry together.
*   **Nearest Neighbors Analysis**:
    *   Calculate the cosine similarity between the embedding vector of a chosen stock (e.g., 'JPM') and all other stock vectors.
    *   Find and list the top 5 stocks with the highest similarity. This shows which companies the model considers most "alike" based on their price dynamics.

## 7. Conclusion

*   **Summary**: Briefly summarize the model's predictive performance (e.g., final test RMSE) and the key findings from the embedding analysis (e.g., "The model successfully clustered most technology stocks together...").
*   **Limitations & Next Steps**: Discuss the project's limitations (e.g., only one year of data, simple feature set). Propose future work, such as using more historical data, incorporating sentiment analysis from news, or trying more advanced architectures like LSTMs or Transformers.