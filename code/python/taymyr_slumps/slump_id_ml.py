import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report
from sklearn.preprocessing import MinMaxScaler
from pathlib import Path

def find_repo_root(marker='.git'):
    """Recursively search for the repository root by locating a marker (like .git or README.md)."""
    current = Path(__file__).resolve().parent
    while current != current.parent:
        if (current / marker).exists():
            return current
        current = current.parent
    raise FileNotFoundError(f"Marker '{marker}' not found in any parent directory.")

# Use the repo root
wd_root = find_repo_root()
wd_imports = f'{wd_root}/imports/'

# Read training data
df = pd.read_csv(f'{wd_imports}taymyr_slump_train_2024_20250605.csv')

# Features and label
features = ['B2', 'B3', 'B4', 'B8', 'B8A', 'B11', 'B12', 'ndvi', 'ndwi']
X = df[features]
y = df['slump']

# Split into train+val and test
X_trainval, X_test, y_trainval, y_test = train_test_split(X, y, test_size=0.2, stratify=y, random_state=42)

# Split train and val
X_train, X_val, y_train, y_val = train_test_split(X_trainval, y_trainval, test_size=0.25, stratify=y_trainval, random_state=42)

# Min-max scaling
scaler = MinMaxScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_val_scaled = scaler.transform(X_val)
X_test_scaled = scaler.transform(X_test)

# Save min/max for GEE deployment
scaling_params = pd.DataFrame({'feature': features,
                               'min': scaler.data_min_,
                               'max': scaler.data_max_})
scaling_params.to_csv(f'{wd_imports}slump_2024_scaling_parameters.csv', index=False)

# Train the model
model = RandomForestClassifier(n_estimators=100, random_state=42)
model.fit(X_train_scaled, y_train)

# Evaluate
print("Validation performance:")
print(classification_report(y_val, model.predict(X_val_scaled)))

print("Test performance:")
print(classification_report(y_test, model.predict(X_test_scaled)))