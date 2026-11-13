"""
build_indices.py : Extension 2, step 1. Build the pre-1994 indices (baseline).

Targets (see config "targets"):
    barriers : girls' high school share in 1990   (main)   trained on towns with 1990 data
    poor     : literacy                           (main)   trained on all towns
    mu0      : girls' high school share in 2000   (robustness: classic predicted
               untreated outcome)                          trained on secular-won towns only

"""
import json
from pathlib import Path




import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.linear_model import Lasso, LinearRegression, Ridge
from sklearn.metrics import r2_score, root_mean_squared_error
from sklearn.model_selection import GridSearchCV, GroupKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import PolynomialFeatures, StandardScaler
 
ROOT = Path(__file__).resolve().parents[2]   # repo root
TARGETS = ["barriers", "poor", "mu0"]        # barriers, poor = main; mu0 = robustness



# 1. Load data --------------------------------------------------------------------

cfg = json.loads((ROOT / "config" / "extension2.json").read_text())
df = pd.read_csv(ROOT / cfg["paths"]["input"])
assert len(df) == 2629 and df[cfg["id"]].is_unique


# Flag missing
df["miss_c90"] = df["c90ageshr19"].isna().astype(int)


 
groups = df[cfg["cluster"]].to_numpy()       # province codes (pcode)
cv = GroupKFold(n_splits=cfg["inner_folds"])  # folds keep provinces together


BINARY = ["merkezi", "merkezp", "subbuyuk", "buyuk", "miss_c90"]


def predictors_for(target):
    """Predictors for each target."""
    excluded = []
    for group in cfg["targets"][target]["exclude_groups"]:
        excluded += cfg["feature_groups"][group]
    features = [f for f in cfg["features_strict"] if f not in excluded]
    cont = [f for f in features if f not in BINARY]
    binary = [f for f in features if f in BINARY] + ["miss_c90"]
    return cont, binary


def training_mask(target):
    """Towns allowed to TRAIN the model:
      -target observed and,
      - for mu0: a secular mayor (train_on = "controls"). 

    All towns are predicted."""
    spec = cfg["targets"][target]
    mask = df[spec["var"]].notna().to_numpy()
    if spec["train_on"] == "controls":
        mask = mask & (df[cfg["treatment"]] == 0).to_numpy()
    return mask
 
        
# 2. Preprocessing -----------------------------------------------------

def make_preprocessor(cont_cols, binary_cols, degree=1):
    """
    Imputation & Scaling 
    """
    continuous = Pipeline([
        ("impute", SimpleImputer(strategy="median")), 
        ("scale_in", StandardScaler()),
        ("poly", PolynomialFeatures(degree=degree, include_bias=False)),
        ("scale_out", StandardScaler()),
    ])
    return ColumnTransformer([
        ("num", continuous, cont_cols),
        ("bin", "passthrough", binary_cols),
    ])



# 3. Candidate models ----------------------------------------------------------


DEGREES = [1, 2]

CANDIDATES = {
    "OLS":   (LinearRegression(), {}),
    "Ridge": (Ridge(), {"model__alpha": np.logspace(-2, 4, 25)}),
    "Lasso":  (Lasso(max_iter=50000), {"model__alpha": np.logspace(-6, -1, 21)}),
}


def make_search(name, cont_cols, binary_cols):
    """GridSearchCV over polynomial degree (and alpha for Ridge/Lasso)."""
    estimator, alpha_grid = CANDIDATES[name]

    pipe = Pipeline([("prep", make_preprocessor(cont_cols, binary_cols)),
                     ("model", estimator)])

    grid = {"prep__num__poly__degree": DEGREES, **alpha_grid}

    return GridSearchCV(pipe, grid, cv=cv, scoring="neg_root_mean_squared_error",
                        return_train_score=True)


def cv_results_table(search):
    """ CV results"""

    res = pd.DataFrame(search.cv_results_)

    params = pd.DataFrame(list(res["params"]))

    params.columns = [c.split("__")[-1] for c in params.columns]

    table = params.assign(cv_rmse=-res["mean_test_score"],
                          cv_rmse_std=res["std_test_score"],
                          train_rmse=-res["mean_train_score"])

    
    return table.sort_values("cv_rmse").reset_index(drop=True)


def check_search_range(search, values):
    """Find best alpha"""

    best = search.best_params_.get("model__alpha")

    if best is None:
        return "no alpha"
    if best == values.min():
        return "lower edge"
    if best == values.max():
        return "upper edge"
    return "interior"


# 4. Choose Model by CV RMSE -------------------------------------------------------------

# Pick candidate model with lowest CV RMSE
def select_model(target):

    cont, binary = predictors_for(target)
    X = df[cont + binary]
    y = df[cfg["targets"][target]["var"]]
    obs = training_mask(target)


    rows, tables = [], []

    for name in CANDIDATES:
        search = make_search(name, cont, binary)
        search.fit(X[obs], y[obs], groups=groups[obs])
        alpha_grid = CANDIDATES[name][1].get("model__alpha", np.array([np.nan]))
        rows.append({
            "target": target, "model": name,
            "degree": search.best_params_["prep__num__poly__degree"],
            "alpha": search.best_params_.get("model__alpha", np.nan),
            "alpha_position": check_search_range(search, alpha_grid),
            "cv_rmse": -search.best_score_,
        })
        tables.append(cv_results_table(search).assign(target=target, model=name))
 
    selection = pd.DataFrame(rows).sort_values("cv_rmse").reset_index(drop=True)
    return selection, pd.concat(tables, ignore_index=True)




# 5. Out of fold Predictions -----------------------------------------------------

def out_of_fold_predict(target, model_name):
    """
    Every town is predicted by a model trained on other provinces only
    """

    cont, binary = predictors_for(target)


    X = df[cont + binary]
    y = df[cfg["targets"][target]["var"]].to_numpy()

    obs = training_mask(target)

    pred = np.full(len(df), np.nan)
    outer = GroupKFold(n_splits=cfg["n_folds"])

    for train_idx, test_idx in outer.split(X, groups=groups):
        fit_idx = train_idx[obs[train_idx]]              
        assert not set(fit_idx) & set(test_idx), "leakage"
        search = make_search(model_name, cont, binary)
        search.fit(X.iloc[fit_idx], y[fit_idx], groups=groups[fit_idx])
        pred[test_idx] = search.predict(X.iloc[test_idx])  
 
    fit = {"oof_rmse": root_mean_squared_error(y[obs], pred[obs]),
           "oof_r2": r2_score(y[obs], pred[obs]),
           "sd_target": float(np.std(y[obs])),
           "n_train": int(obs.sum())}
    return pred, fit

# 6. Run and save -----------------------------------------------------------------------
 
indices = pd.DataFrame({cfg["id"]: df[cfg["id"]]})
diagnostics, all_tables = [], []


for target in TARGETS:
    selection, tables = select_model(target)
    all_tables.append(tables)
    chosen = selection.loc[0, "model"]
    print(f"\n{target}: CV RMSE by candidate")
    print(selection.round(5).to_string(index=False))
 
    pred, fit = out_of_fold_predict(target, chosen)
    indices[target] = pred
    diagnostics.append({"target": target, "model": chosen,
                        "degree": int(selection.loc[0, "degree"]),
                        "alpha": selection.loc[0, "alpha"], **fit})
    print(f"-> {chosen}: out-of-fold R2 = {fit['oof_r2']:.3f}, RMSE = {fit['oof_rmse']:.4f} "
          f"(SD of target {fit['sd_target']:.4f}, {fit['n_train']} towns)")
 
assert indices.notna().all().all(), "missing predictions"
 
indices.to_csv(ROOT / cfg["paths"]["indices"], index=False)
pd.DataFrame(diagnostics).to_csv(ROOT / cfg["paths"]["diagnostics"], index=False)
pd.concat(all_tables).to_csv(ROOT / "data" / "derived" / "e2_model_selection.csv", index=False)
print("\nSaved indices, diagnostics and model-selection tables.")
