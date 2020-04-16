#KRR Regression

dataset = pd.read_csv("Aflow_6.csv")
dataset.head()
dataset.describe()

dataset = dataset.drop(["Bravias_lattice"],axis=1)
dataset.head()

from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
X= pd.DataFrame(dataset, columns=['mass_density', 
                                  'ratio_oxygen_by_transition_metal_atom',
                                   'ratio_atoms_cell_by_cell_vol', 'electronic_energy_band_gap',
                                    'energy_atom','point_group', 'c/a_ratio','AGL_bulk_mod'])
y= pd.DataFrame(dataset["AGL_thermal_conductivity"])

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)
X_test.head()

scaler = StandardScaler().fit(X_train)
X_train_scaled = scaler.transform(X_train)
X_test_scaled=scaler.transform(X_test)
print(X_train_scaled)
print(X_test_scaled)

from sklearn.kernel_ridge import KernelRidge
from sklearn.model_selection import GridSearchCV


#Replace with kernel = 'rbf' for rbf and 'linear' for linear kernel
krr = GridSearchCV(KernelRidge(kernel='poly', gamma=0.1),
                  param_grid={"alpha": [1e0, 0.1, 1e-2, 1e-3],
                              "gamma": np.logspace(-2, 2, 5)})

krr.fit(X_train_scaled, y_train)
print("KRR with Polynomial Kernel Model accuracy = ",krr.score(X_test_scaled,y_test))