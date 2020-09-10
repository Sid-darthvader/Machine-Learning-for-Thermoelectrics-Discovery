# Machine-Learning-for-Thermoelectrics-Discovery

Published Version avaialable at:- 
https://www.cambridge.org/core/journals/data-centric-engineering/article/machine-learning-approaches-to-identify-and-design-low-thermal-conductivity-oxides-for-thermoelectric-applications/7086514CABE816961AA8413206FD6977


Transition metal oxides are attractive materials for high temperature thermoelectric applications due to their thermal stability, low cost bulk processing and natural abundance. Notwithstanding the high power factor, their high thermal conductivity is a roadblock in achieving higher efficiency. The search space for new thermoelectric oxides has been limited to the alloys of a few previously explored systems, such as ZnO, SrTiO3 and CaMnO3. The phenomenon of thermal conduction in crystalline alloys and its dependence on crystal properties is also poorly understood, which limits the ability to design new alloys. In this paper, we apply machine-learning models for discovering novel transition metal oxides with low lattice thermal conductivity (kL). A two-step process is proposed to address the problem of small datasets frequently encountered in materials informatics. First, a gradient boosted tree classifier is learnt to categorize unknown compounds into three categories of thermal conductivity: Low, Medium, and High. In the second step, we fit regression models on the targeted class (i.e. low kL) to estimate kL with an R2 value of 0.96. Gradient boosted tree model was also used to identify key material properties influencing classification of kL, namely lattice energy per atom, atom density, electronic energy band gap, mass density, and ratio of oxygen by transition metal atoms. Only fundamental materials properties describing the crystal symmetry, compound chemistry and interatomic bonding were used in the classification process, which can be readily used as selection parameters. The proposed two-step process addresses the problem of small datasets and improves the predictive accuracy.
