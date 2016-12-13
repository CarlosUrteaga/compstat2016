Es más frecuente utilizar sorteos (eventos aleatorios) utilizando integración por Monte Carlo, el cual es una estimación estadística del valor de una integral evluando un conjunto de datos de la distribución. La estimación de integral por medio de Monte Carlo es una simulación que puede ser utilizada con varios parámetros.  Sea una función de esperanza de una variable aleatorio $E(h(X))$. Se denota como $f$ la densidad de $X$ y $\mu$ la esperanza de $h(X)$ con repescto a $f$. Si X es v.a. i.i.d.  se puede aproximar la esperanza de acuerdo a la siguiente función

$$
\hat \mu_{MC} = \frac{1}{n}\sum_{i=1}^{n}h(X_i)\to \int h(X)f(X)dx=\mu
$$

esto se cumple si  se cumple la ley de los grandes números cuando $n\to\infty$. Además, se puede dejar hacer una aproximación a la varianza de la función $h$ dado $\hat\mu_{MC}$

$$
\hat{var}\{\hat \mu_{MC}\} = \frac{1}{n-1}\sum_{i=1}^{n}[ h(X_i)-\hat\mu_{MC}]^2
$$