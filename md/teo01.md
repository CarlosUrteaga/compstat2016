#### Chi

La prueba chi cuadrada permite determinar cuando dos distribuciones no son iguales, para esto se hace una hipótesis y se acepta o se rechada dependiendo de la diferencia de los datos.  La siguiente formula expresa la función de chi cuadrada


$${\displaystyle \chi^{2}=\sum_{i=1}^{n}{\frac {(O_{i}-E_{i})^{2}}{E_{i}}}=N\sum _{i=1}^{n}{\frac {\left(O_{i}/N-p_{i}\right)^{2}}{p_{i}}}}$$

$O_{i}$ = el número de observaciones de $i$

$N$ = Total de observaciones

$E_{i}=N \times p_{i}$ Es la frecuencia esperada o teórica de $i$, el cual permite afirmar la hipóstesis nula que es una fracción del tipo $i$ en una muestra $p_{i}$

#### Kolmogorv
La prueba de Kolmogorov-Smirnov $(K-S)$ se define para comparar una funciónde distribuciónacumulada observada de una función teórica determinada. Esta se calcula a partir de la diferncia mayor entre la teorica y la observada. Esta prueba de bondad de ajuste contrasta si las observaciones podrían razonablemente proceder de la distribución especificada. 

$$ 
D_{n}=\max _{x}|O_{n}(x)-E_n(x)|
$$


#### LCG

La definición informal de una secuencia aleatoria es dado cada nuevo número generado no debe de tener una relación inmediatamente obvia con los números generados antes de él. Existen algoritmos que dan la apariencia de una secuencia aleatoria. E


Una secuencia congruencial lineal es una serie de números basada en la fórmula de relación de recurrencia:
$$
Xn = (aX_{n-1} + c) mod m
$$

En esta fórmula, m se denomina módulo, a es un factor, y ces el incremento. No es difícil imaginar la secuencia resultante de esta fórmula cuando se le dan valores diferentes. Por ejemplo, con a = 2, c = 3, m = 10, y un valor inicial de X = 5, se produciría la siguiente secuencia:

	5 3 9 1 5 3 9 1 5 3 ...

Observe que la secuencia se repite después de que se hayan generado cuatro números. Esto se debe a la aritmética modular que obliga a envolver los valores en el rango deseado. En la teoría de números aleatorios, la duración de este ciclo se denomina período de la secuencia. Es decir, un período más largo permite secuencias que aparecen más aleatorias porque no hay patrón inmediatamente discernible. El período más largo posible para un algoritmo congruencial lineal es el valor de m mismo. Como tal, cuanto mayor es el m, mejores son las posibilidades de tener un período largo.

Observe también que a pesar de un módulo de 10, el período del algoritmo dado fue sólo 4. Esto sugiere que tanto el incremento como el multiplicador deben elegirse con cuidado. Es posible obtener una secuencia que cubra toda la gama de m tal que la secuencia resultante sea una permutación válida de los valores en el intervalo. Esto se llama un generador lineal congruencial de ciclo completo. Hay algunas reglas (descritas por Knuth), que cuando se cumplen, garantizan un ciclo completo. Sin embargo, estas reglas sólo se aplican a la fórmula cuando c no es cero. Cuando c no es cero, el algoritmo se llama típicamente un generador congruencial mixto.

Desafortunadamente, un largo período no garantiza una secuencia aleatoria. La mayoría de las combinaciones de a, c y m tendrán suficiente aleatoriedad pero un período inaceptablemente corto, o un período largo con una secuencia obviamente no aleatoria. De las formas de evitar este problema, lo más común es ignorar el generador congruencial mixto y en su lugar utilizar la fórmula para un generador multiplicativo puro. La fórmula es la misma, excepto la eliminación de la variable c:

$$
Xn = aX_{n-1} mod m
$$
La diferencia entre un generador congruencial mixto y un generador multiplicativo puro es que el primero usa c, donde c no es igual a cero, y este último usa c, donde c es siempre igual a cero. Debido a que c es ahora una constante sin ningún efecto, puede ser eliminado completamente de la fórmula. El resultado es un algoritmo más simple, por lo que muchos algoritmos de números aleatorios utilizan un generador multiplicativo puro.


Referencias

Linear Congruential Generators, "http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_rand.aspx"

Linear congruential generator, "https://rosettacode.org/wiki/Linear_congruential_generator#C.2B.2B"