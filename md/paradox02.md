# Teoría
El problema consiste en encontrar cual es la probabilidad de que en un salón se tengan dos personas que cumplan el mismo día [1], para eso se calcula primero al probabilidad de que el los cumpleaños sean diferentes. La probabilidad de que ninguan persona cumpla el mismo día es 

$$
p={\frac{365}{365}}\cdot {\frac  {364}{365}}\cdot {\frac  {363}{365}}\cdots {\frac  {365-n+1}{365}}
$$

Entonces, la probabilidad del evento, está dado de acuerdo a la siguiente figura.

![equation](https://wikimedia.org/api/rest_v1/media/math/render/svg/5e0a6a40789554ad7dce66f8ce47c3e7a06e92bb)

Ahora, la probabilidad de que al menos dos personas tengan el mismo día de cumpleaños.

![equation](https://wikimedia.org/api/rest_v1/media/math/render/svg/86b6c0a250b4bb389e96ae3b739dfbfad3a2af95)

En contraste, la probabilidad que cualquiera en una habitación de n personas (excluido Ud.) tengan el mismo día de cumpleaños que usted está dada por

$$
1-\left({\frac  {364}{365}}\right)^{n}
$$

[1] Flajolet, Philippe, Daniele Gardy, and Loÿs Thimonier. "Birthday paradox, coupon collectors, caching algorithms and self-organizing search." Discrete Applied Mathematics 39.3 (1992): 207-229.