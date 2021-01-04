# Integral definida

Algoritmo para el cálculo de integrales definidas. Realiza aproximaciones basadas en el uso de rectángulos, la regla de los trapecios y la regla de Simpson.


Objetivo: construir función que permita obtener la integral definida de una función sobre un intervalo dado.
<p>&nbsp;</p>
**integral_definida()**

* Descripción: aproxima la integral definida de una función sobre [a,b], dado un número de particiones.

* Argumentos:

  - ```funcion```: class=function. Función matemática que queremos integrar.
  
  - ```a```: class=numeric. Límite inferior de integración.
  
  - ```b```: class=numeric. Límite superior de integración.
  
  - ```n```: class=numeric. Número de particiones.
  
  - ```aprox```: class=character. Técnica de integración elegida. La integral definida se puede aproximar mediante rectángulos             (```'rect'```), la regla de los trapecios (```'trap'```) o mediante la regla de Simpson (```'simp'```). Ésta última proporciona las mejores        aproximaciones, pero cuando n es un número par. 
  <p>&nbsp;</p>
      Aproximación por rectángulos: $\displaystyle \int_a^b  f(x)\,dx = \lim_{n\rightarrow \infty}  I_n=\lim_{n\rightarrow \infty}S_n$.
      
      $In=\sum_{i=1}^n f(m_i)\cdot (\frac{b-a}{n})$ ; $Sn=\sum_{i=1}^n f(M_i)\cdot (\frac{b-a}{n})$
      <p>&nbsp;</p>
      Regla de los trapecios:  $\displaystyle \int_a^b f(x)\, dx \approx \frac{b-a}{2n}[f(x_0) + 2f(x_1) + 2f(x_2) + 2f(x_3) + ... + 2f(x_{n-2}) + 2f(x_{n-1}) + f(x_n)]$<p>&nbsp;</p>
      Regla de Simpson:  $\displaystyle \int_a^b f(x)\, dx \approx \frac{b-a}{3n}[f(x_0) + 4f(x_1) + 2f(x_2) + 4f(x_3) + ... + 2f(x_{n-2}) + 4f(x_{n-1}) + f(x_n)]$
  
* Devuelve:
  - Si ```aprox='rect'```: lista con dos elementos class=numeric que representan la suma inferior (In) y la suma superior (Sn)
  
  - Si ```aprox='trap'``` o ```aprox='simp'```: vector de longitud uno que representa la estimación de la integral definida sobre a y      b.  Class=numeric
```{r}

integral_definida <- function(funcion,a,b,n,aprox='rect'){

  interv <- (b-a)/n
  
  seg <- seq(from=a,to=b,by=interv)
  
  if (aprox=='trap'){
    
    strt_end <- c(funcion(seg[1]),funcion(seg[n+1]))
    mid <- 2*funcion(seg[2:n])
    
    return(( (b-a)/(2*n) )*sum(strt_end,mid))
  }
  else if (aprox=='simp'){
    
    strt_end <- c(funcion(seg[1]),funcion(seg[n+1]))
    seg_cut <- seg[2:n]
    
    indices <- 1:length(seg_cut)
    mid <- c(4*funcion(seg_cut[indices%%2 != 0]),
             2*funcion(seg_cut[indices%%2 == 0]))
    
    return(( (b-a)/(3*n) )*sum(strt_end,mid))
  }
  else{
    
    In <- double(1)
    Sn <- double(1)
  
    for (i in 1:n){
      fmi <- min(funcion(seg[i]:seg[i+1]))
      In <- In+fmi*interv
      fMi <- max(funcion(seg[i]:seg[i+1]))
      Sn <- Sn+fMi*interv
    }
    return(list(In=In,Sn=Sn))
  }
}

integral_definida(dnorm,0,1.5,10)
```
