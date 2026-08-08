# EVALUACION 3.1 - anexo en R
# problemas 2, 3 y 5

# ---------------- problema 2: criterios bajo incertidumbre ----------------

pagos2 <- matrix(c(12, 18, 24, 28,
                   5, 22, 35, 48,
                   -8, 20, 50, 70,
                   -25, 10, 55, 95),
                 nrow = 4, byrow = TRUE)
rownames(pagos2) <- c("Mercado local", "Expansion regional",
                      "Plataforma nacional", "Expansion internacional")
colnames(pagos2) <- c("Contraccion", "Crec. debil", "Crec. moderado", "Crec. fuerte")

pagos2

# maximin: peor resultado de cada alternativa y me quedo con el mejor
minimos <- apply(pagos2, 1, min)
minimos
names(which.max(minimos))

# maximax: mejor resultado de cada alternativa y me quedo con el mayor
maximos <- apply(pagos2, 1, max)
maximos
names(which.max(maximos))

# matriz de arrepentimientos: mejor pago de cada columna menos cada pago
mejor_col <- apply(pagos2, 2, max)
arrep <- sweep(pagos2, 2, mejor_col, FUN = function(x, m) m - x)
arrep

# minimax de arrepentimiento
arrep_max <- apply(arrep, 1, max)
arrep_max
names(which.min(arrep_max))

# ---------------- problema 3: VEM bajo riesgo ----------------

pagos3 <- matrix(c(36, 42, 48, 50,
                   20, 50, 70, 82,
                   -15, 30, 85, 120),
                 nrow = 3, byrow = TRUE)
rownames(pagos3) <- c("Plan pequeno", "Plan mediano", "Plan grande")
colnames(pagos3) <- c("Baja", "Media", "Alta", "Excepcional")
prob3 <- c(0.2, 0.35, 0.3, 0.15)

# verificar que las probabilidades son validas
sum(prob3)
all(prob3 >= 0 & prob3 <= 1)

# VEM de cada plan
vem3 <- pagos3 %*% prob3
vem3
rownames(pagos3)[which.max(vem3)]

# ---------------- problema 5: VEM, informacion perfecta y VEIP ----------------

pagos5 <- matrix(c(24, 24, 24, 24,
                   12, 36, 36, 36,
                   -4, 24, 48, 48,
                   -20, 12, 36, 60),
                 nrow = 4, byrow = TRUE)
rownames(pagos5) <- c("Pedir 100", "Pedir 150", "Pedir 200", "Pedir 250")
colnames(pagos5) <- c("D100", "D150", "D200", "D250")
prob5 <- c(0.15, 0.35, 0.30, 0.20)

# VEM de cada pedido (decision sin informacion)
vem5 <- pagos5 %*% prob5
vem5
rownames(pagos5)[which.max(vem5)]

# valor esperado con informacion perfecta: mejor pago de cada nivel de demanda
mejor_pago <- apply(pagos5, 2, max)
mejor_pago
ve_ip <- sum(mejor_pago * prob5)
ve_ip

# VEIP y decision sobre el informe (cuesta 7.5)
veip <- ve_ip - max(vem5)
veip
veip > 7.5          # conviene comprar el informe?
ve_ip - 7.5         # valor esperado neto despues de pagarlo
