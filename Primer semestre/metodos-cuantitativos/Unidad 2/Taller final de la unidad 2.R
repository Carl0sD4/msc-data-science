# Datos en matriz
datos <- matrix(c(
  241, 28, 24, 86,  90,
  235, 35, 23, 82,  88,
  280, 48, 26, 94, 100,
  278, 65, 25, 92,  98,
  310, 77, 26, 96, 106,
  321, 23, 27, 98, 110,
  312, 45, 26, 97, 108,
  286, 53, 26, 93, 102,
  275, 58, 25, 91,  96,
  288, 54, 26, 94, 104,
  270, 37, 25, 90,  94,
  260, 26, 25, 89,  92
), nrow = 12, byrow = TRUE)

# Nombres de columnas
colnames(datos) <- c("y", "X1", "X2", "X3", "X4")

# a) Modelo matricial: y y X
y <- datos[, "y"]
X <- cbind(1, datos[, c("X1", "X2", "X3", "X4")])
colnames(X)[1] <- "Intercepto"

# b) B = (X'X)^(-1) X'y
XtX     <- t(X) %*% X
XtX_inv <- solve(XtX)
Xty     <- t(X) %*% y
B       <- XtX_inv %*% Xty
round(B, 4)

# c) Ecuación de regresión múltiple usando lm()
df     <- as.data.frame(datos)
modelo <- lm(y ~ X1 + X2 + X3 + X4, data = df)
b      <- coef(modelo)

cat("Ecuación de regresión múltiple:\n")
cat("y_hat =",
    round(b[1], 4), "+",
    round(b["X1"], 4), "* X1 +",
    round(b["X2"], 4), "* X2 +",
    round(b["X3"], 4), "* X3 +",
    round(b["X4"], 4), "* X4\n")

# d) Predicción para X1=41, X2=25, X3=95, X4=120
nuevo <- data.frame(
  X1 = 41,
  X2 = 25,
  X3 = 95,
  X4 = 120
)

prediccion <- predict(modelo, newdata = nuevo)
prediccion
