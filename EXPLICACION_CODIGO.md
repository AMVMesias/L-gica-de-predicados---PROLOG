# 📚 Explicación del Sistema Experto de Diagnóstico de Otitis en Prolog

## 🎯 ¿Qué es Prolog?

Prolog es un lenguaje de **programación lógica** donde defines **hechos** y **reglas**, y luego haces **consultas**. Es como tener una base de conocimientos a la que le puedes hacer preguntas.

---

## 📋 Estructura General del Código

### 1️⃣ **DEFINICIÓN DEL GRAFO DE SÍNTOMAS** (Líneas 16-39)

```prolog
% NIVEL 1: Síntomas iniciales (leves)
sintoma_inicial(dolor_oido).
sintoma_inicial(zumbido).
```

**¿Qué hace?**
- Define **hechos** (verdades) sobre los síntomas
- `sintoma_inicial(dolor_oido)` significa: "dolor_oido ES un síntoma inicial"
- Es como etiquetar cada síntoma según su gravedad

**Analogía simple:**
```
Es como organizar archivos en carpetas:
📁 Síntomas Iniciales
   - dolor_oido
   - zumbido
📁 Síntomas Intermedios
   - presion_oido
   - perdida_audicion
```

---

### 2️⃣ **CONEXIONES DEL GRAFO** (Líneas 45-64)

```prolog
% NIVEL 1 -> NIVEL 2
puede_evolucionar(dolor_oido, presion_oido).
puede_evolucionar(dolor_oido, dolor_punzante).
```

**¿Qué hace?**
- Define las **relaciones** entre síntomas
- `puede_evolucionar(dolor_oido, presion_oido)` significa: "el dolor de oído PUEDE evolucionar a presión de oído"

**Analogía simple:**
```
Es como un mapa de carreteras:
Dolor de Oído ──→ Presión de Oído
              └──→ Dolor Punzante

Resfriado ──→ Secreción Nasal ──→ Secreción ──→ OTITIS
```

---

### 3️⃣ **PESOS DE IMPORTANCIA** (Líneas 70-81)

```prolog
peso_sintoma(dolor_oido, 0.3).
peso_sintoma(secrecion, 0.9).
peso_sintoma(otitis, 1.0).
```

**¿Qué hace?**
- Asigna un **número de gravedad** a cada síntoma (de 0.0 a 1.0)
- Mientras más alto el número, más grave el síntoma

**Analogía simple:**
```
Semáforo de gravedad:
🟢 0.2-0.3 = Leve (dolor_oido, zumbido)
🟡 0.4-0.6 = Moderado (presion_oido, secrecion_nasal)
🔴 0.7-1.0 = Grave (dolor_punzante, secrecion, otitis)
```

---

## 🔍 ALGORITMOS DE BÚSQUEDA

### 4️⃣ **BFS - Búsqueda por Amplitud** (Líneas 87-135)

**¿Qué es BFS?**
- BFS = Breadth-First Search (búsqueda a lo ancho)
- Explora **nivel por nivel**, como leer un árbol de arriba hacia abajo

**Ejemplo visual:**
```
Inicio: Resfriado

Nivel 1: [Resfriado]
         ↓
Nivel 2: [Secreción Nasal]
         ↓
Nivel 3: [Secreción]
         ↓
Nivel 4: [OTITIS] ✓ ¡Encontrado!
```

**¿Cómo funciona el código?**

```prolog
bfs_cola([[SintomaInicial, [SintomaInicial]]], Camino, 1)
```
- Usa una **cola** (FIFO = First In, First Out)
- Como una fila en el banco: el primero que entra, es el primero que sale

**Paso a paso:**
1. **Inicio:** Agrega el síntoma inicial a la cola
2. **Saca** el primer elemento de la cola
3. **Explora** sus vecinos (síntomas conectados)
4. **Agrega** los vecinos al FINAL de la cola
5. **Repite** hasta encontrar OTITIS

**Código simplificado:**
```prolog
% Caso base: Si encontramos OTITIS, terminamos
bfs_cola([[otitis, CaminoActual]|_], Camino, Paso) :-
    reverse(CaminoActual, Camino),  % Voltear el camino
    format('Paso ~d: Llego a OTITIS!~n', [Paso]).

% Caso recursivo: Seguir explorando
bfs_cola([[Actual, CaminoActual]|Resto], CaminoFinal, Paso) :-
    % 1. Marcar como visitado
    assertz(visitado(Actual)),
    
    % 2. Encontrar vecinos no visitados
    findall([Vecino, [Vecino|CaminoActual]], 
            (puede_evolucionar(Actual, Vecino), \+ visitado(Vecino)),
            NuevosNodos),
    
    % 3. Agregar vecinos AL FINAL de la cola
    agregar_al_final(Resto, NuevosNodos, NuevaCola),
    
    % 4. Continuar con la nueva cola
    ProximoPaso is Paso + 1,
    bfs_cola(NuevaCola, CaminoFinal, ProximoPaso).
```

---

### 5️⃣ **DFS - Búsqueda en Profundidad** (Líneas 141-196)

**¿Qué es DFS?**
- DFS = Depth-First Search (búsqueda en profundidad)
- Explora **hasta el fondo** de un camino antes de retroceder

**Ejemplo visual:**
```
Inicio: Dolor de Oído

Camino 1: Dolor de Oído → Presión → Oído Tapado → Secreción → OTITIS ✓
(Va directo hasta el fondo)
```

**¿Cómo funciona el código?**

```prolog
dfs_pila([[SintomaInicial, [SintomaInicial]]], Camino, 1)
```
- Usa una **pila** (LIFO = Last In, First Out)
- Como una pila de platos: el último que entra, es el primero que sale

**Diferencia con BFS:**
```
BFS: Agrega vecinos al FINAL
DFS: Agrega vecinos al PRINCIPIO (tope de la pila)
```

**Código simplificado:**
```prolog
dfs_pila([[Actual, CaminoActual]|Resto], CaminoFinal, Paso) :-
    assertz(visitado(Actual)),
    
    % Encontrar vecinos
    findall([Vecino, [Vecino|CaminoActual]], 
            (puede_evolucionar(Actual, Vecino), \+ visitado(Vecino)),
            NuevosNodos),
    
    % Agregar vecinos AL PRINCIPIO de la pila (LIFO)
    append(NuevosNodos, Resto, NuevaPila),
    
    % Continuar
    ProximoPaso is Paso + 1,
    dfs_pila(NuevaPila, CaminoFinal, ProximoPaso).
```

---

## 🎮 MODOS DE USO

### 6️⃣ **MODO AUTOMÁTICO: iniciar** (Líneas 387-413)

**¿Qué hace?**
1. Muestra una lista de síntomas numerados
2. Tú eliges UN número (1-9)
3. Eliges el algoritmo (BFS o DFS)
4. El sistema recorre automáticamente hasta OTITIS

**Flujo:**
```
Usuario ingresa: 5 (resfriado)
Usuario ingresa: 1 (BFS)
        ↓
Sistema ejecuta: diagnosticar_otitis(resfriado, bfs)
        ↓
BFS recorre: Resfriado → Secreción Nasal → Secreción → OTITIS
        ↓
Muestra: Camino completo + Probabilidad
```

**Código clave:**
```prolog
iniciar :-
    % Mostrar lista de síntomas
    write('1. dolor_oido'), nl,
    write('2. zumbido'), nl,
    % ...
    
    % Leer número del usuario
    read(Numero),
    
    % Convertir número a síntoma
    numero_a_sintoma(Numero, Sintoma),
    
    % Ejecutar algoritmo
    diagnosticar_otitis(Sintoma, Algoritmo).
```

---

### 7️⃣ **MODO INTERACTIVO: interactivo** (Líneas 419-477)

**¿Qué hace?**
1. Eliges un síntoma inicial
2. El sistema **pregunta SI o NO** por cada síntoma siguiente
3. Solo pregunta por síntomas **conectados** según el grafo
4. Si respondes "si", continúa explorando desde ahí

**Flujo:**
```
Usuario elige: 5 (resfriado)
        ↓
Sistema: ¿Tiene secreción nasal? 
Usuario: si.
        ↓
Sistema: ¿Tiene secreción en el oído?
Usuario: si.
        ↓
Sistema detecta: OTITIS CONFIRMADA
```

**Código clave:**
```prolog
interactivo :-
    % Usuario elige síntoma inicial
    read(Numero),
    numero_a_sintoma(Numero, SintomaInicial),
    
    % Marcar como presente
    assertz(sintoma_presente(SintomaInicial)),
    
    % Explorar preguntando
    explorar_interactivo([SintomaInicial]),
    
    % Evaluar diagnóstico
    evaluar_diagnostico_interactivo.
```

**Exploración interactiva:**
```prolog
explorar_interactivo([SintomaActual|Resto]) :-
    % 1. Encontrar síntomas conectados
    findall(Siguiente, puede_evolucionar(SintomaActual, Siguiente), Posibles),
    
    % 2. Preguntar por cada uno
    preguntar_sintomas_interactivos(Posibles, NuevosSintomas),
    
    % 3. Continuar con los que respondió "si"
    append(Resto, NuevosSintomas, TodosSintomas),
    explorar_interactivo(TodosSintomas).
```

---

## 🧮 CÁLCULO DE PROBABILIDAD (Líneas 225-251)

**¿Cómo calcula la probabilidad?**

```prolog
calcular_probabilidad_camino(Camino, Probabilidad) :-
    % 1. Obtener el peso de cada síntoma en el camino
    findall(Peso, (member(Sintoma, Camino), peso_sintoma(Sintoma, Peso)), Pesos),
    
    % 2. Sumar todos los pesos
    sum_list(Pesos, Total),
    
    % 3. Contar cuántos síntomas hay
    length(Pesos, N),
    
    % 4. Calcular el promedio
    Probabilidad is Total / N.
```

**Ejemplo:**
```
Camino: [resfriado, secrecion_nasal, secrecion, otitis]
Pesos:  [0.25,      0.6,              0.9,       1.0]

Total = 0.25 + 0.6 + 0.9 + 1.0 = 2.75
N = 4 síntomas

Probabilidad = 2.75 / 4 = 0.6875 = 68.75%
```

---

## 🛠️ UTILIDADES

### 8️⃣ **Formatear nombres** (Líneas 297-310)

```prolog
formatear_sintoma(dolor_oido) → "Dolor Oido"
formatear_sintoma(secrecion_nasal) → "Secrecion Nasal"
```

**¿Qué hace?**
- Convierte `dolor_oido` (nombre técnico) a "Dolor Oido" (texto legible)
- Reemplaza guiones bajos `_` por espacios
- Capitaliza la primera letra

---

### 9️⃣ **Agregar al final de lista** (Líneas 287-294)

```prolog
agregar_al_final([1,2], [3,4], [1,2,3,4])
```

**¿Para qué sirve?**
- BFS necesita agregar elementos AL FINAL de la cola
- Es la diferencia principal entre BFS (cola) y DFS (pila)

---

## 📊 COMPARACIÓN BFS vs DFS

### Visual:

**BFS (Por Amplitud):**
```
Nivel 1: [A]
         ↓
Nivel 2: [B, C, D]  ← Explora todos del nivel 2
         ↓
Nivel 3: [E, F, G, H]  ← Luego explora todos del nivel 3
```

**DFS (En Profundidad):**
```
A → B → E → I → J  ← Va hasta el fondo
    ↑
    Luego retrocede y prueba otro camino
    B → F → K
```

---

## 🎯 CONCEPTOS CLAVE DE PROLOG

### 1. **Hechos**
```prolog
sintoma_inicial(dolor_oido).  % "dolor_oido ES un síntoma inicial"
```

### 2. **Reglas**
```prolog
puede_evolucionar(A, B).  % "A PUEDE evolucionar a B"
```

### 3. **Consultas**
```prolog
?- sintoma_inicial(X).  % "¿Cuáles son los síntomas iniciales?"
X = dolor_oido ;
X = zumbido.
```

### 4. **Variables dinámicas**
```prolog
:- dynamic(visitado/1).  % Permite cambiar durante ejecución
assertz(visitado(dolor_oido)).  % Agregar hecho
retractall(visitado(_)).  % Borrar todos los hechos
```

### 5. **Listas**
```prolog
[1, 2, 3]  % Lista de elementos
[H|T]      % H = cabeza (primer elemento), T = cola (resto)
```

### 6. **Operadores especiales**
```prolog
\+          % Negación (NO)
:-          % Implicación (SI... ENTONCES...)
is          % Evaluación aritmética
member(X, L) % X es miembro de la lista L
```

---

## 🚀 FLUJO COMPLETO DE EJECUCIÓN

### Ejemplo: `iniciar` con síntoma "resfriado" y algoritmo "BFS"

```
1. Usuario ejecuta: ?- iniciar.

2. Sistema muestra menú de síntomas

3. Usuario ingresa: 5. (resfriado)

4. Sistema convierte: numero_a_sintoma(5, resfriado)

5. Usuario elige algoritmo: 1. (BFS)

6. Sistema convierte: numero_a_algoritmo(1, bfs)

7. Sistema ejecuta: diagnosticar_otitis(resfriado, bfs)

8. diagnosticar_otitis llama: bfs_otitis(resfriado)

9. BFS inicia con cola: [[resfriado, [resfriado]]]

10. PASO 1:
    - Saca: resfriado
    - Marca: visitado(resfriado)
    - Encuentra vecinos: secrecion_nasal
    - Cola queda: [[secrecion_nasal, [secrecion_nasal, resfriado]]]

11. PASO 2:
    - Saca: secrecion_nasal
    - Marca: visitado(secrecion_nasal)
    - Encuentra vecinos: secrecion
    - Cola queda: [[secrecion, [secrecion, secrecion_nasal, resfriado]]]

12. PASO 3:
    - Saca: secrecion
    - Marca: visitado(secrecion)
    - Encuentra vecinos: otitis
    - Cola queda: [[otitis, [otitis, secrecion, secrecion_nasal, resfriado]]]

13. PASO 4:
    - Encuentra: OTITIS
    - Camino: [resfriado, secrecion_nasal, secrecion, otitis]

14. Calcula probabilidad: (0.25 + 0.6 + 0.9 + 1.0) / 4 = 68.75%

15. Muestra resultado:
    ========================================
    CAMINO ENCONTRADO A OTITIS
    ========================================
    -> Resfriado
    -> Secrecion Nasal
    -> Secrecion
    -> Otitis
    
    Probabilidad: 68.75%
    DIAGNOSTICO: OTITIS confirmada
    RECOMENDACION: Consultar médico URGENTE
```

---

## 📝 RESUMEN PARA PRINCIPIANTES

| Concepto | Explicación Simple |
|----------|-------------------|
| **Hechos** | Verdades que defines (como etiquetas) |
| **Reglas** | Relaciones entre hechos (como flechas) |
| **BFS** | Explora nivel por nivel (a lo ancho) |
| **DFS** | Explora camino completo (en profundidad) |
| **Cola (FIFO)** | Primero que entra, primero que sale |
| **Pila (LIFO)** | Último que entra, primero que sale |
| **assertz** | Agregar un hecho temporalmente |
| **retractall** | Borrar hechos temporales |
| **findall** | Buscar todos los que cumplan condición |

---

## 💡 ¿Cómo probarlo?

1. **Abrir terminal de Prolog:**
   ```bash
   swipl -s agente_otitis.pl
   ```

2. **Modo Automático:**
   ```prolog
   ?- iniciar.
   ```

3. **Modo Interactivo:**
   ```prolog
   ?- interactivo.
   ```

4. **Casos de prueba:**
   ```prolog
   ?- caso_resfriado_bfs.
   ?- caso_dolor_oido_dfs.
   ?- comparar_algoritmos(resfriado).
   ```

---

## 🎓 Conceptos Importantes

### ¿Por qué usar BFS vs DFS?

- **BFS:** Encuentra el camino MÁS CORTO (menos pasos)
- **DFS:** Puede ser más rápido si el objetivo está profundo

### ¿Qué es "visitado"?

- Marca los nodos ya explorados para NO volver a visitarlos
- Evita ciclos infinitos (A→B→A→B...)

### ¿Qué es la "cola" vs "pila"?

**Cola (BFS):**
```
[1] → [2] → [3]
↑           ↓
Entra       Sale
```

**Pila (DFS):**
```
[3]
[2]
[1]
↑ ↓
Entra y Sale
```

---

¡Espero que esta explicación te ayude a entender el código! 🎉
