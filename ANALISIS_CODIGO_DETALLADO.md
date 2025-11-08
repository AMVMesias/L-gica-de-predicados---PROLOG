# Análisis Detallado de Bloques de Código - Sistema Experto Otitis

## 📋 Tabla de Contenidos

1. [Estructura General del Archivo](#estructura-general-del-archivo)
2. [Bloque 1: Declaraciones Dinámicas](#bloque-1-declaraciones-dinámicas)
3. [Bloque 2: Definición del Grafo](#bloque-2-definición-del-grafo)
4. [Bloque 3: Algoritmo BFS](#bloque-3-algoritmo-bfs)
5. [Bloque 4: Algoritmo DFS](#bloque-4-algoritmo-dfs)
6. [Bloque 5: Evaluación de Probabilidad](#bloque-5-evaluación-de-probabilidad)
7. [Bloque 6: Utilidades de Formateo](#bloque-6-utilidades-de-formateo)
8. [Bloque 7: Modo Interactivo](#bloque-7-modo-interactivo)
9. [Flujos de Ejecución](#flujos-de-ejecución-completos)

---

## 📐 Estructura General del Archivo

El archivo `agente_otitis.pl` está organizado en **8 secciones principales**:

```
1. DECLARACIONES DINAMICAS ─┐
2. DEFINICION DEL GRAFO     ├─── Datos
3. PESOS DE SINTOMAS        ─┤

4. ALGORITMO BFS            ├─── Algoritmos
5. ALGORITMO DFS            ─┤

6. DIAGNOSTICO PRINCIPAL    ─┬
7. EVALUACION PROBABILIDAD  ├─── Lógica de inferencia
                            ─┤

8. UTILIDADES               ┬
9. CASOS DE PRUEBA          ├─── Funciones auxiliares
10. MODO AUTOMATICO         ├─── Casos de uso
11. MODO INTERACTIVO        ├─── Interfaces
12. AYUDA                   ─┘
```

---

## 🔧 Bloque 1: Declaraciones Dinámicas

### Código

```prolog
:- dynamic(sintoma_presente/1).
:- dynamic(visitado/1).
```

### ¿Qué hace?

Declara predicados **dinámicos** que pueden **modificarse durante la ejecución**.

### ¿Cómo lo hace?

#### Concepto: Predicados Dinámicos

En Prolog, los hechos normales son **inmutables**:

```prolog
% Hechos FIJOS (no pueden cambiar)
sintoma_inicial(dolor_oido).
sintoma_inicial(zumbido).
```

Los predicados **dinámicos** permiten agregar o eliminar hechos:

```prolog
:- dynamic(sintoma_presente/1).  % Declara que puede cambiar

% En ejecución:
?- assertz(sintoma_presente(dolor_oido)).  % AGREGAR un hecho
% Ahora: sintoma_presente(dolor_oido) = true

?- retractall(sintoma_presente(_)).  % ELIMINAR todos los hechos
% Ahora: sintoma_presente(X) = false (para cualquier X)
```

#### Propósito en nuestro código

| Predicado | Propósito | Cuándo se usa |
|-----------|-----------|---------------|
| `sintoma_presente/1` | Marca síntomas presentes en el paciente | Modo interactivo: cuando el usuario responde "sí" |
| `visitado/1` | Marca síntomas ya explorados | BFS/DFS: para evitar visitar dos veces el mismo nodo |

#### Analogía

```prolog
% Sin dynamic (no funciona):
sintoma_presente(dolor_oido).  % Fijo al cargar
% No se puede agregar más durante ejecución

% Con dynamic (funciona):
:- dynamic(sintoma_presente/1).
?- assertz(sintoma_presente(zumbido)).  % ✓ Se agrega dinámicamente
?- retractall(sintoma_presente(_)).     % ✓ Se limpia
```

---

## 📊 Bloque 2: Definición del Grafo

### Código (Sintomas por niveles)

```prolog
% NIVEL 1: Sintomas iniciales (leves)
sintoma_inicial(dolor_oido).
sintoma_inicial(zumbido).

% NIVEL 2: Sintomas intermedios
sintoma_intermedio(presion_oido).
sintoma_intermedio(perdida_audicion).

% NIVEL 2.5: Resfriado
sintoma_intermedio_avanzado(resfriado).

% NIVEL 3: Sintomas graves
sintoma_grave(oido_tapado).
sintoma_grave(dolor_punzante).
sintoma_grave(secrecion_nasal).

% NIVEL 4: Sintoma critico
sintoma_critico(secrecion).

% Diagnostico final
diagnostico_final(otitis).
```

### ¿Qué hace?

Define **nodos del grafo** clasificados por **niveles de gravedad**.

### ¿Cómo lo hace?

#### Estructura de niveles

```
NIVEL 1              NIVEL 2              NIVEL 3              NIVEL 4         DIAG
(leves)              (intermedios)        (graves)             (crítico)       (final)
─────────────────────────────────────────────────────────────────────────────────────
dolor_oido    →      presion_oido   →     oido_tapado    →     secrecion  →   otitis
                                                           →
zumbido       →      perdida_audicion →   dolor_punzante →
                     presion_oido    →    
                     
resfriado     →      secrecion_nasal →    secrecion      →
```

#### Cuándo se usa cada predicado

```prolog
% En INICIAR: mostrar opciones disponibles
(sintoma_inicial(X), formatear_sintoma(X))
% Muestra: dolor_oido, zumbido

% En INTERACTIVO: decidir si el nodo es el final
(diagnostico_final(otitis), 
 write('¡Diagnostico encontrado!'))

% En ORDENAR SINTOMAS: clasificarlos por gravedad
(sintoma_grave(X) -> agregar_lista_graves(X) ; true)
```

### Código (Aristas del grafo)

```prolog
% NIVEL 1 -> NIVEL 2
puede_evolucionar(dolor_oido, presion_oido).
puede_evolucionar(dolor_oido, dolor_punzante).
puede_evolucionar(zumbido, presion_oido).
puede_evolucionar(zumbido, perdida_audicion).

% NIVEL 2 -> NIVEL 3
puede_evolucionar(presion_oido, oido_tapado).
puede_evolucionar(presion_oido, dolor_punzante).
puede_evolucionar(perdida_audicion, oido_tapado).

% NIVEL 2.5 -> NIVEL 3
puede_evolucionar(resfriado, secrecion_nasal).

% NIVEL 3 -> NIVEL 4
puede_evolucionar(oido_tapado, secrecion).
puede_evolucionar(dolor_punzante, secrecion).
puede_evolucionar(secrecion_nasal, secrecion).

% NIVEL 4 -> DIAGNOSTICO FINAL
puede_evolucionar(secrecion, otitis).
```

### ¿Qué hace?

Define **aristas del grafo**: relaciones entre síntomas (quién puede evolucionar a quién).

### ¿Cómo lo hace?

Cada regla `puede_evolucionar(A, B)` significa: **"el síntoma A puede evolucionar hacia B"**

```prolog
puede_evolucionar(dolor_oido, presion_oido).
% Si tienes dolor de oído, puedes desarrollar presión en el oído

?- puede_evolucionar(dolor_oido, X).
% X = presion_oido ;   <- Primera vecino
% X = dolor_punzante.  <- Segundo vecino
```

#### Uso en búsqueda

```prolog
% En BFS/DFS: encontrar vecinos
findall(Vecino, puede_evolucionar(Actual, Vecino), Vecinos)
% Si Actual = presion_oido
% Vecinos = [oido_tapado, dolor_punzante]
```

### Código (Pesos)

```prolog
peso_sintoma(dolor_oido, 0.3).
peso_sintoma(zumbido, 0.2).
peso_sintoma(presion_oido, 0.4).
peso_sintoma(resfriado, 0.25).
peso_sintoma(perdida_audicion, 0.4).
peso_sintoma(oido_tapado, 0.5).
peso_sintoma(dolor_punzante, 0.7).
peso_sintoma(secrecion_nasal, 0.6).
peso_sintoma(secrecion, 0.9).
peso_sintoma(otitis, 1.0).
```

### ¿Qué hace?

Asigna **pesos (confianza/probabilidad)** a cada síntoma.

### ¿Cómo lo hace?

Cada peso representa **qué tan indicativo es el síntoma de otitis**:

```
Peso   Significado                    Síntomas
────────────────────────────────────────────────────────
0.2    Muy débil                      zumbido
0.3    Débil                          dolor_oido
0.4    Moderado                       presion_oido, perdida_audicion
0.5    Medio                          oido_tapado
0.6    Fuerte                         secrecion_nasal
0.7    Muy fuerte                     dolor_punzante
0.9    Crítico (casi seguro)          secrecion
1.0    Diagnóstico confirmado         otitis
```

#### Uso en evaluación

```prolog
% Para calcular probabilidad final
calcular_probabilidad_camino([dolor_oido, presion_oido, secrecion], Prob)
% Pesos: 0.3, 0.4, 0.9
% Promedio: (0.3 + 0.4 + 0.9) / 3 = 0.533...
```

---

## 🔍 Bloque 3: Algoritmo BFS

### Código

```prolog
% BFS para encontrar camino a OTITIS desde un sintoma inicial
bfs_otitis(SintomaInicial) :-
    retractall(visitado(_)),
    write('=== BUSQUEDA POR AMPLITUD (BFS) ==='), nl,
    write('Sintoma inicial: '), formatear_sintoma(SintomaInicial), nl, nl,
    write('--- EXPLORACION PASO A PASO ---'), nl, nl,
    bfs_cola([[SintomaInicial, [SintomaInicial]]], Camino, 1),
    !,
    nl,
    write('========================================'), nl,
    write('CAMINO ENCONTRADO A OTITIS'), nl,
    write('========================================'), nl,
    mostrar_camino(Camino), nl,
    evaluar_probabilidad(Camino).

% Caso base: llegamos a OTITIS
bfs_cola([[otitis, CaminoActual]|_], Camino, Paso) :-
    reverse(CaminoActual, Camino),
    format('Paso ~d: Llego a OTITIS!~n', [Paso]), nl.

% Caso recursivo: explorar vecinos nivel por nivel
bfs_cola([[Actual, CaminoActual]|RestoContenedor], CaminoFinal, Paso) :-
    Actual \= otitis,
    assertz(visitado(Actual)),
    
    % Mostrar paso actual
    format('Paso ~d: Explorando - ', [Paso]),
    formatear_sintoma(Actual), nl,
    
    % Mostrar cola actual
    write('  Cola actual: ['),
    extraer_sintomas_cola([[Actual, CaminoActual]|RestoContenedor], SintomasCola),
    mostrar_cola(SintomasCola),
    write(']'), nl,
    
    % Encontrar vecinos no visitados
    findall([Vecino, [Vecino|CaminoActual]], 
            (puede_evolucionar(Actual, Vecino), \+ visitado(Vecino)),
            NuevosNodos),
    
    % Mostrar vecinos encontrados
    (   NuevosNodos = [] ->
        write('  No hay vecinos nuevos'), nl
    ;   write('  Vecinos agregados: ['),
        extraer_sintomas_cola(NuevosNodos, Vecinos),
        mostrar_cola(Vecinos),
        write(']'), nl
    ),
    nl,
    
    agregar_al_final(RestoContenedor, NuevosNodos, NuevaContenedor),
    ProximoPaso is Paso + 1,
    bfs_cola(NuevaContenedor, CaminoFinal, ProximoPaso).
```

### ¿Qué hace?

Implementa **búsqueda en amplitud (BFS)** para encontrar el camino MÁS CORTO desde un síntoma inicial hasta OTITIS.

### ¿Cómo lo hace?

#### Pseudocódigo de BFS

```
BFS(inicio):
    cola = [[inicio, [inicio]]]      // [nodo_actual, camino_hasta_aqui]
    visitados = []
    paso = 1
    
    mientras cola NO esté vacía:
        extraer primer elemento: [Actual, CaminoActual]
        
        // Verificar si llegamos al destino
        si Actual == OTITIS:
            retornar CaminoActual (invertido)
        
        // No revisitar
        si Actual NO está visitado:
            marcar Actual como visitado
            encontrar vecinos de Actual
            
            para cada vecino:
                nuevo_elemento = [Vecino, [Vecino|CaminoActual]]
                agregar al FINAL de cola (FIFO)
            
            mostrar estado y continuar
```

#### Estructura de datos

```prolog
% Cola = [[nodo, camino], [nodo, camino], ...]
% Ejemplo: 
% Cola = [[presion_oido, [presion_oido, dolor_oido]],
%         [dolor_punzante, [dolor_punzante, dolor_oido]]]

% Nodo = síntoma actual
% Camino = lista de todos los nodos visitados hasta aquí
%         (en orden INVERSO: [actual, anterior, anterior_anterior, ..., inicio])
```

#### Desglose de cada parte

```prolog
% INICIALIZACION
bfs_otitis(SintomaInicial) :-
    retractall(visitado(_)),  % Limpiar visitados previos
    write('=== BUSQUEDA POR AMPLITUD (BFS) ==='), nl,
    % Iniciar cola con: [[SintomaInicial, [SintomaInicial]]]
    bfs_cola([[SintomaInicial, [SintomaInicial]]], Camino, 1),
    % ...
```

```prolog
% CASO BASE: Encontramos OTITIS
bfs_cola([[otitis, CaminoActual]|_], Camino, Paso) :-
    reverse(CaminoActual, Camino),  % Invertir camino a orden correcto
    format('Paso ~d: Llego a OTITIS!~n', [Paso]), nl.
    
% Ejemplo:
% CaminoActual = [otitis, secrecion, oido_tapado, presion_oido, dolor_oido]
% Camino (invertido) = [dolor_oido, presion_oido, oido_tapado, secrecion, otitis]
```

```prolog
% CASO RECURSIVO: No es OTITIS, explorar vecinos
bfs_cola([[Actual, CaminoActual]|RestoContenedor], CaminoFinal, Paso) :-
    Actual \= otitis,  % Solo si NO es OTITIS
    assertz(visitado(Actual)),  % Marcar como visitado
    
    % Encontrar vecinos
    findall([Vecino, [Vecino|CaminoActual]], 
            (puede_evolucionar(Actual, Vecino),     % Es vecino
             \+ visitado(Vecino)),                   % Y NO ha sido visitado
            NuevosNodos),
    
    % Agregar nuevos nodos al FINAL (FIFO de cola)
    agregar_al_final(RestoContenedor, NuevosNodos, NuevaContenedor),
    
    % Recursión con siguiente paso
    ProximoPaso is Paso + 1,
    bfs_cola(NuevaContenedor, CaminoFinal, ProximoPaso).
```

#### Ejemplo de ejecución paso a paso

```
Comando: ?- bfs_otitis(dolor_oido).

=== BUSQUEDA POR AMPLITUD (BFS) ===
Sintoma inicial: Dolor de Oído

--- EXPLORACION PASO A PASO ---

Paso 1: Explorando - Dolor de Oído
  Cola actual: [Dolor de Oído]
  Vecinos agregados: [Presión Oído, Dolor Punzante]

Paso 2: Explorando - Presión Oído
  Cola actual: [Dolor Punzante, Oído Tapado, Dolor Punzante]
  Vecinos agregados: [Oído Tapado]

Paso 3: Explorando - Dolor Punzante
  Cola actual: [Oído Tapado, Dolor Punzante, Secreción]
  Vecinos agregados: [Secreción]

Paso 4: Explorando - Oído Tapado
  Cola actual: [Dolor Punzante, Secreción, Secreción]
  Vecinos agregados: [Secreción]

Paso 5: Explorando - Secreción
  Cola actual: [Secreción, Otitis]
  Vecinos agregados: [Otitis]

Paso 6: Llego a OTITIS!

========================================
CAMINO ENCONTRADO A OTITIS
========================================
  -> Dolor de Oído
  -> Presión Oído
  -> Oído Tapado
  -> Secreción
  -> Otitis

=== EVALUACION DE RIESGO ===
Probabilidad de OTITIS: 53.30%
DIAGNOSTICO: OTITIS probable
RECOMENDACION: Consultar medico pronto
```

#### ¿Por qué BFS encuentra el camino MÁS CORTO?

```
BFS explora por NIVELES (layer by layer):

Nivel 0: [dolor_oido]
Nivel 1: [presion_oido, dolor_punzante]
Nivel 2: [oido_tapado, dolor_punzante, secrecion_nasal]
Nivel 3: [secrecion]
Nivel 4: [otitis]  ← ENCONTRADO en nivel 4

Si hubiera múltiples caminos:
- Camino A: 5 pasos (el más corto)
- Camino B: 7 pasos
BFS encontraría Camino A primero porque lo alcanza primero en amplitud.
```

---

## 🔍 Bloque 4: Algoritmo DFS

### Código

```prolog
% DFS para encontrar camino a OTITIS desde un sintoma inicial
dfs_otitis(SintomaInicial) :-
    retractall(visitado(_)),
    write('=== BUSQUEDA EN PROFUNDIDAD (DFS) ==='), nl,
    write('Sintoma inicial: '), formatear_sintoma(SintomaInicial), nl, nl,
    write('--- EXPLORACION PASO A PASO ---'), nl, nl,
    dfs_pila([[SintomaInicial, [SintomaInicial]]], Camino, 1),
    !,
    nl,
    write('========================================'), nl,
    write('CAMINO ENCONTRADO A OTITIS'), nl,
    write('========================================'), nl,
    mostrar_camino(Camino), nl,
    evaluar_probabilidad(Camino).

% Caso base: llegamos a OTITIS
dfs_pila([[otitis, CaminoActual]|_], Camino, Paso) :-
    reverse(CaminoActual, Camino),
    format('Paso ~d: Llego a OTITIS!~n', [Paso]), nl.

% Caso recursivo: explorar en profundidad (LIFO - pila)
dfs_pila([[Actual, CaminoActual]|RestoPila], CaminoFinal, Paso) :-
    Actual \= otitis,
    \+ visitado(Actual),
    assertz(visitado(Actual)),
    
    % Mostrar paso actual
    format('Paso ~d: Explorando - ', [Paso]),
    formatear_sintoma(Actual), nl,
    
    % Mostrar pila actual
    write('  Pila actual: ['),
    extraer_sintomas_cola([[Actual, CaminoActual]|RestoPila], SintomasPila),
    mostrar_cola(SintomasPila),
    write(']'), nl,
    
    % Encontrar vecinos no visitados
    findall([Vecino, [Vecino|CaminoActual]], 
            (puede_evolucionar(Actual, Vecino), \+ visitado(Vecino)),
            NuevosNodos),
    
    % Mostrar vecinos encontrados
    (   NuevosNodos = [] ->
        write('  No hay vecinos nuevos'), nl
    ;   write('  Vecinos (agregar al tope): ['),
        extraer_sintomas_cola(NuevosNodos, Vecinos),
        mostrar_cola(Vecinos),
        write(']'), nl
    ),
    nl,
    
    % Agregar nuevos nodos al TOPE de la pila (LIFO)
    append(NuevosNodos, RestoPila, NuevaPila),
    ProximoPaso is Paso + 1,
    dfs_pila(NuevaPila, CaminoFinal, ProximoPaso).

% Si el nodo ya fue visitado, saltar al siguiente en la pila
dfs_pila([[Actual, _]|RestoPila], CaminoFinal, Paso) :-
    visitado(Actual),
    dfs_pila(RestoPila, CaminoFinal, Paso).
```

### ¿Qué hace?

Implementa **búsqueda en profundidad (DFS)** para encontrar un camino (no necesariamente el más corto) desde un síntoma inicial hasta OTITIS.

### ¿Cómo lo hace?

#### Pseudocódigo de DFS

```
DFS(inicio):
    pila = [[inicio, [inicio]]]      // [nodo_actual, camino_hasta_aqui]
    visitados = []
    paso = 1
    
    mientras pila NO esté vacía:
        extraer elemento del TOPE: [Actual, CaminoActual]
        
        // Verificar si llegamos al destino
        si Actual == OTITIS:
            retornar CaminoActual (invertido)
        
        // No revisitar
        si Actual NO está visitado:
            marcar Actual como visitado
            encontrar vecinos de Actual
            
            para cada vecino:
                nuevo_elemento = [Vecino, [Vecino|CaminoActual]]
                agregar al TOPE de pila (LIFO)
            
            mostrar estado y continuar
```

#### Diferencia clave: LIFO vs FIFO

```prolog
% BFS usa append al FINAL (FIFO - cola)
agregar_al_final(RestoContenedor, NuevosNodos, NuevaContenedor)
% Resultado: [viejo1, viejo2, nuevo1, nuevo2]

% DFS usa append al TOPE (LIFO - pila)
append(NuevosNodos, RestoPila, NuevaPila)
% Resultado: [nuevo1, nuevo2, viejo1, viejo2]
```

#### Ejemplo de ejecución paso a paso

```
Comando: ?- dfs_otitis(dolor_oido).

=== BUSQUEDA EN PROFUNDIDAD (DFS) ===
Sintoma inicial: Dolor de Oído

--- EXPLORACION PASO A PASO ---

Paso 1: Explorando - Dolor de Oído
  Pila actual: [Dolor de Oído]
  Vecinos (agregar al tope): [Presión Oído, Dolor Punzante]

Paso 2: Explorando - Presión Oído
  Pila actual: [Presión Oído, Dolor Punzante]
  Vecinos (agregar al tope): [Oído Tapado, Dolor Punzante]

Paso 3: Explorando - Oído Tapado
  Pila actual: [Oído Tapado, Dolor Punzante, Dolor Punzante]
  Vecinos (agregar al tope): [Secreción]

Paso 4: Explorando - Secreción
  Pila actual: [Secreción, Dolor Punzante, Dolor Punzante]
  Vecinos (agregar al tope): [Otitis]

Paso 5: Llego a OTITIS!

========================================
CAMINO ENCONTRADO A OTITIS
========================================
  -> Dolor de Oído
  -> Presión Oído
  -> Oído Tapado
  -> Secreción
  -> Otitis

=== EVALUACION DE RIESGO ===
Probabilidad de OTITIS: 53.30%
DIAGNOSTICO: OTITIS probable
RECOMENDACION: Consultar medico pronto
```

#### ¿Por qué DFS NO garantiza el camino más corto?

```
DFS explora hacia PROFUNDIDAD (en lugar de amplitud):

Puede encontrar:
  [inicio, a, b, c, d, otitis]        <- 5 pasos
Cuando existe:
  [inicio, x, y, otitis]              <- 3 pasos (más corto)

Porque DFS explora completamente "a" antes de probar "x"
```

#### Comparación BFS vs DFS

| Característica | BFS | DFS |
|----------------|-----|-----|
| **Estructura** | Cola (FIFO) | Pila (LIFO) |
| **Camino** | MÁS CORTO | NO garantizado |
| **Memoria** | Mayor | Menor |
| **Orden exploración** | Por niveles | Por profundidad |
| **Primer nodo encontrado** | Sí es el más corto | No necesariamente |

---

## 📊 Bloque 5: Evaluación de Probabilidad

### Código

```prolog
evaluar_probabilidad(Camino) :-
    calcular_probabilidad_camino(Camino, Probabilidad),
    ProbPorcentaje is Probabilidad * 100,
    nl,
    write('=== EVALUACION DE RIESGO ==='), nl,
    format('Probabilidad de OTITIS: ~2f%~n', [ProbPorcentaje]),
    (   Probabilidad >= 0.7 ->
        write('DIAGNOSTICO: OTITIS confirmada'), nl,
        write('RECOMENDACION: Consultar medico URGENTE'), nl
    ;   Probabilidad >= 0.5 ->
        write('DIAGNOSTICO: OTITIS probable'), nl,
        write('RECOMENDACION: Consultar medico pronto'), nl
    ;   Probabilidad >= 0.3 ->
        write('DIAGNOSTICO: Riesgo moderado de OTITIS'), nl,
        write('RECOMENDACION: Monitorear sintomas'), nl
    ;   write('DIAGNOSTICO: Riesgo bajo de OTITIS'), nl,
        write('RECOMENDACION: Observacion'), nl
    ).

% Calcular probabilidad promedio basada en pesos
calcular_probabilidad_camino(Camino, Probabilidad) :-
    findall(Peso, (member(Sintoma, Camino), peso_sintoma(Sintoma, Peso)), Pesos),
    sum_list(Pesos, Total),
    length(Pesos, N),
    (   N > 0 ->
        Probabilidad is Total / N
    ;   Probabilidad is 0.0
    ).
```

### ¿Qué hace?

Calcula la **probabilidad de que el paciente tenga OTITIS** basándose en los síntomas encontrados y sus pesos.

### ¿Cómo lo hace?

#### Paso 1: Extraer pesos

```prolog
findall(Peso, (member(Sintoma, Camino), peso_sintoma(Sintoma, Peso)), Pesos)

% Ejemplo:
% Camino = [dolor_oido, presion_oido, oido_tapado, secrecion, otitis]
% Pesos extraídos:
%   - dolor_oido: 0.3
%   - presion_oido: 0.4
%   - oido_tapado: 0.5
%   - secrecion: 0.9
%   - otitis: 1.0
% Pesos = [0.3, 0.4, 0.5, 0.9, 1.0]
```

#### Paso 2: Sumar todos los pesos

```prolog
sum_list(Pesos, Total)
% sum_list([0.3, 0.4, 0.5, 0.9, 1.0], Total)
% Total = 3.1
```

#### Paso 3: Calcular promedio

```prolog
length(Pesos, N),  % N = 5
Probabilidad is Total / N  % Probabilidad = 3.1 / 5 = 0.62
```

#### Paso 4: Interpretar según rangos

```prolog
(   Probabilidad >= 0.7 ->
    write('OTITIS confirmada - URGENCIA MEDICA')
;   Probabilidad >= 0.5 ->
    write('OTITIS probable - consultar pronto')
;   Probabilidad >= 0.3 ->
    write('Riesgo moderado - monitorear')
;   write('Riesgo bajo - observacion')
).
```

#### Tabla de interpretación

| Probabilidad | Rango | Diagnóstico | Recomendación |
|--------------|-------|-------------|---------------|
| ≥ 0.7 | 70% - 100% | OTITIS confirmada | URGENCIA: Consultar médico AHORA |
| 0.5 - 0.69 | 50% - 69% | OTITIS probable | Consultar médico PRONTO |
| 0.3 - 0.49 | 30% - 49% | Riesgo moderado | Monitorear síntomas |
| < 0.3 | < 30% | Riesgo bajo | Observación |

#### Ejemplo completo

```
Camino encontrado: [dolor_oido, presion_oido, secrecion, otitis]

Extracción de pesos:
  dolor_oido    → 0.3
  presion_oido  → 0.4
  secrecion     → 0.9
  otitis        → 1.0
  ─────────────────────
  Suma: 2.6

Cálculo:
  Promedio = 2.6 / 4 = 0.65

Interpretación:
  Probabilidad: 65.00%
  DIAGNOSTICO: OTITIS probable
  RECOMENDACION: Consultar médico pronto
```

---

## 🎨 Bloque 6: Utilidades de Formateo

### Código: Formatear síntomas

```prolog
% Formatear nombre de sintoma
formatear_sintoma(Sintoma) :-
    atom_chars(Sintoma, Chars),
    reemplazar_guiones(Chars, CharsFormateados),
    capitalize_first(CharsFormateados, CharsCapitalizados),
    atom_chars(SintomaFormateado, CharsCapitalizados),
    write(SintomaFormateado).

reemplazar_guiones([], []).
reemplazar_guiones(['_'|T], [' '|RestoFormateado]) :-
    reemplazar_guiones(T, RestoFormateado).
reemplazar_guiones([H|T], [H|RestoFormateado]) :-
    H \= '_',
    reemplazar_guiones(T, RestoFormateado).

capitalize_first([], []).
capitalize_first([H|T], [HC|T]) :-
    char_type(H, lower),
    char_type(HC, upper),
    upcase_atom(H, HC), !.
capitalize_first([H|T], [H|T]).
```

### ¿Qué hace?

Convierte nombres de síntomas con guiones bajos (ej: `dolor_oido`) a formato legible (ej: `Dolor Oído`).

### ¿Cómo lo hace?

#### Paso 1: Convertir átomo a lista de caracteres

```prolog
atom_chars(dolor_oido, Chars)
% Chars = ['d', 'o', 'l', 'o', 'r', '_', 'o', 'i', 'd', 'o']
```

#### Paso 2: Reemplazar guiones bajos por espacios

```prolog
reemplazar_guiones(Chars, CharsFormateados)
% Entrada:  ['d', 'o', 'l', 'o', 'r', '_', 'o', 'i', 'd', 'o']
% Salida:   ['d', 'o', 'l', 'o', 'r', ' ', 'o', 'i', 'd', 'o']

% Cómo funciona (recursivamente):
% reemplazar_guiones(['d','o','l','o','r','_'|T], [?, ?, ?, ?, ?, ' '|RestoFormateado])
%   Caso base: '_' → ' '
%   Caso recursivo: 'd' → 'd', procesar resto
```

#### Paso 3: Capitalizar primera letra

```prolog
capitalize_first(CharsFormateados, CharsCapitalizados)
% Entrada:  ['d', 'o', 'l', 'o', 'r', ' ', 'o', 'i', 'd', 'o']
% Salida:   ['D', 'o', 'l', 'o', 'r', ' ', 'o', 'i', 'd', 'o']

% Solo cambia la primera letra si es minúscula
```

#### Paso 4: Convertir lista de caracteres de vuelta a átomo

```prolog
atom_chars(SintomaFormateado, CharsCapitalizados)
% Entrada:  ['D', 'o', 'l', 'o', 'r', ' ', 'o', 'i', 'd', 'o']
% Salida:   SintomaFormateado = 'Dolor oido'
```

#### Ejemplo completo

```
Entrada: dolor_oido
  ↓
Paso 1: ['d','o','l','o','r','_','o','i','d','o']
  ↓
Paso 2: ['d','o','l','o','r',' ','o','i','d','o']
  ↓
Paso 3: ['D','o','l','o','r',' ','o','i','d','o']
  ↓
Paso 4: "Dolor oido"
  ↓
Salida: Dolor oido
```

### Código: Mostrar camino

```prolog
% Mostrar camino formateado
mostrar_camino([]).
mostrar_camino([Sintoma]) :-
    write('  -> '), formatear_sintoma(Sintoma), nl.
mostrar_camino([Sintoma|Resto]) :-
    Resto \= [],
    write('  -> '), formatear_sintoma(Sintoma), nl,
    mostrar_camino(Resto).
```

### ¿Qué hace?

Imprime el camino completo de síntomas de forma legible con flechas.

### ¿Cómo lo hace?

```prolog
% Entrada: [dolor_oido, presion_oido, secrecion, otitis]

% Salida:
%   -> Dolor Oido
%   -> Presión Oido
%   -> Secreción
%   -> Otitis
```

Usa **recursión simple**:
- Caso base: lista vacía → no imprime nada
- Caso un elemento: imprime con flecha
- Caso múltiples: imprime primero y recursiona con el resto

---

## 💬 Bloque 7: Modo Interactivo

### Código: Inicio interactivo

```prolog
interactivo :-
    retractall(sintoma_presente(_)),
    retractall(visitado(_)),
    nl,
    write('========================================'), nl,
    write('  DIAGNOSTICO INTERACTIVO DE OTITIS'), nl,
    write('========================================'), nl, nl,
    write('SINTOMAS DISPONIBLES:'), nl, nl,
    write('NIVEL 1 - Sintomas Iniciales:'), nl,
    write('  1. dolor_oido       - Dolor de oido'), nl,
    write('  2. zumbido          - Zumbido en el oido'), nl, nl,
    % ... más opciones ...
    write('Seleccione el NUMERO del sintoma inicial (1-9): '),
    read(Numero),
    numero_a_sintoma(Numero, SintomaInicial),
    nl,
    write('Sintoma inicial seleccionado: '), 
    formatear_sintoma(SintomaInicial), nl,
    write('========================================'), nl, nl,
    write('El sistema preguntara por los sintomas siguiendo'), nl,
    write('la logica del grafo. Responda SI o NO.'), nl,
    write('(Escriba: si. o no. seguido de Enter)'), nl, nl,
    
    % Iniciar exploracion interactiva
    assertz(sintoma_presente(SintomaInicial)),
    explorar_interactivo([SintomaInicial]),
    
    % Evaluar diagnostico final
    evaluar_diagnostico_interactivo.
```

### ¿Qué hace?

Inicia un **diálogo SI/NO con el usuario** para determinar qué síntomas tiene.

### ¿Cómo lo hace?

#### Paso 1: Limpiar estado previo

```prolog
retractall(sintoma_presente(_)),  % Eliminar síntomas marcados previamente
retractall(visitado(_)),          % Eliminar nodos visitados
```

#### Paso 2: Mostrar menú y leer entrada

```prolog
write('Seleccione el NUMERO del sintoma inicial (1-9): '),
read(Numero),
numero_a_sintoma(Numero, SintomaInicial)

% Ejemplo: Usuario escribe "1."
% numero_a_sintoma(1, X) → X = dolor_oido
```

#### Paso 3: Marcar síntoma inicial

```prolog
assertz(sintoma_presente(SintomaInicial))

% Ahora: sintoma_presente(dolor_oido) = true
```

#### Paso 4: Explorar siguiendo el grafo

```prolog
explorar_interactivo([SintomaInicial])

% Esto preguntará por los vecinos de SintomaInicial,
% y recursivamente por sus vecinos, formando un árbol de preguntas
```

### Código: Exploración interactiva

```prolog
% Explorar interactivamente preguntando por sintomas siguientes
explorar_interactivo([]).
explorar_interactivo([SintomaActual|RestoSintomas]) :-
    % Obtener todos los síntomas que pueden seguir a SintomaActual
    findall(Siguiente, puede_evolucionar(SintomaActual, Siguiente), SiguientesPosibles),
    
    % Preguntar al usuario por cada uno
    preguntar_sintomas_interactivos(SiguientesPosibles, NuevosSintomas),
    
    % Continuar explorando desde los nuevos síntomas
    append(RestoSintomas, NuevosSintomas, TodosSintomas),
    explorar_interactivo(TodosSintomas).

% Preguntar por una lista de sintomas de forma interactiva
preguntar_sintomas_interactivos([], []).
preguntar_sintomas_interactivos([Sintoma|Resto], NuevosSintomas) :-
    (   sintoma_presente(Sintoma) ->
        % Ya fue marcado, no preguntar de nuevo
        preguntar_sintomas_interactivos(Resto, NuevosSintomas)
    ;   visitado(Sintoma) ->
        % Ya fue preguntado, no preguntar de nuevo
        preguntar_sintomas_interactivos(Resto, NuevosSintomas)
    ;   Sintoma = otitis ->
        % Si llegamos a OTITIS, marcar y detener
        assertz(sintoma_presente(otitis)),
        assertz(visitado(otitis)),
        NuevosSintomas = []
    ;   % Preguntar por este sintoma
        assertz(visitado(Sintoma)),
        crear_pregunta_sintoma(Sintoma, Pregunta),
        write('  '), write(Pregunta), write(' '),
        read(Respuesta),
        (   Respuesta = si ->
            assertz(sintoma_presente(Sintoma)),
            preguntar_sintomas_interactivos(Resto, RestoNuevos),
            NuevosSintomas = [Sintoma|RestoNuevos]
        ;   % Respuesta = no, continuar con el resto
            preguntar_sintomas_interactivos(Resto, NuevosSintomas)
        )
    ).
```

### ¿Qué hace?

Recorre el grafo interactivamente, preguntando al usuario por cada síntoma posible.

### ¿Cómo lo hace?

#### Paso 1: Obtener vecinos

```prolog
findall(Siguiente, puede_evolucionar(SintomaActual, Siguiente), SiguientesPosibles)

% Si SintomaActual = dolor_oido
% SiguientesPosibles = [presion_oido, dolor_punzante]
```

#### Paso 2: Preguntar por cada vecino

```prolog
% Para presion_oido:
crear_pregunta_sintoma(presion_oido, 'Siente presion en el oido?')
write('  Siente presion en el oido? ')
read(Respuesta)

% Si Respuesta = si → marcar como presente
% Si Respuesta = no → pasar al siguiente
```

#### Paso 3: Evitar preguntar dos veces

```prolog
(   sintoma_presente(Sintoma) ->
    % Ya está marcado, no preguntar
    ... 
;   visitado(Sintoma) ->
    % Ya fue preguntado, no preguntar
    ...
;   % Preguntar por primera vez
    ...
)
```

#### Ejemplo de sesión interactiva

```
?- interactivo.

========================================
  DIAGNOSTICO INTERACTIVO DE OTITIS
========================================

SINTOMAS DISPONIBLES:

NIVEL 1 - Sintomas Iniciales:
  1. dolor_oido       - Dolor de oido
  2. zumbido          - Zumbido en el oido

...

Seleccione el NUMERO del sintoma inicial (1-9): 1.

Sintoma inicial seleccionado: Dolor Oido
========================================

El sistema preguntara por los sintomas siguiendo
la logica del grafo. Responda SI o NO.
(Escriba: si. o no. seguido de Enter)

  Siente presion en el oido? si.
  Tiene oido tapado? si.
  Tiene secrecion en el oido? si.

========================================
RESUMEN DE SINTOMAS PRESENTES
========================================
  - Dolor Oido
  - Presión Oido
  - Oido Tapado
  - Secreción

========================================
DIAGNOSTICO: OTITIS CONFIRMADA
========================================
Camino de sintomas detectado:
  -> Dolor Oido
  -> Presión Oido
  -> Oido Tapado
  -> Secreción
  -> Otitis

Probabilidad: 62.00%

RECOMENDACION: Consultar medico URGENTE
```

---

## 🔄 Flujos de Ejecución Completos

### Flujo 1: Modo Automático con BFS

```
Usuario escribe: ?- iniciar.
        ↓
interactivo/0
        ↓
Mostrar menú de síntomas
        ↓
Usuario selecciona: 1 (dolor_oido)
        ↓
Usuario selecciona algoritmo: 1 (BFS)
        ↓
diagnosticar_otitis(dolor_oido, bfs)
        ↓
bfs_otitis(dolor_oido)
        ↓
bfs_cola([[dolor_oido, [dolor_oido]]], Camino, 1)
        ↓
┌─────────────────────────────────────────┐
│  ITERACIÓN 1: Explorar desde dolor_oido │
│  Vecinos: [presion_oido, dolor_punzante]│
│  Cola: [[presion_oido, ...], ...]       │
└─────────────────────────────────────────┘
        ↓
┌─────────────────────────────────────────┐
│  ITERACIÓN 2: Explorar desde presion_oido
│  Vecinos: [oido_tapado, ...]           │
│  Cola: [[dolor_punzante, ...], ...]    │
└─────────────────────────────────────────┘
        ↓
        ... más iteraciones ...
        ↓
┌─────────────────────────────────────────┐
│  ITERACIÓN N: Encontrado OTITIS        │
│  Camino = [dolor_oido, ..., otitis]    │
│  ÉXITO: retornar                        │
└─────────────────────────────────────────┘
        ↓
evaluar_probabilidad(Camino)
        ↓
calcular_probabilidad_camino(Camino, Prob)
        ↓
Mostrar resultado y diagnóstico
```

### Flujo 2: Modo Interactivo

```
Usuario escribe: ?- interactivo.
        ↓
Mostrar menú
        ↓
Usuario selecciona: 1 (dolor_oido)
        ↓
assertz(sintoma_presente(dolor_oido))
        ↓
explorar_interactivo([dolor_oido])
        ↓
findall vecinos de dolor_oido: [presion_oido, dolor_punzante]
        ↓
preguntar_sintomas_interactivos([presion_oido, dolor_punzante], Nuevos)
        ↓
┌──────────────────────────────────┐
│ Pregunta: "Siente presion...?"   │
│ Usuario: si.                     │
│ → assertz(sintoma_presente(...)) │
│ → Nuevos = [presion_oido|...]    │
└──────────────────────────────────┘
        ↓
┌──────────────────────────────────┐
│ Pregunta: "Tiene oido tapado...?"│
│ Usuario: no.                     │
│ → NO agregar a síntomas          │
└──────────────────────────────────┘
        ↓
explorar_interactivo(nuevos_sintomas)
        ↓
        ... más preguntas ...
        ↓
Llega a otitis
        ↓
evaluar_diagnostico_interactivo
        ↓
Mostrar resumen y diagnóstico
```

---

## 📝 Diagrama de Flujo General

```
┌─────────────────────────────────────────────────────────┐
│                    INICIO PROGRAMA                      │
└──────────────────────┬──────────────────────────────────┘
                       ↓
       ┌───────────────────────────────────┐
       │     Mostrar AYUDA al cargar       │
       │     :- ayuda.                     │
       └───────────────┬───────────────────┘
                       ↓
    ┌──────────────────────────────────────────┐
    │   Usuario elige modo de ejecución        │
    ├──────────────────────────────────────────┤
    │ ?- iniciar.     │ ?- interactivo.        │
    │ ?- diagnosticar │ ?- caso_xxx.           │
    │     _otitis()   │                        │
    └──┬──────────────────┬───────────────────┘
       ↓                  ↓
   ┌─────────────┐  ┌──────────────────────┐
   │  AUTOMATICO │  │   INTERACTIVO        │
   │  (BFS/DFS)  │  │   (SI/NO preguntas)  │
   └─────┬───────┘  └─────────┬────────────┘
         ↓                    ↓
    ┌──────────┐         ┌──────────────┐
    │ Seleccionar│      │ Seleccionar   │
    │ síntoma    │      │ síntoma inicial│
    │ inicial    │      │ + explorar     │
    └─────┬──────┘      │ interactivamente
          ↓             └────────┬───────┘
    ┌──────────┐                ↓
    │ BFS o DFS│           ┌──────────────┐
    │   búsqueda│          │ Preguntar    │
    └─────┬─────┘          │ por vecinos  │
          ↓                │ (SI/NO)      │
   ┌─────────────────┐     └────────┬─────┘
   │ Encontrar camino│            ↓
   │ a OTITIS o fallo│     ┌────────────────┐
   └──────┬──────────┘     │ Marcar síntomas│
          ↓                │ presentes/visi│
    ┌──────────────────┐   └────────┬───────┘
    │ calcular_probabi │           ↓
    │ lidad_camino()   │   ┌────────────────┐
    └──────┬───────────┘   │ Construir camino
          ↓                │ de diagnóstico  │
    ┌──────────────────┐   └────────┬───────┘
    │ Interpretar según│           ↓
    │ umbrales (0.3,   │   ┌────────────────┐
    │ 0.5, 0.7)       │   │ calcular_probabi
    └──────┬───────────┘   │ lidad_camino()  │
          ↓                └────────┬───────┘
    ┌──────────────────┐           ↓
    │ Mostrar resultado│   ┌────────────────┐
    │ y recomendación │   │ Interpretar     │
    └──────────────────┘   │ resultado final │
                           └────────┬───────┘
                                    ↓
                           ┌────────────────┐
                           │ Mostrar        │
                           │ diagnóstico    │
                           │ y reco mendación
                           └────────────────┘
```

---

## 🎓 Conclusión

Este código implementa un **sistema experto completo** con:

✅ **Estructura de datos** (grafo de síntomas con pesos)  
✅ **Algoritmos de búsqueda** (BFS y DFS)  
✅ **Evaluación probabilística** (promedio ponderado)  
✅ **Interfaces de usuario** (automática e interactiva)  
✅ **Utilidades** (formateo, visualización)  

Cada bloque cumple una función específica y **todos juntos** forman un **motor de diagnóstico funcional**.

La complejidad surge de la **combinación** de estas partes, pero cada una es comprensible si se estudia de forma aislada primero.

**Próximos pasos de aprendizaje:**
1. Ejecutar los casos de prueba en SWI-Prolog
2. Usar `trace` para ver paso a paso la ejecución
3. Modificar el grafo: añadir síntomas o aristas
4. Implementar variantes (ej: DFS iterativo)

