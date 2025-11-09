% ===========================================================================
% SISTEMA EXPERTO PARA DIAGNOSTICO DE OTITIS EN PROLOG
% Basado en el grafo de sintomas de agente_otitis.py
% ===========================================================================

% Limpiar la base de datos al inicio
:- dynamic(sintoma_presente/1).
:- dynamic(visitado/1).

% ===========================================================================
% DEFINICION DEL GRAFO DE SINTOMAS
% Basado exactamente en la estructura de agente_otitis.py
% ===========================================================================

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

% ===========================================================================
% REGLAS DE CONEXION DEL GRAFO (Relaciones entre sintomas)
% Exactamente como esta definido en agente_otitis.py
% ===========================================================================

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

% ===========================================================================
% PESOS DE IMPORTANCIA DE LOS SINTOMAS
% Basado en los pesos definidos en agente_otitis.py
% ===========================================================================

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

% ===========================================================================
% ALGORITMO BFS (Busqueda por Amplitud)
% ===========================================================================

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
    
    % Mostrar cola actual (solo los sintomas)
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

% ===========================================================================
% ALGORITMO DFS (Busqueda en Profundidad)
% ===========================================================================

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
    
    % Mostrar pila actual (solo los sintomas)
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

% ===========================================================================
% DIAGNOSTICO PRINCIPAL
% ===========================================================================

diagnosticar_otitis(SintomaInicial, Algoritmo) :-
    (   Algoritmo = bfs ->
        bfs_otitis(SintomaInicial)
    ;   Algoritmo = dfs ->
        dfs_otitis(SintomaInicial)
    ;   write('Error: Algoritmo debe ser "bfs" o "dfs"'), nl
    ).

% Diagnostico cuando no se encuentra camino a OTITIS
diagnosticar_otitis(SintomaInicial, Algoritmo) :-
    write('=== DIAGNOSTICO ==='), nl,
    write('Sintoma inicial: '), formatear_sintoma(SintomaInicial), nl,
    write('Algoritmo: '), write(Algoritmo), nl, nl,
    write('RESULTADO: Paciente SANO'), nl,
    write('No se encontro un camino que lleve a OTITIS'), nl,
    write('El sintoma puede ser leve o no relacionado con otitis.'), nl.

% ===========================================================================
% EVALUACION DE PROBABILIDAD
% ===========================================================================

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

% Crear pregunta apropiada para cada sintoma
crear_pregunta_sintoma(presion_oido, 'Siente presion en el oido?').
crear_pregunta_sintoma(perdida_audicion, 'Ha notado perdida de audicion?').
crear_pregunta_sintoma(oido_tapado, 'Siente el oido tapado?').
crear_pregunta_sintoma(dolor_punzante, 'Tiene dolor punzante en el oido?').
crear_pregunta_sintoma(secrecion_nasal, 'Tiene secrecion nasal?').
crear_pregunta_sintoma(secrecion, 'Tiene secrecion en el oido?').

% ===========================================================================
% UTILIDADES
% ===========================================================================

% Agregar elementos al final de una lista
agregar_al_final([], [], []).
agregar_al_final(Lista, [], Lista).
agregar_al_final([], [Elem|Resto], [Elem|RestoFinal]) :-
    agregar_al_final([], Resto, RestoFinal).
agregar_al_final([H|T], Nuevos, [H|RestoFinal]) :-
    agregar_al_final(T, Nuevos, RestoFinal).

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

% Mostrar camino formateado
mostrar_camino([]).
mostrar_camino([Sintoma]) :-
    write('  -> '), formatear_sintoma(Sintoma), nl.
mostrar_camino([Sintoma|Resto]) :-
    Resto \= [],
    write('  -> '), formatear_sintoma(Sintoma), nl,
    mostrar_camino(Resto).

% Mostrar cola/pila de forma compacta
mostrar_cola([]).
mostrar_cola([Elem]) :-
    formatear_sintoma(Elem).
mostrar_cola([Elem|Resto]) :-
    Resto \= [],
    formatear_sintoma(Elem),
    write(', '),
    mostrar_cola(Resto).

% Extraer sintomas de la estructura [Sintoma, Camino] para la cola/pila
extraer_sintomas_cola([], []).
extraer_sintomas_cola([[Sintoma, _]|Resto], [Sintoma|RestoSintomas]) :-
    extraer_sintomas_cola(Resto, RestoSintomas).

% ===========================================================================
% CASOS DE PRUEBA
% ===========================================================================

% Caso 1: Camino completo - Resfriado -> Otitis
caso_resfriado_bfs :-
    nl,
    write('========================================'), nl,
    write('CASO 1: Paciente con RESFRIADO (BFS)'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(resfriado, bfs).

caso_resfriado_dfs :-
    nl,
    write('========================================'), nl,
    write('CASO 1: Paciente con RESFRIADO (DFS)'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(resfriado, dfs).

% Caso 2: Dolor de oido
caso_dolor_oido_bfs :-
    nl,
    write('========================================'), nl,
    write('CASO 2: Paciente con DOLOR DE OIDO (BFS)'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(dolor_oido, bfs).

caso_dolor_oido_dfs :-
    nl,
    write('========================================'), nl,
    write('CASO 2: Paciente con DOLOR DE OIDO (DFS)'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(dolor_oido, dfs).

% Caso 3: Zumbido (sintoma leve)
caso_zumbido_bfs :-
    nl,
    write('========================================'), nl,
    write('CASO 3: Paciente con ZUMBIDO (BFS)'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(zumbido, bfs).

% Caso 4: Comparar BFS vs DFS con mismo sintoma
comparar_algoritmos(Sintoma) :-
    nl,
    write('========================================'), nl,
    write('COMPARACION BFS vs DFS'), nl,
    write('========================================'), nl, nl,
    diagnosticar_otitis(Sintoma, bfs),
    nl, nl,
    diagnosticar_otitis(Sintoma, dfs).

% ===========================================================================
% MODO 1: INICIAR - Escoger sintoma y recorrer automaticamente
% ===========================================================================

iniciar :-
    nl,
    write('========================================'), nl,
    write('  DIAGNOSTICO AUTOMATICO DE OTITIS'), nl,
    write('========================================'), nl, nl,
    write('SINTOMAS DISPONIBLES:'), nl, nl,
    
    write('NIVEL 1 - Sintomas Iniciales:'), nl,
    write('  1. dolor_oido       - Dolor de oido'), nl,
    write('  2. zumbido          - Zumbido en el oido'), nl, nl,
    
    write('NIVEL 2 - Sintomas Intermedios:'), nl,
    write('  3. presion_oido     - Presion en el oido'), nl,
    write('  4. perdida_audicion - Perdida de audicion'), nl, nl,
    
    write('NIVEL 2.5 - Condicion Respiratoria:'), nl,
    write('  5. resfriado        - Resfriado'), nl, nl,
    
    write('NIVEL 3 - Sintomas Graves:'), nl,
    write('  6. oido_tapado      - Oido tapado'), nl,
    write('  7. dolor_punzante   - Dolor punzante'), nl,
    write('  8. secrecion_nasal  - Secrecion nasal'), nl, nl,
    
    write('NIVEL 4 - Sintoma Critico:'), nl,
    write('  9. secrecion        - Secrecion en el oido'), nl, nl,
    
    write('========================================'), nl,
    write('Ingrese el NUMERO del sintoma inicial (1-9): '),
    read(Numero),
    numero_a_sintoma(Numero, Sintoma),
    nl,
    write('Sintoma seleccionado: '), formatear_sintoma(Sintoma), nl,
    write('========================================'), nl,
    write('Seleccione el algoritmo de busqueda:'), nl,
    write('  1. BFS (Busqueda por Amplitud)'), nl,
    write('  2. DFS (Busqueda en Profundidad)'), nl,
    write('Ingrese 1 o 2: '),
    read(NumAlgoritmo),
    numero_a_algoritmo(NumAlgoritmo, Algoritmo),
    nl,
    diagnosticar_otitis(Sintoma, Algoritmo).

% ===========================================================================
% MODO 2: INTERACTIVO - Preguntar SI/NO por cada sintoma siguiendo el grafo
% ===========================================================================

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
    
    write('NIVEL 2 - Sintomas Intermedios:'), nl,
    write('  3. presion_oido     - Presion en el oido'), nl,
    write('  4. perdida_audicion - Perdida de audicion'), nl, nl,
    
    write('NIVEL 2.5 - Condicion Respiratoria:'), nl,
    write('  5. resfriado        - Resfriado'), nl, nl,
    
    write('NIVEL 3 - Sintomas Graves:'), nl,
    write('  6. oido_tapado      - Oido tapado'), nl,
    write('  7. dolor_punzante   - Dolor punzante'), nl,
    write('  8. secrecion_nasal  - Secrecion nasal'), nl, nl,
    
    write('NIVEL 4 - Sintoma Critico:'), nl,
    write('  9. secrecion        - Secrecion en el oido'), nl, nl,
    
    write('========================================'), nl,
    write('Seleccione el NUMERO del sintoma inicial (1-9): '),
    read(Numero),
    numero_a_sintoma(Numero, SintomaInicial),
    nl,
    write('Sintoma inicial seleccionado: '), formatear_sintoma(SintomaInicial), nl,
    write('========================================'), nl, nl,
    write('El sistema preguntara por los sintomas siguiendo'), nl,
    write('la logica del grafo. Responda SI o NO.'), nl,
    write('(Escriba: si. o no. seguido de Enter)'), nl, nl,
    
    % Iniciar exploracion interactiva desde el sintoma seleccionado
    assertz(sintoma_presente(SintomaInicial)),
    explorar_interactivo([SintomaInicial]),
    
    % Evaluar diagnostico final
    evaluar_diagnostico_interactivo.

% Explorar interactivamente preguntando por sintomas siguientes
explorar_interactivo([]).
explorar_interactivo([SintomaActual|RestoSintomas]) :-
    findall(Siguiente, puede_evolucionar(SintomaActual, Siguiente), SiguientesPosibles),
    preguntar_sintomas_interactivos(SiguientesPosibles, NuevosSintomas),
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
        % Si llegamos a OTITIS, marcar y no preguntar
        assertz(sintoma_presente(otitis)),
        assertz(visitado(otitis)),
        NuevosSintomas = []  % Detener exploracion
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

% Evaluar diagnostico final en modo interactivo
evaluar_diagnostico_interactivo :-
    findall(S, sintoma_presente(S), TodosSintomas),
    nl,
    write('========================================'), nl,
    write('RESUMEN DE SINTOMAS PRESENTES'), nl,
    write('========================================'), nl,
    (   TodosSintomas = [] ->
        write('  Ningun sintoma presente'), nl
    ;   mostrar_lista_sintomas(TodosSintomas)
    ), nl,
    
    % Verificar si llego a OTITIS
    (   sintoma_presente(otitis) ->
        write('========================================'), nl,
        write('DIAGNOSTICO: OTITIS CONFIRMADA'), nl,
        write('========================================'), nl,
        construir_camino_diagnostico(TodosSintomas, Camino),
        calcular_probabilidad_camino(Camino, Prob),
        ProbPct is Prob * 100,
        format('Probabilidad: ~2f%~n', [ProbPct]), nl,
        write('RECOMENDACION: Consultar medico URGENTE'), nl
    ;   % No llego a OTITIS
        write('========================================'), nl,
        (   sintoma_presente(secrecion) ->
            write('DIAGNOSTICO: Riesgo MUY ALTO de OTITIS'), nl,
            write('========================================'), nl,
            write('Sintoma critico detectado (secrecion).'), nl,
            write('Muy proximo a desarrollar OTITIS.'), nl, nl,
            write('RECOMENDACION: Consultar medico URGENTE'), nl
        ;   (sintoma_presente(dolor_punzante); 
             sintoma_presente(secrecion_nasal); 
             sintoma_presente(oido_tapado)) ->
            write('DIAGNOSTICO: Riesgo ALTO de OTITIS'), nl,
            write('========================================'), nl,
            write('Sintomas graves detectados que pueden'), nl,
            write('evolucionar a OTITIS si no se tratan.'), nl, nl,
            write('RECOMENDACION: Consultar medico PRONTO'), nl
        ;   (sintoma_presente(presion_oido); 
             sintoma_presente(perdida_audicion)) ->
            write('DIAGNOSTICO: Riesgo MODERADO'), nl,
            write('========================================'), nl,
            write('Sintomas intermedios detectados.'), nl,
            write('RECOMENDACION: Monitorear sintomas'), nl
        ;   write('DIAGNOSTICO: Riesgo BAJO'), nl,
            write('========================================'), nl,
            write('Sintomas leves. Paciente mayormente SANO.'), nl,
            write('RECOMENDACION: Observacion'), nl
        )
    ).

% Construir camino de diagnostico
construir_camino_diagnostico(Sintomas, Camino) :-
    ordenar_por_niveles(Sintomas, CaminoOrdenado),
    CaminoOrdenado = Camino.

% Ordenar sintomas por niveles (del grafo)
ordenar_por_niveles(Sintomas, Ordenados) :-
    separarPorNiveles(Sintomas, N1, N2, N25, N3, N4, N5),
    append(N1, N2, Temp1),
    append(Temp1, N25, Temp2),
    append(Temp2, N3, Temp3),
    append(Temp3, N4, Temp4),
    append(Temp4, N5, Ordenados).

separarPorNiveles([], [], [], [], [], [], []).
separarPorNiveles([S|Resto], N1, N2, N25, N3, N4, N5) :-
    separarPorNiveles(Resto, N1R, N2R, N25R, N3R, N4R, N5R),
    (   sintoma_inicial(S) -> N1 = [S|N1R], N2 = N2R, N25 = N25R, N3 = N3R, N4 = N4R, N5 = N5R
    ;   sintoma_intermedio(S) -> N1 = N1R, N2 = [S|N2R], N25 = N25R, N3 = N3R, N4 = N4R, N5 = N5R
    ;   sintoma_intermedio_avanzado(S) -> N1 = N1R, N2 = N2R, N25 = [S|N25R], N3 = N3R, N4 = N4R, N5 = N5R
    ;   sintoma_grave(S) -> N1 = N1R, N2 = N2R, N25 = N25R, N3 = [S|N3R], N4 = N4R, N5 = N5R
    ;   sintoma_critico(S) -> N1 = N1R, N2 = N2R, N25 = N25R, N3 = N3R, N4 = [S|N4R], N5 = N5R
    ;   diagnostico_final(S) -> N1 = N1R, N2 = N2R, N25 = N25R, N3 = N3R, N4 = N4R, N5 = [S|N5R]
    ;   N1 = N1R, N2 = N2R, N25 = N25R, N3 = N3R, N4 = N4R, N5 = N5R
    ).

% Mostrar lista de sintomas formateados
mostrar_lista_sintomas([]).
mostrar_lista_sintomas([S|Resto]) :-
    write('  - '), formatear_sintoma(S), nl,
    mostrar_lista_sintomas(Resto).

% Mapeo de numeros a sintomas
numero_a_sintoma(1, dolor_oido).
numero_a_sintoma(2, zumbido).
numero_a_sintoma(3, presion_oido).
numero_a_sintoma(4, perdida_audicion).
numero_a_sintoma(5, resfriado).
numero_a_sintoma(6, oido_tapado).
numero_a_sintoma(7, dolor_punzante).
numero_a_sintoma(8, secrecion_nasal).
numero_a_sintoma(9, secrecion).
numero_a_sintoma(_, dolor_oido) :- 
    write('Numero invalido, usando "dolor_oido" por defecto'), nl.

% Mapeo de numeros a algoritmos
numero_a_algoritmo(1, bfs).
numero_a_algoritmo(2, dfs).
numero_a_algoritmo(_, bfs) :- 
    write('Opcion invalida, usando BFS por defecto'), nl.

% ===========================================================================
% AYUDA
% ===========================================================================

ayuda :-
    nl,
    write('========================================'), nl,
    write('  SISTEMA EXPERTO DE DIAGNOSTICO DE OTITIS'), nl,
    write('========================================'), nl, nl,
    write('COMANDOS PRINCIPALES:'), nl, nl,
    write('  iniciar.                    - Modo AUTOMATICO'), nl,
    write('                                Escoges UN sintoma inicial'), nl,
    write('                                y el sistema recorre hasta OTITIS'), nl, nl,
    write('  interactivo.                - Modo INTERACTIVO'), nl,
    write('                                Escoges sintoma inicial y el sistema'), nl,
    write('                                pregunta SI/NO por cada sintoma siguiendo'), nl,
    write('                                la logica del grafo'), nl, nl,
    write('OTROS COMANDOS:'), nl,
    write('  diagnosticar_otitis(S, A).  - Diagnostico directo'), nl,
    write('                                S = sintoma, A = bfs/dfs'), nl, nl,
    write('CASOS DE PRUEBA:'), nl,
    write('  caso_resfriado_bfs.         - Resfriado con BFS'), nl,
    write('  caso_dolor_oido_dfs.        - Dolor con DFS'), nl,
    write('  comparar_algoritmos(S).     - Comparar BFS vs DFS'), nl, nl,
    write('EJEMPLOS:'), nl,
    write('  ?- iniciar.                 <- Modo automatico (RECOMENDADO)'), nl,
    write('  ?- interactivo.             <- Modo pregunta SI/NO'), nl,
    write('  ?- diagnosticar_otitis(resfriado, bfs).'), nl, nl,
    write('========================================'), nl, nl.

% Mostrar ayuda al cargar
:- ayuda.
